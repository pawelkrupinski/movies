package services.identity
package adapters

import clients.TmdbClient
import org.bson.{BsonArray, BsonDocument, BsonInt32, BsonString}
import play.api.Logging
import services.cadence.DueBoundary
import tools.{ForwardingHttpFetch, HttpFetch}

import java.time.{Clock, LocalDate, ZoneOffset}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.control.NonFatal

/**
 * Keeps the normalized TMDB store current through TMDB's own change lists, so no edit to an answer
 * the model holds is lost and nothing unchanged is fetched again:
 *
 *  - a film we hold whose edits touch what the resolver reads — title, original title, release
 *    dates, runtime, directors, countries, alternative titles, IMDb id, or a translation in the
 *    deployment's language or English — has its record fetched again;
 *  - a person we hold who gained or lost a directing or writing credit on ANY edited film — a
 *    director's new film is an edit to that film, not to them — has their credits fetched again;
 *  - an edit that cannot be read is taken as relevant: fetched again rather than missed.
 *
 * Every re-fetch goes through the normalizing client, so a real change moves its document and wakes
 * the model like any answer; an unchanged one writes nothing.
 *
 * PACED ACROSS THE DAY (docs/design/identity-resolver.md §19.3). Each country's day starts at its own
 * hashed phase ([[TmdbChangesSweep.Schedule]]); from then the complete UTC days since the last one swept
 * (TMDB's windows are UTC days, at most 14 a query) are the next window. A [[tick]] (once a minute)
 * spends at most a minute's allowance: first on the window's change-list pages, then on its changed
 * films, one at a time, at the window's rate: its films spread over `Schedule.spread`, never above
 * `Schedule.maxPerMinute`. The window, its films and a cursor are persisted as they move, so a restart
 * resumes at the next film, neither losing nor repeating one; a request TMDB or the shared lookup chain
 * refuses (429, 5xx, open breaker, network) ends the tick with the cursor on that film. The watermark
 * moves only once the window's last film is done, so no day is skipped. A tick's allowance is one
 * minute's whatever came before it, so a catch-up after downtime is the same pace for longer, never a burst.
 *
 * Brand-new films are the one edit a change list cannot tie to our questions (it names the film,
 * not the searches it would now answer): the fill's periodic re-asking of stored searches covers them.
 */
final class TmdbChangesSweep(store: TmdbStore, docs: TmdbDocuments, fetch: HttpFetch, tmdb: HttpFetch => TmdbClient,
                             language: String, schedule: TmdbChangesSweep.Schedule, clock: Clock)
    extends Logging {
  import TmdbChangesSweep._

  private val languages = Set(language.takeWhile(_ != '-'), "en")

  /** One minute's worth of the sweep: how many requests it made, and the window (from, to) it worked on. */
  def tick(): Tick = synchronized(tools.HttpPath.within(tools.HttpPath.Sweep) {
    val counted = new CountingFetch(fetch)
    val client  = tmdb(counted)
    val window  = pending.orElse(nextWindow)
    try window.foreach(w => if (w.listed) check(w, client, counted) else list(w, client, counted))
    catch {
      case e if NonFatal(e) && LookupFillLiveFetch.isOverload(e) =>
        logger.info(s"identity store: TMDB changes sweep paused this minute (${e.getClass.getSimpleName})")
    }
    Tick(counted.asked, window.map(w => w.from -> w.to))
  })

  /** Reads the window's change-list pages, one request each, persisting each; read to the last, it
   *  fixes the window's rate: its films over the spread. */
  private def list(window: Window, client: TmdbClient, counted: CountingFetch): Unit = {
    var w = window
    while (!w.listed && counted.asked < schedule.listingPerMinute) {
      val (ids, pages) = client.changedMovies(w.from, w.to, w.pagesRead + 1)
      w = w.copy(ids = w.ids ++ ids, pagesRead = w.pagesRead + 1, pages = pages.max(1))
      if (w.listed) {
        val films = w.ids.distinct
        w = w.copy(ids = films, perMinute = schedule.perMinuteFor(films.size))
        logger.info(s"identity store: TMDB changes ${w.from}..${w.to} — ${films.size} films edited, checked at ${w.perMinute}/min")
      }
      docs.put(TmdbKind.Query, Seq(WindowKey -> w.document))
    }
  }

  /** Checks the window's films from its cursor while the minute's allowance lasts, saving the cursor
   *  after each; the last one done, moves the watermark to the window's end. */
  private def check(window: Window, client: TmdbClient, counted: CountingFetch): Unit = {
    var cursor = cursorOf(window)
    val slice  = window.ids.slice(cursor.next, cursor.next + window.perMinute)
    val held   = store.get(TmdbKind.Film, slice.map(_.toString)).keySet
    slice.iterator.takeWhile(_ => counted.asked < window.perMinute).foreach { id =>
      val edits  = try client.movieChanges(id, window.from, window.to)
        catch { case e if NonFatal(e) && !LookupFillLiveFetch.isOverload(e) => Unreadable }
      val film   = held(id.toString) && edits.exists(relevantToRecord)
      if (film) client.identityRecord(id)
      val people = store.get(TmdbKind.Person, edits.flatMap(creditedPeople).distinct.map(_.toString)).keySet
      people.foreach { p => client.personDirectorCredits(p.toInt); () }
      cursor = cursor.copy(next = cursor.next + 1, films = cursor.films + (if (film) 1 else 0), people = cursor.people + people.size)
      docs.put(TmdbKind.Query, Seq(CursorKey -> cursor.document))
    }
    if (cursor.next >= window.ids.size) {
      docs.put(TmdbKind.Query, Seq(SweptThrough -> new BsonDocument("day", BsonString(window.to.toString))))
      logger.info(s"identity store: TMDB changes ${window.from}..${window.to} swept — ${window.ids.size} films edited, " +
        s"${cursor.films} held films and ${cursor.people} held people fetched again")
    }
  }

  private def relevantToRecord(edit: TmdbClient.MovieEdit): Boolean = edit.key match {
    case UnreadableKey  => true
    case "translations" => edit.languages.isEmpty || edit.languages.exists(languages)
    case "crew"         => edit.jobs.isEmpty || edit.jobs.exists(TmdbFilmRecord.DirectorJobs)
    case key            => RecordKeys(key)
  }

  /** The people an edit gave or took a directing or writing credit — an unreadable one names no one. */
  private def creditedPeople(edit: TmdbClient.MovieEdit): Seq[Int] =
    if (edit.key == "crew") edit.credits.collect { case (person, department) if CreditDepartments(department) => person } else Nil

  /** The last UTC day this country counts complete: the day before its phase-shifted today. */
  private def lastCompleteDay: LocalDate =
    LocalDate.ofInstant(clock.instant().minusMillis(schedule.phase.toMillis), ZoneOffset.UTC).minusDays(1)

  /** The last day swept to its end. The once-a-day sweep this replaced kept the day it ran on, which it
   *  had swept only to that moment: that day is swept again. */
  private def sweptThrough: Option[LocalDate] = day(SweptThrough).orElse(day(LegacyWatermark).map(_.minusDays(1)))

  /** The days after the last swept, through the last complete one, at most TMDB's 14. */
  private def nextWindow: Option[Window] = {
    val end  = lastCompleteDay
    val from = sweptThrough.fold(end)(_.plusDays(1))
    Option.when(!from.isAfter(end))(Window(from, Seq(from.plusDays(MaxWindowDays - 1), end).minBy(_.toEpochDay)))
  }

  /** The window in progress: the persisted one, unless the watermark already covers it. */
  private def pending: Option[Window] = docs.get(TmdbKind.Query, Seq(WindowKey)).get(WindowKey).map(Window.from)
    .filter(w => sweptThrough.forall(_.isBefore(w.to)))

  private def cursorOf(w: Window): Cursor = docs.get(TmdbKind.Query, Seq(CursorKey)).get(CursorKey).map(Cursor.from)
    .filter(c => c.from == w.from && c.to == w.to).getOrElse(Cursor(w.from, w.to, 0, 0, 0))

  private def day(key: String): Option[LocalDate] =
    docs.get(TmdbKind.Query, Seq(key)).get(key).flatMap(d => Option(d.get("day"))).map(v => LocalDate.parse(v.asString.getValue))
}

object TmdbChangesSweep {
  /** What one tick asked TMDB, and the window (from, to) it worked on. */
  final case class Tick(asked: Int, window: Option[(LocalDate, LocalDate)])

  /**
   * When and how fast a country sweeps. Its day starts `phase` past midnight UTC; a window's films are
   * checked over `spread` (under a day, so a day's window is done before the next opens), at between
   * `minPerMinute` and `maxPerMinute` requests a minute; its change-list pages at `listingPerMinute`.
   */
  final case class Schedule(phase: FiniteDuration, spread: FiniteDuration = 20.hours, minPerMinute: Int = 2,
                            maxPerMinute: Int = 60, listingPerMinute: Int = 20) {
    /** A window's rate: its films (a request each, and a re-fetch for the few relevant) over the spread. */
    def perMinuteFor(films: Int): Int =
      math.ceil(films.toDouble / spread.toMinutes).toInt.max(minPerMinute).min(maxPerMinute)
  }

  object Schedule {
    /** The country's schedule, its phase hashed from its code into the day: stable across restarts, no storage. */
    def forCountry(code: String): Schedule =
      Schedule(DueBoundary.hashedPhaseMillis(s"tmdb-changes-sweep|$code", 1.day.toMillis).millis)
  }

  /** How often the sweep ticks: its allowances are a minute's. */
  val TickInterval: FiniteDuration = 1.minute

  /** A window in progress: its days, the films its change list named, the pages read of how many, and its rate. */
  private final case class Window(from: LocalDate, to: LocalDate, ids: Vector[Int] = Vector.empty, pagesRead: Int = 0,
                                  pages: Int = 0, perMinute: Int = 0) {
    def listed: Boolean = pages > 0 && pagesRead >= pages
    def document: BsonDocument = new BsonDocument("from", BsonString(from.toString)).append("to", BsonString(to.toString))
      .append("ids", new BsonArray(ids.map(i => new BsonInt32(i): org.bson.BsonValue).asJava))
      .append("pagesRead", new BsonInt32(pagesRead)).append("pages", new BsonInt32(pages)).append("perMinute", new BsonInt32(perMinute))
  }
  private object Window {
    def from(d: BsonDocument): Window = Window(LocalDate.parse(d.getString("from").getValue), LocalDate.parse(d.getString("to").getValue),
      d.getArray("ids").asScala.map(_.asInt32.getValue).toVector, d.getInt32("pagesRead").getValue, d.getInt32("pages").getValue,
      d.getInt32("perMinute").getValue)
  }

  /** The next film of a window to check, and what checking it so far fetched again. */
  private final case class Cursor(from: LocalDate, to: LocalDate, next: Int, films: Int, people: Int) {
    def document: BsonDocument = new BsonDocument("from", BsonString(from.toString)).append("to", BsonString(to.toString))
      .append("next", new BsonInt32(next)).append("films", new BsonInt32(films)).append("people", new BsonInt32(people))
  }
  private object Cursor {
    def from(d: BsonDocument): Cursor = Cursor(LocalDate.parse(d.getString("from").getValue), LocalDate.parse(d.getString("to").getValue),
      d.getInt32("next").getValue, d.getInt32("films").getValue, d.getInt32("people").getValue)
  }

  /** Counts a tick's requests, retries included: what its allowance is spent in. */
  private final class CountingFetch(underlying: HttpFetch) extends ForwardingHttpFetch {
    var asked = 0
    protected def around[A](url: String)(call: HttpFetch => A): A = { asked += 1; call(underlying) }
  }

  /** The last day swept to its end. */
  val SweptThrough    = "meta|changes-swept-through"
  /** The day the once-a-day sweep last ran on: read once, to resume after it. */
  val LegacyWatermark = "meta|changes-swept"
  val WindowKey       = "meta|changes-sweep-window"
  val CursorKey       = "meta|changes-sweep-cursor"
  val MaxWindowDays   = 14
  /** The record's fields, as TMDB names its edits (release dates move the year). */
  val RecordKeys = Set("title", "original_title", "release_date", "release_dates", "runtime", "alternative_titles",
    "production_countries", "origin_country", "imdb_id", "crew")
  val CreditDepartments = Set("Directing", "Writing")
  private val UnreadableKey = "?"
  private val Unreadable    = Seq(TmdbClient.MovieEdit(UnreadableKey, Set.empty, Set.empty))
}
