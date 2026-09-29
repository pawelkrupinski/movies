package services.identity

import clients.TmdbClient
import org.bson.{BsonDocument, BsonString}
import play.api.Logging

import java.time.{Clock, LocalDate, ZoneOffset}
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
 * the model like any answer; an unchanged one writes nothing. Swept from the day before the last
 * COMPLETE sweep (TMDB's windows are UTC days; the overlap keeps an edit made during a sweep) to
 * today, in windows of at most 14 days (TMDB's limit); the watermark moves only once every window
 * has been read to its last page, so a failure repeats the window rather than skipping it.
 *
 * Brand-new films are the one edit a change list cannot tie to our questions (it names the film,
 * not the searches it would now answer): the fill's periodic re-asking of stored searches covers them.
 */
final class TmdbChangesSweep(store: TmdbStore, docs: TmdbDocuments, client: TmdbClient, language: String, clock: Clock)
    extends Logging {
  import TmdbChangesSweep._

  private val languages = Set(language.takeWhile(_ != '-'), "en")

  /** Whether the store has not yet been swept through today. */
  def behind: Boolean = watermark.forall(_.isBefore(today))

  /** Sweep what TMDB edited since the last complete sweep; the films and people fetched again. */
  def sweep(): SweepResult = synchronized {
    val end   = today
    val start = watermark.fold(end.minusDays(1))(_.minusDays(1))
    val windows = Iterator.iterate(start)(_.plusDays(MaxWindowDays)).takeWhile(!_.isAfter(end))
      .map(from => (from, Seq(from.plusDays(MaxWindowDays - 1), end).minBy(_.toEpochDay))).toSeq
    val results = windows.map { case (from, to) => sweepWindow(from, to) }
    docs.put(TmdbKind.Query, Seq(Watermark -> new BsonDocument("day", BsonString(end.toString))))
    val total = results.foldLeft(SweepResult(0, 0, 0))(_ + _)
    logger.info(s"identity store: TMDB changes $start..$end — ${total.changed} films edited, ${total.films} held films and " +
      s"${total.people} held people fetched again")
    total
  }

  private def sweepWindow(from: LocalDate, to: LocalDate): SweepResult = {
    val first            = client.changedMovies(from, to, 1)
    val changed          = (first._1 ++ (2 to first._2).flatMap(page => client.changedMovies(from, to, page)._1)).distinct
    val heldFilms        = changed.map(_.toString).grouped(500).flatMap(ids => store.get(TmdbKind.Film, ids).keySet).map(_.toInt).toSet
    var films            = Set.empty[Int]
    var people           = Set.empty[Int]
    changed.foreach { id =>
      val edits = try client.movieChanges(id, from, to) catch { case NonFatal(_) => Unreadable }
      if (heldFilms(id) && edits.exists(relevantToRecord)) films += id
      people ++= edits.flatMap(creditedPeople)
    }
    val heldPeople = people.map(_.toString).toSeq.grouped(500).flatMap(ids => store.get(TmdbKind.Person, ids).keySet).map(_.toInt).toSet
    films.foreach(client.identityRecord)
    heldPeople.foreach { id => client.personDirectorCredits(id); () }
    SweepResult(changed.size, films.size, heldPeople.size)
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

  private def watermark: Option[LocalDate] =
    docs.get(TmdbKind.Query, Seq(Watermark)).get(Watermark).flatMap(d => Option(d.get("day"))).map(v => LocalDate.parse(v.asString.getValue))
  private def today: LocalDate = LocalDate.ofInstant(clock.instant(), ZoneOffset.UTC)
}

object TmdbChangesSweep {
  final case class SweepResult(changed: Int, films: Int, people: Int) {
    def +(o: SweepResult): SweepResult = SweepResult(changed + o.changed, films + o.films, people + o.people)
  }

  val Watermark     = "meta|changes-swept"
  val MaxWindowDays = 14
  /** The record's fields, as TMDB names its edits (release dates move the year). */
  val RecordKeys = Set("title", "original_title", "release_date", "release_dates", "runtime", "alternative_titles",
    "production_countries", "origin_country", "imdb_id", "crew")
  val CreditDepartments = Set("Directing", "Writing")
  private val UnreadableKey = "?"
  private val Unreadable    = Seq(TmdbClient.MovieEdit(UnreadableKey, Set.empty, Set.empty))
}
