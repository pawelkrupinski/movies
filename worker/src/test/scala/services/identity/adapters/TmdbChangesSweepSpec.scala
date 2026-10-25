package services.identity
package adapters

import clients.TmdbClient
import org.bson.{BsonDocument, BsonString}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{HttpFetch, HttpStatusException, MutableClock}

import java.nio.file.{Files, Paths}
import java.time.temporal.ChronoUnit
import java.time.{Duration, Instant, LocalDate}
import scala.collection.mutable
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/** TMDB's change lists keep the normalized store current: a held film whose edits touch its record,
 *  and a held person credited anew on any film, are fetched again — nothing else. The sweep is paced
 *  across each country's day from its own phase, resumes across restarts, and skips no day. Over
 *  TMDB's own recorded change answers, and a synthetic change list of known films a day. */
class TmdbChangesSweepSpec extends AnyFlatSpec with Matchers {
  import TmdbChangesSweepSpec._

  private def fixture(name: String) = new String(Files.readAllBytes(Paths.get(s"test/resources/fixtures/tmdb/$name")), "UTF-8")
  private val changeList = fixture("movie_changes_2026-09-28_page1.json")
  private val director   = 3214936   // credited Director on film 1782278 in its recorded edits
  private val producer   = 6509957   // credited Executive Producer there: not a credit the walk reads

  /** TMDB as recorded: the change list's first page (its other pages empty), each film's edits, and
   *  a record or credits for anything fetched again. */
  private final class Recorded(unreadable: Set[Int] = Set.empty) extends Tmdb {
    def answer(path: String, url: String): String =
      if (path == "/3/movie/changes") { if (url.contains("page=1&") || url.endsWith("page=1")) changeList else """{"results":[],"total_pages":57}""" }
      else if (path.endsWith("/changes")) path.split('/')(3).toInt match {
        case id if unreadable(id) => throw new IllegalStateException("unparseable")
        case 1782278              => fixture("movie_1782278_changes.json")
        case 1744659              => fixture("movie_1744659_changes_status_only.json")
        case _                    => """{"changes":[]}"""
      }
      else if (path.startsWith("/3/movie/")) """{"id":1,"title":"Nowy tytuł","credits":{"crew":[]},"alternative_titles":{"titles":[]}}"""
      else if (path.startsWith("/3/person/")) """{"crew":[{"id":1782278,"title":"A","department":"Directing","release_date":"2026-01-01"}]}"""
      else "{}"
  }

  private final class World(val tmdb: Tmdb, start: Instant = Instant.parse("2026-09-28T20:00:00Z"),
                            schedule: TmdbChangesSweep.Schedule = TmdbChangesSweep.Schedule(0.hours)) {
    val clock = new MutableClock(start)
    val docs  = new InMemoryTmdbDocuments
    val store = new TmdbStore(docs, clock)
    // What the store holds before the sweep: two films and two people.
    docs.put(TmdbKind.Film, Seq("1782278" -> new BsonDocument(), "1744659" -> new BsonDocument()))
    docs.put(TmdbKind.Person, Seq(director.toString -> new BsonDocument(), producer.toString -> new BsonDocument()))
    /** A worker's sweep over the store; a new one is a restart. */
    def sweep(): TmdbChangesSweep = new TmdbChangesSweep(store, docs, new NormalizingHttpFetch(tmdb, new TmdbNormalizer(store)),
      new TmdbClient(_, apiKey = Some(settings.TmdbApiKey("k"))), "pl-PL", schedule, clock)
    var current: TmdbChangesSweep = sweep()
    def swept(day: String): Unit =
      docs.put(TmdbKind.Query, Seq(TmdbChangesSweep.SweptThrough -> new BsonDocument("day", BsonString(day))))
    def sweptThrough: Option[LocalDate] = docs.get(TmdbKind.Query, Seq(TmdbChangesSweep.SweptThrough))
      .get(TmdbChangesSweep.SweptThrough).map(d => LocalDate.parse(d.getString("day").getValue))
    /** A tick a minute, as the schedule runs them, until `until`: when each ran and what it asked. */
    def tickUntil(until: String): Seq[(Instant, Int)] = {
      val ticks = mutable.Buffer.empty[(Instant, Int)]
      while (clock.instant().isBefore(Instant.parse(until))) {
        ticks += clock.instant() -> current.tick().asked
        clock.advance(Duration.ofMinutes(1))
      }
      ticks.toSeq
    }
  }

  private def refetched(tmdb: Tmdb) = tmdb.asked.map(u => new java.net.URI(u).getPath).filter(p => !p.endsWith("/changes")).toSet
  private def daysIn(from: String, until: String): Seq[LocalDate] =
    LocalDate.parse(from).datesUntil(LocalDate.parse(until)).iterator().asScala.toSeq

  "a sweep" should "fetch again the held film whose edits touch its record, and the held person credited anew — nothing else" in {
    val w = new World(new Recorded)
    w.tickUntil("2026-09-28T23:59:00Z")
    refetched(w.tmdb) shouldBe Set("/3/movie/1782278", s"/3/person/$director/movie_credits")   // 1744659's edit was only its status
    w.sweptThrough shouldBe Some(LocalDate.parse("2026-09-27"))
    w.docs.get(TmdbKind.Person, Seq(director.toString))(director.toString).getArray("directed").size shouldBe 1
  }

  it should "fetch again a held film whose edits cannot be read, rather than miss one" in {
    val w = new World(new Recorded(unreadable = Set(1744659)))
    w.tickUntil("2026-09-28T23:59:00Z")
    refetched(w.tmdb) should contain ("/3/movie/1744659")
  }

  it should "sweep every day since the last swept, in TMDB's windows of at most 14 days, none twice" in {
    val days = new Days(perDay = 10)
    val w    = new World(days, Instant.parse("2026-10-18T20:00:00Z"))
    w.swept("2026-09-27")
    w.tickUntil("2026-10-18T23:59:00Z")
    days.windows shouldBe Seq(("2026-09-28", "2026-10-11"), ("2026-10-12", "2026-10-17"))
    days.checked.sorted shouldBe daysIn("2026-09-28", "2026-10-18").flatMap(days.of)
  }

  it should "resume after the once-a-day sweep it replaces: that sweep's last day again, then on" in {
    val days = new Days(perDay = 10)
    val w    = new World(days)
    w.docs.put(TmdbKind.Query, Seq(TmdbChangesSweep.LegacyWatermark -> new BsonDocument("day", BsonString("2026-09-26"))))
    w.tickUntil("2026-09-28T23:59:00Z")
    days.windows shouldBe Seq(("2026-09-26", "2026-09-27"))
  }

  "a day's sweep" should "spread its films over the configured spread, a minute's allowance a tick — not one burst" in {
    val days     = new Days(perDay = 2400)
    val schedule = TmdbChangesSweep.Schedule(0.hours, spread = 20.hours)
    val w        = new World(days, Instant.parse("2026-09-28T00:00:00Z"), schedule)
    val ticks    = w.tickUntil("2026-09-29T00:00:00Z")
    schedule.perMinuteFor(2400) shouldBe 2
    ticks.map(_._2).max should be <= schedule.listingPerMinute          // the change list's 24 pages, two ticks
    ticks.drop(2).map(_._2).max shouldBe 2                              // then the films, two a minute
    val working = ticks.filter(_._2 > 0).map(_._1)
    working.max should (be > Instant.parse("2026-09-28T19:30:00Z") and be < Instant.parse("2026-09-28T21:00:00Z"))
    days.checked.sorted shouldBe days.of(LocalDate.parse("2026-09-27"))
    w.sweptThrough shouldBe Some(LocalDate.parse("2026-09-27"))
  }

  it should "start at each country's own phase of the day" in {
    val phases = Seq("pl", "de", "uk", "us", "es").map(c => c -> TmdbChangesSweep.Schedule.forCountry(c))
    phases.map(_._2.phase).distinct should have size 5
    phases.foreach { case (country, schedule) =>
      schedule.phase should (be >= 0.hours and be < 24.hours)
      val w = new World(new Days(perDay = 10), Instant.parse("2026-09-28T00:00:00Z"), schedule)
      w.swept("2026-09-26")
      val first = w.tickUntil("2026-09-29T00:00:00Z").find(_._2 > 0).map(_._1)
      withClue(country) {
        first shouldBe Some(Instant.parse("2026-09-28T00:00:00Z").plusMillis(schedule.phase.toMillis + 59999)
          .truncatedTo(ChronoUnit.MINUTES))
      }
    }
  }

  "a restart" should "resume at the next film, losing and repeating none, and skip no day across day boundaries" in {
    val days = new Days(perDay = 2400)
    val w    = new World(days, Instant.parse("2026-09-28T00:00:00Z"))
    // A restart every 7 hours for three days: mid-window and between windows alike.
    (1 to 10).foreach { _ =>
      w.tickUntil(w.clock.instant().plus(Duration.ofHours(7)).toString)
      w.current = w.sweep()
    }
    days.checked.size shouldBe days.checked.distinct.size
    days.checked.sorted shouldBe daysIn("2026-09-27", "2026-09-30").flatMap(days.of)
    days.windows shouldBe Seq(("2026-09-27", "2026-09-27"), ("2026-09-28", "2026-09-28"), ("2026-09-29", "2026-09-29"))
  }

  it should "keep a film TMDB refuses, and check it on a later tick" in {
    val refused = LocalDate.parse("2026-09-27").toEpochDay.toInt * 100000 + 5
    val days    = new Days(perDay = 100, refuse = Map(refused -> 3))   // every attempt of the client's first ask
    val w       = new World(days, Instant.parse("2026-09-28T00:00:00Z"))
    w.tickUntil("2026-09-28T03:00:00Z")
    days.checked.sorted shouldBe days.of(LocalDate.parse("2026-09-27"))
    w.sweptThrough shouldBe Some(LocalDate.parse("2026-09-27"))
  }

  "a catch-up after downtime" should "go at a paced rate for longer, never a burst" in {
    val days     = new Days(perDay = 6000)
    val schedule = TmdbChangesSweep.Schedule(0.hours)
    val w        = new World(days, Instant.parse("2026-10-08T00:00:00Z"), schedule)
    w.swept("2026-09-27")
    val ticks = w.tickUntil("2026-10-08T06:00:00Z")
    schedule.perMinuteFor(10 * 6000) shouldBe 50
    ticks.head._2 should be <= schedule.listingPerMinute                   // the first minute back: no more than any other
    ticks.map(_._2).max should be <= 50
    ticks.map(_._2).sum should be <= 6 * 60 * 50
    days.checked.size should be > 0
  }

  // TMDB overloaded or unreachable paused the minute's work with an info line, and the tick still completed: a sweep
  // TMDB refused for hours never went stale.
  it should "not complete a minute TMDB paused" in {
    @volatile var down = true
    val recorded = new Recorded
    val w = new World(new Tmdb {
      def answer(path: String, url: String): String =
        if (down) throw new tools.HttpStatusException(503, "GET", url, None) else recorded.answer(path, url)
    })
    w.current.tick().run shouldBe tools.TickRun.Incomplete
    down = false
    w.current.tick().run shouldBe tools.TickRun.Completed
  }
}

object TmdbChangesSweepSpec {
  /** A TMDB that keeps every request. */
  abstract class Tmdb extends HttpFetch {
    val asked = mutable.ArrayBuffer.empty[String]
    def answer(path: String, url: String): String
    def get(url: String): String = { asked += url; answer(new java.net.URI(url).getPath, url) }
    def post(url: String, body: String, contentType: String): String = get(url)
  }

  /** A change list of `perDay` known films a day (`epochDay·100000 + n`), 100 a page, no film's edits relevant;
   *  a film in `refuse` answers 429 to its first that-many asks. */
  final class Days(perDay: Int, refuse: Map[Int, Int] = Map.empty) extends Tmdb {
    val checked = mutable.ArrayBuffer.empty[Int]
    val windows = mutable.ArrayBuffer.empty[(String, String)]
    private val refusals = mutable.Map.empty[Int, Int]
    def of(day: LocalDate): Seq[Int] = (0 until perDay).map(day.toEpochDay.toInt * 100000 + _)
    private val Window = "start_date=([0-9-]+)&end_date=([0-9-]+)&page=([0-9]+)".r.unanchored
    def answer(path: String, url: String): String =
      if (path == "/3/movie/changes") url match {
        case Window(from, to, page) =>
          if (page == "1") windows += from -> to
          val films = LocalDate.parse(from).datesUntil(LocalDate.parse(to).plusDays(1)).iterator().asScala.toSeq.flatMap(of)
          val slice = films.slice((page.toInt - 1) * 100, page.toInt * 100)
          s"""{"results":[${slice.map(id => s"""{"id":$id}""").mkString(",")}],"total_pages":${((films.size + 99) / 100).max(1)}}"""
        case _ => throw new IllegalArgumentException(url)
      }
      else if (path.endsWith("/changes")) {
        val id = path.split('/')(3).toInt
        if (refusals.getOrElse(id, 0) < refuse.getOrElse(id, 0)) {
          refusals(id) = refusals.getOrElse(id, 0) + 1
          throw new HttpStatusException(429, "GET", url, None)
        }
        checked += id
        """{"changes":[]}"""
      }
      else "{}"
  }
}
