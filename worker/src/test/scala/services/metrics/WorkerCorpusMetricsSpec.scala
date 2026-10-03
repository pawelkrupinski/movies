package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.MovieRecord
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.CorpusMetricsFixtures.{clock, past, ready, repositoryOf, row, slot, tomorrow}
import org.scalatest.prop.TableDrivenPropertyChecks.*

import java.time.{Clock, LocalDateTime, ZoneId, ZoneOffset}
import services.metrics.WorkerCorpusMetrics.{CorpusCounts, Subset}

/**
 * Locks the corpus census the worker exposes for the Grafana "corpus coverage"
 * chart: total records, the any-rating / tmdb-id / imdb-id populations, and the
 * four per-source rating counts (imdb/rt/mc/fw) — all carried on one labelled
 * `kinowo_worker_corpus_movies{subset=…}` gauge.
 */
class WorkerCorpusMetricsSpec extends AnyFlatSpec with Matchers {

  // Distinct titles: the in-memory store keys rows by `sanitize(title)|year`, so
  // same-titled rows would collapse into one.
  private def rows(records: Seq[MovieRecord]) =
    records.zipWithIndex.map { case (r, i) => row(s"A Film $i", r) }

  private def render(registry: PrometheusRegistry): String = PrometheusExposition.render(registry)

  private def gauge(text: String, subset: String): Option[Double] =
    PrometheusExposition.sample(text, WorkerCorpusMetrics.Name, s"""country="pl",subset="$subset"""")

  // A mix exercising every subset: each record opts into a distinct combination.
  private val corpus = Seq(
    MovieRecord(tmdbId = Some(1), imdbId = Some("tt1"), imdbRating = Some(7.0)),
    MovieRecord(tmdbId = Some(2), rottenTomatoes = Some(90)),
    MovieRecord(tmdbId = Some(3), metascore = Some(80), filmwebRating = Some(8.1)),
    MovieRecord(imdbId = Some("tt4")),                       // id but no rating
    MovieRecord()                                            // bare: counts only toward total
  )

  // The population that was invisible: rows resolved to a film their own cinemas
  // contradict. Five sat in prod undetected until a hand-written scan found them,
  // so the point of the series is that nobody has to go looking again.
  "CorpusCounts" should "count rows whose cinemas contradict the film they resolved to" in {
    val misresolved = models.MovieRecord(
      tmdbId = Some(1667002),
      data = Map[models.Source, models.SourceData](
        models.Tmdb -> models.SourceData(title = Some("STABAT MATER RV621"), runtimeMinutes = Some(18)),
        models.KinoApollo -> models.SourceData(title = Some("Vivaldi i ja"), runtimeMinutes = Some(110))))
    val corroborated = models.MovieRecord(
      tmdbId = Some(1321666),
      data = Map[models.Source, models.SourceData](
        models.Tmdb -> models.SourceData(title = Some("Lalka"), runtimeMinutes = Some(162)),
        models.KinoApollo -> models.SourceData(title = Some("Lalka"), runtimeMinutes = Some(147))))

    val c = CorpusCounts.from(Seq(misresolved, corroborated), clock)
    c.bySubset.toMap.apply(Subset.Misresolved) shouldBe 1
    c.total shouldBe 2
  }

  "CorpusCounts" should "tally each subset independently" in {
    val c = CorpusCounts.from(corpus, clock)
    c.total         shouldBe 5
    c.withTmdbId    shouldBe 3
    c.withImdbId    shouldBe 2
    c.imdbRating    shouldBe 1
    c.rtRating      shouldBe 1
    c.mcRating      shouldBe 1
    c.fwRating      shouldBe 1
    c.withAnyRating shouldBe 3 // three records carry at least one of imdb/rt/mc/fw
  }

  // The OUTCOME half of `misresolved`, and the half that used to be silent: the sweep
  // rejects a wrong film, finds no right one, and leaves the row unresolved. It then
  // fails `readyToProject`, the projector prunes its card, and the film is invisible
  // on the site while its venues still sell tickets. Every other census gauge gates on
  // `readyToProject`, so these rows drop out of all of them without being counted
  // anywhere. Four needed hand repair on 2026-09-06 before this series existed.
  "CorpusCounts" should "count an unresolved row whose cinemas are still screening it" in {
    val invisible  = MovieRecord(data = Map[models.Source, models.SourceData](models.KinoApollo -> slot(tomorrow)))
    val resolved   = ready(models.KinoApollo, 1321666, tomorrow)
    // Unresolved too, but every showing has passed — legitimately gone, not invisible.
    val playedOut  = MovieRecord(data = Map[models.Source, models.SourceData](models.KinoApollo -> slot(past)))

    val c = CorpusCounts.from(Seq(invisible, resolved, playedOut), clock)
    c.bySubset.toMap.apply(Subset.UnresolvedWithShowtimes) shouldBe 1
    c.total shouldBe 3
  }

  // A showtime is the venue's wall clock. Judged in the pod's zone (UTC) a Los Angeles
  // row playing only tonight read as played out 7 hours early. Pinned to each zone's
  // next DST change, where a fixed offset would be wrong by an hour on top.
  private def cinemaIn(zone: String): models.Cinema =
    models.City.all.find(_.zoneId == ZoneId.of(zone)).flatMap(_.cinemas.headOption)
      .getOrElse(fail(s"no cinema in $zone"))

  // Showing counts while it started under Showtime.Grace (30 min) ago, in VENUE time.
  private val venueLocal = Table(
    ("zone",                "now (UTC instant)",    "showtime (venue-local)", "still screening"),
    // 2026-11-01 19:00 PST, after the fall-back: a UTC reading (03:00 next day) drops it,
    // a fixed PDT offset (20:00) would too.
    ("America/Los_Angeles", "2026-11-02T03:00:00Z", "2026-11-01T19:20",       true),
    ("America/Los_Angeles", "2026-11-02T03:00:00Z", "2026-11-01T18:20",       false),
    // 01:45 EST, inside the repeated hour: the 01:30 show began 15 min ago.
    ("America/New_York",    "2026-11-01T06:45:00Z", "2026-11-01T01:30",       true),
    // 03:00 CET just after Europe's fall-back; a CEST reading (04:00) would drop it.
    ("Europe/Warsaw",       "2026-10-25T02:00:00Z", "2026-10-25T02:40",       true),
    // 19:00 CET: began 40 min ago — a UTC reading (18:00) still counted it.
    ("Europe/Warsaw",       "2026-10-25T18:00:00Z", "2026-10-25T18:20",       false),
    // 19:00 BST the evening before the change; 18:00 GMT the evening after.
    ("Europe/London",       "2026-10-24T18:00:00Z", "2026-10-24T18:20",       false),
    ("Europe/London",       "2026-10-25T18:00:00Z", "2026-10-25T17:45",       true),
    // 22:00 CEST: began 40 min ago — a UTC reading (20:00) still counted it.
    ("Europe/Madrid",       "2026-10-24T20:00:00Z", "2026-10-24T21:20",       false),
  )

  "CorpusCounts" should "judge each slot's showtimes in its own venue's zone, across DST changes" in {
    forAll(venueLocal) { (zone, nowUtc, showtime, screening) =>
      val at      = Clock.fixed(java.time.Instant.parse(nowUtc), ZoneOffset.UTC)
      val record  = MovieRecord(data = Map[models.Source, models.SourceData](cinemaIn(zone) -> slot(LocalDateTime.parse(showtime))))
      withClue(s"$zone at $nowUtc, show $showtime: ") {
        WorkerCorpusMetrics.unresolvedYetScreening(record, at) shouldBe screening
      }
    }
  }

  it should "publish the unresolved-yet-screening series through a real scan" in {
    val registry = new PrometheusRegistry()
    val metrics  = new WorkerCorpusMetrics(WorkerCorpusMetrics.gauge(registry), "pl", clock)
    val invisible = MovieRecord(data = Map[models.Source, models.SourceData](models.KinoApollo -> slot(tomorrow)))

    new WorkerCorpusScan(repositoryOf(rows(Seq(invisible, ready(models.KinoApollo, 1, tomorrow)))*), Seq(metrics)).sample()

    gauge(render(registry), Subset.UnresolvedWithShowtimes) shouldBe Some(1.0)
  }

  "an empty corpus" should "count zero everywhere" in {
    CorpusCounts.from(Nil, clock) shouldBe CorpusCounts.empty
  }

  "WorkerCorpusMetrics.sample" should "publish every subset onto the shared registry" in {
    val registry = new PrometheusRegistry()
    val metrics  = new WorkerCorpusMetrics(WorkerCorpusMetrics.gauge(registry), "pl", clock)

    new WorkerCorpusScan(repositoryOf(rows(corpus)*), Seq(metrics)).sample()
    val text = render(registry)

    gauge(text, Subset.Total)         shouldBe Some(5.0)
    gauge(text, Subset.WithTmdbId)    shouldBe Some(3.0)
    gauge(text, Subset.WithImdbId)    shouldBe Some(2.0)
    gauge(text, Subset.WithAnyRating) shouldBe Some(3.0)
    gauge(text, Subset.ImdbRating)    shouldBe Some(1.0)
    gauge(text, Subset.RtRating)      shouldBe Some(1.0)
    gauge(text, Subset.McRating)      shouldBe Some(1.0)
    gauge(text, Subset.FwRating)      shouldBe Some(1.0)
  }

  // A 0 here is not "no data yet", it is "the corpus is empty": every worker restart on
  // 2026-10-03 drew each country's coverage lines to 0 and back for the ~5 minutes before
  // the first census, and read as the corpus swinging. Absent until the first complete
  // census is the truth — the same reason an incomplete pass publishes nothing.
  it should "publish no series before the first complete census" in {
    val registry = new PrometheusRegistry()
    new WorkerCorpusMetrics(WorkerCorpusMetrics.gauge(registry), "pl", clock) // constructed, not yet sampled
    val text = render(registry)

    Subset.all.foreach(s => gauge(text, s) shouldBe None)
  }
}
