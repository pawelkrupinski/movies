package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{Helios, HeliosMagnolia, KinoApollo, MovieRecord, Rialto, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.WorkerSourceFilmsMetrics.Scope
import services.movies.{MovieRepository, StoredMovieRecord}

import java.time.{Clock, LocalDateTime, ZoneId}

/**
 * Locks the worker-side per-city `kinowo_worker_movies_served` gauge — the
 * source-collection mirror of the web's `kinowo_web_movies_served`. The
 * load-bearing behaviour: the same `all` vs `tomorrow` scope split and future
 * filter the web applies, PLUS the `readyToProject` gate (a film still pending
 * TMDB enrichment is absent from the read model, so it must not inflate the source
 * count). The records mirror `WebMovieMetricsSpec` so the two gauges are visibly
 * apples-to-apples.
 */
class WorkerSourceFilmsMetricsSpec extends AnyFlatSpec with Matchers {

  private val warsaw = ZoneId.of("Europe/Warsaw")
  // Fixed "now": 2026-06-08 12:00 Warsaw → tomorrow is 2026-06-09.
  private val now    = LocalDateTime.of(2026, 6, 8, 12, 0)
  private val clock  = Clock.fixed(now.atZone(warsaw).toInstant, warsaw)

  private val today    = LocalDateTime.of(2026, 6, 8, 18, 0)
  private val tomorrow = LocalDateTime.of(2026, 6, 9, 18, 0)
  private val past     = LocalDateTime.of(2026, 6, 8, 9, 0) // before now − 30min, must drop out

  private def slot(times: LocalDateTime*): SourceData =
    SourceData(title = Some("x"), showtimes = times.map(t => Showtime(t, bookingUrl = None)))

  // tmdbId set → tmdbConcluded → readyToProject, matching what the projector writes.
  private def ready(cinema: Source, tmdb: Int, times: LocalDateTime*): MovieRecord =
    MovieRecord(tmdbId = Some(tmdb), data = Map(cinema -> slot(times*)))

  private def row(title: String, record: MovieRecord): StoredMovieRecord =
    StoredMovieRecord(title, Some(2026), record)

  private def repositoryOf(rows: StoredMovieRecord*): MovieRepository = new MovieRepository {
    def enabled = true
    def findAll() = rows
    def delete(t: String, y: Option[Int]) = ()
    def deleteById(id: String) = ()
    def upsert(t: String, y: Option[Int], e: MovieRecord) = ()
    def updateIfPresent(t: String, y: Option[Int], before: MovieRecord, after: MovieRecord) = false
    override def close() = ()
  }

  // Same corpus as WebMovieMetricsSpec, with tmdbId set so the rows are ready.
  private val corpus = repositoryOf(
    row("Today And Tomorrow", ready(Helios,         1, today, tomorrow)),
    row("Today Only",         ready(KinoApollo,     2, today)),
    row("Past Only",          ready(Rialto,         3, past)),
    row("Wroclaw Tomorrow",   ready(HeliosMagnolia, 4, tomorrow)),
  )

  private def gauge(text: String, city: String, scope: String): Option[Double] =
    text.linesIterator
      .find(l => l.startsWith(s"${WorkerSourceFilmsMetrics.Name}{") &&
                 l.contains(s"""city="$city"""") && l.contains(s"""scope="$scope""""))
      .map(_.trim.split("\\s+").last.toDouble)

  "countAll" should "count distinct ready films per city, by scope" in {
    val counts = WorkerSourceFilmsMetrics.countAll(corpus, models.City.all, clock)

    // Poznań: 2 films with a future showing (past-only drops out); 1 shows tomorrow.
    counts.getOrElse(("poznan", Scope.All), 0)      shouldBe 2
    counts.getOrElse(("poznan", Scope.Tomorrow), 0) shouldBe 1
    // Wrocław: 1 film, showing tomorrow.
    counts.getOrElse(("wroclaw", Scope.All), 0)      shouldBe 1
    counts.getOrElse(("wroclaw", Scope.Tomorrow), 0) shouldBe 1
  }

  it should "exclude a film whose TMDB enrichment hasn't concluded (not ready to project)" in {
    // A scraped-but-unresolved row plays tomorrow in Poznań, but the projector
    // holds it back, so the source gauge must not count it either.
    val pending = MovieRecord(data = Map[Source, SourceData](Helios -> slot(tomorrow))) // no tmdbId, no tmdbNoMatch
    pending.readyToProject shouldBe false
    val counts = WorkerSourceFilmsMetrics.countAll(repositoryOf(row("Pending", pending)), models.City.all, clock)

    counts.getOrElse(("poznan", Scope.All), 0)      shouldBe 0
    counts.getOrElse(("poznan", Scope.Tomorrow), 0) shouldBe 0
  }

  "sample" should "publish the per-city counts onto the shared registry" in {
    val registry = new PrometheusRegistry()
    val metrics  = new WorkerSourceFilmsMetrics(corpus, WorkerSourceFilmsMetrics.gauge(registry), "pl", clock = clock)

    metrics.sample()
    val text = PrometheusExposition.render(registry)

    gauge(text, "poznan", Scope.All)      shouldBe Some(2.0)
    gauge(text, "poznan", Scope.Tomorrow) shouldBe Some(1.0)
    gauge(text, "wroclaw", Scope.All)      shouldBe Some(1.0)
    gauge(text, "wroclaw", Scope.Tomorrow) shouldBe Some(1.0)
  }

  it should "seed every city at 0 before the first sample (a drop-to-zero is a sample, not an absence)" in {
    val registry = new PrometheusRegistry()
    new WorkerSourceFilmsMetrics(repositoryOf(), WorkerSourceFilmsMetrics.gauge(registry), "pl", clock = clock) // constructed, not yet sampled

    val text = PrometheusExposition.render(registry)
    gauge(text, "krakow", Scope.All)      shouldBe Some(0.0)
    gauge(text, "krakow", Scope.Tomorrow) shouldBe Some(0.0)
    gauge(text, "poznan", Scope.All)      shouldBe Some(0.0)
  }
}
