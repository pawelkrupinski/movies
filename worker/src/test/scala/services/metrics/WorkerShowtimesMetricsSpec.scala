package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{Helios, HeliosMagnolia, KinoApollo, MovieRecord, Rialto, Showtime, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.{MovieRepository, StoredMovieRecord}

import java.time.{Clock, LocalDateTime, ZoneId}

/**
 * Locks the worker-side per-city `kinowo_worker_showtimes` gauge — the slot-volume
 * complement to `kinowo_worker_movies_served`. The load-bearing behaviour: it counts
 * INDIVIDUAL upcoming showtimes (not distinct films), drops past slots, and honours the
 * `readyToProject` gate; the per-city series sum to the general total. Records mirror
 * [[WorkerSourceFilmsMetricsSpec]] so the two gauges are visibly apples-to-apples — a
 * film shown in a city there is one film here becomes N slots.
 */
class WorkerShowtimesMetricsSpec extends AnyFlatSpec with Matchers {

  private val warsaw = ZoneId.of("Europe/Warsaw")
  // Fixed "now": 2026-06-08 12:00 Warsaw.
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

  // Poznań: 3 upcoming slots (today+tomorrow=2, today=1), one past slot dropped.
  // Wrocław: 1 upcoming slot (tomorrow).
  private val corpus = repositoryOf(
    row("Today And Tomorrow", ready(Helios,         1, today, tomorrow)),
    row("Today Only",         ready(KinoApollo,     2, today)),
    row("Past Only",          ready(Rialto,         3, past)),
    row("Wroclaw Tomorrow",   ready(HeliosMagnolia, 4, tomorrow)),
  )

  private def gauge(text: String, city: String): Option[Double] =
    text.linesIterator
      .find(l => l.startsWith(s"${WorkerShowtimesMetrics.Name}{") && l.contains(s"""city="$city""""))
      .map(_.trim.split("\\s+").last.toDouble)

  "countAll" should "sum upcoming showtimes per city, dropping past slots" in {
    val counts = WorkerShowtimesMetrics.countAll(corpus, models.City.all, clock)

    // Poznań: (today+tomorrow) 2 + (today) 1 = 3; the past-only slot drops out.
    counts.getOrElse("poznan", 0)  shouldBe 3
    // Wrocław: a single tomorrow slot.
    counts.getOrElse("wroclaw", 0) shouldBe 1
  }

  it should "count individual slots, not films (a film with two upcoming slots counts twice)" in {
    val counts = WorkerShowtimesMetrics.countAll(
      repositoryOf(row("Double", ready(Helios, 9, today, tomorrow))), models.City.all, clock)
    counts.getOrElse("poznan", 0) shouldBe 2
  }

  it should "exclude a film whose TMDB enrichment hasn't concluded (not ready to project)" in {
    val pending = MovieRecord(data = Map[Source, SourceData](Helios -> slot(tomorrow))) // no tmdbId, no tmdbNoMatch
    pending.readyToProject shouldBe false
    val counts = WorkerShowtimesMetrics.countAll(repositoryOf(row("Pending", pending)), models.City.all, clock)
    counts.getOrElse("poznan", 0) shouldBe 0
  }

  "sample" should "publish the per-city showtime counts onto the shared registry" in {
    val registry = new PrometheusRegistry()
    val metrics  = new WorkerShowtimesMetrics(corpus, registry, clock = clock)

    metrics.sample()
    val text = PrometheusExposition.render(registry)

    gauge(text, "poznan")  shouldBe Some(3.0)
    gauge(text, "wroclaw") shouldBe Some(1.0)
  }

  it should "seed every city at 0 before the first sample (a drop-to-zero is a sample, not an absence)" in {
    val registry = new PrometheusRegistry()
    new WorkerShowtimesMetrics(repositoryOf(), registry, clock = clock) // constructed, not yet sampled

    val text = PrometheusExposition.render(registry)
    gauge(text, "krakow") shouldBe Some(0.0)
    gauge(text, "poznan") shouldBe Some(0.0)
  }
}
