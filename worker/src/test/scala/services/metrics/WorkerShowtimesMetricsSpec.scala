package services.metrics

import services.movies.SingleCountryNormalizer.titleNormalizer

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.Helios
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.CorpusMetricsFixtures._

/**
 * Locks the worker-side per-city `kinowo_worker_showtimes` gauge — the slot-volume
 * complement to `kinowo_worker_movies_served`. The load-bearing behaviour: it counts
 * INDIVIDUAL upcoming showtimes (not distinct films), drops past slots, and honours the
 * `readyToProject` gate; the per-city series sum to the general total. Records mirror
 * [[WorkerSourceFilmsMetricsSpec]] so the two gauges are visibly apples-to-apples — a
 * film shown in a city there is one film here becomes N slots.
 */
class WorkerShowtimesMetricsSpec extends AnyFlatSpec with Matchers {

  private def gauge(text: String, city: String): Option[Double] =
    PrometheusExposition.sample(text, WorkerShowtimesMetrics.Name, s"""city="$city",country="pl"""")

  "countAll" should "sum upcoming showtimes per city, dropping past slots" in {
    val counts = WorkerShowtimesMetrics.countAll(upcomingCorpus, models.City.all, clock, titleNormalizer)

    // Poznań: (today+tomorrow) 2 + (today) 1 = 3; the past-only slot drops out.
    counts.getOrElse("poznan", 0)  shouldBe 3
    // Wrocław: a single tomorrow slot.
    counts.getOrElse("wroclaw", 0) shouldBe 1
  }

  it should "count individual slots, not films (a film with two upcoming slots counts twice)" in {
    val counts = WorkerShowtimesMetrics.countAll(
      Seq(row("Double", ready(Helios, 9, today, tomorrow))), models.City.all, clock, titleNormalizer)
    counts.getOrElse("poznan", 0) shouldBe 2
  }

  it should "exclude a film whose TMDB enrichment hasn't concluded (not ready to project)" in {
    val counts = WorkerShowtimesMetrics.countAll(Seq(pendingInPoznan), models.City.all, clock, titleNormalizer)
    counts.getOrElse("poznan", 0) shouldBe 0
  }

  "sample" should "publish the per-city showtime counts onto the shared registry" in {
    val registry = new PrometheusRegistry()
    val metrics  = new WorkerShowtimesMetrics(WorkerShowtimesMetrics.gauge(registry), "pl", clock = clock, normalizer = services.movies.SingleCountryNormalizer.titleNormalizer)

    new WorkerCorpusScan(repositoryOf(upcomingCorpus*), Seq(metrics)).sample()
    val text = PrometheusExposition.render(registry)

    gauge(text, "poznan")  shouldBe Some(3.0)
    gauge(text, "wroclaw") shouldBe Some(1.0)
  }

  it should "seed every city at 0 before the first sample (a drop-to-zero is a sample, not an absence)" in {
    val registry = new PrometheusRegistry()
    new WorkerShowtimesMetrics(WorkerShowtimesMetrics.gauge(registry), "pl", clock = clock, normalizer = services.movies.SingleCountryNormalizer.titleNormalizer) // constructed, not yet sampled

    val text = PrometheusExposition.render(registry)
    gauge(text, "krakow") shouldBe Some(0.0)
    gauge(text, "poznan") shouldBe Some(0.0)
  }
}
