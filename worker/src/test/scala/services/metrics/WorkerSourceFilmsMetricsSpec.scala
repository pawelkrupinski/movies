package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import models.{Helios, HeliosMagnolia, KinoApollo, MovieRecord, Rialto, Source, SourceData}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.CorpusMetricsFixtures._
import services.metrics.WorkerSourceFilmsMetrics.Scope

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

  // Same corpus as WebMovieMetricsSpec, with tmdbId set so the rows are ready.
  private val corpus = Seq(
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
    val counts = WorkerSourceFilmsMetrics.countAll(Seq(row("Pending", pending)), models.City.all, clock)

    counts.getOrElse(("poznan", Scope.All), 0)      shouldBe 0
    counts.getOrElse(("poznan", Scope.Tomorrow), 0) shouldBe 0
  }

  "sample" should "publish the per-city counts onto the shared registry" in {
    val registry = new PrometheusRegistry()
    val metrics  = new WorkerSourceFilmsMetrics(WorkerSourceFilmsMetrics.gauge(registry), "pl", clock = clock)

    new WorkerCorpusScan(repositoryOf(corpus*), Seq(metrics)).sample()
    val text = PrometheusExposition.render(registry)

    gauge(text, "poznan", Scope.All)      shouldBe Some(2.0)
    gauge(text, "poznan", Scope.Tomorrow) shouldBe Some(1.0)
    gauge(text, "wroclaw", Scope.All)      shouldBe Some(1.0)
    gauge(text, "wroclaw", Scope.Tomorrow) shouldBe Some(1.0)
  }

  it should "seed every city at 0 before the first sample (a drop-to-zero is a sample, not an absence)" in {
    val registry = new PrometheusRegistry()
    new WorkerSourceFilmsMetrics(WorkerSourceFilmsMetrics.gauge(registry), "pl", clock = clock) // constructed, not yet sampled

    val text = PrometheusExposition.render(registry)
    gauge(text, "krakow", Scope.All)      shouldBe Some(0.0)
    gauge(text, "krakow", Scope.Tomorrow) shouldBe Some(0.0)
    gauge(text, "poznan", Scope.All)      shouldBe Some(0.0)
  }
}
