package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
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

  private def gauge(text: String, city: String, scope: String): Option[Double] =
    PrometheusExposition.sample(text, WorkerSourceFilmsMetrics.Name, s"""city="$city",country="pl",scope="$scope"""")

  "The corpus census" should "count distinct ready films per city, by scope" in {
    val counts = reading(upcomingCorpus).served

    // Poznań: 2 films with a future showing (past-only drops out); 1 shows tomorrow.
    counts.getOrElse(("poznan", Scope.All), 0)      shouldBe 2
    counts.getOrElse(("poznan", Scope.Tomorrow), 0) shouldBe 1
    // Wrocław: 1 film, showing tomorrow.
    counts.getOrElse(("wroclaw", Scope.All), 0)      shouldBe 1
    counts.getOrElse(("wroclaw", Scope.Tomorrow), 0) shouldBe 1
  }

  it should "exclude a film whose TMDB enrichment hasn't concluded (not ready to project)" in {
    val counts = reading(Seq(pendingInPoznan)).served

    counts.getOrElse(("poznan", Scope.All), 0)      shouldBe 0
    counts.getOrElse(("poznan", Scope.Tomorrow), 0) shouldBe 0
  }

  "Its publish" should "publish the per-city counts onto the shared registry" in {
    val registry = new PrometheusRegistry()
    val census   = censusOver(cacheOver(repositoryOf(upcomingCorpus*)), registry)

    census.seed()
    census.publish()
    val text = PrometheusExposition.render(registry)

    gauge(text, "poznan", Scope.All)      shouldBe Some(2.0)
    gauge(text, "poznan", Scope.Tomorrow) shouldBe Some(1.0)
    gauge(text, "wroclaw", Scope.All)      shouldBe Some(1.0)
    gauge(text, "wroclaw", Scope.Tomorrow) shouldBe Some(1.0)
  }

  it should "seed every city at 0 before the first sample (a drop-to-zero is a sample, not an absence)" in {
    val registry = new PrometheusRegistry()
    censusOver(cacheOver(repositoryOf(upcomingCorpus*)), registry) // constructed, not yet published

    val text = PrometheusExposition.render(registry)
    gauge(text, "krakow", Scope.All)      shouldBe Some(0.0)
    gauge(text, "krakow", Scope.Tomorrow) shouldBe Some(0.0)
    gauge(text, "poznan", Scope.All)      shouldBe Some(0.0)
  }
}
