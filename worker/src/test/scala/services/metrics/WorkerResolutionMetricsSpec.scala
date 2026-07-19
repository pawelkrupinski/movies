package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.resolution.ResolutionOutcome

class WorkerResolutionMetricsSpec extends AnyFlatSpec with Matchers {

  private def render(r: PrometheusRegistry) = PrometheusExposition.render(r)

  "WorkerResolutionMetrics" should "seed every country × source × outcome at 0" in {
    val registry = new PrometheusRegistry()
    val _ = new WorkerResolutionMetrics(Seq("de", "pl"), registry)
    val text = render(registry)
    for (c <- Seq("de", "pl"); s <- WorkerResolutionMetrics.Sources; o <- ResolutionOutcome.all)
      text should include (s"""kinowo_worker_resolution_total{country="$c",outcome="$o",source="$s"} 0""")
  }

  it should "increment the right (country, source, outcome) cell via its bound recorder" in {
    val registry = new PrometheusRegistry()
    val metrics  = new WorkerResolutionMetrics(Seq("de", "pl"), registry)
    val plRt = metrics.recorderFor("pl", "rt")

    plRt.record(ResolutionOutcome.HitMemory)
    plRt.record(ResolutionOutcome.HitMemory)
    plRt.record(ResolutionOutcome.MissUnresolved)

    val text = render(registry)
    text should include ("""kinowo_worker_resolution_total{country="pl",outcome="hit_memory",source="rt"} 2""")
    text should include ("""kinowo_worker_resolution_total{country="pl",outcome="miss_unresolved",source="rt"} 1""")
    // Neither the other source nor the other country moved.
    text should include ("""kinowo_worker_resolution_total{country="pl",outcome="hit_memory",source="mc"} 0""")
    text should include ("""kinowo_worker_resolution_total{country="de",outcome="hit_memory",source="rt"} 0""")
  }

  // The seed grid is what keeps a Grafana line continuous from boot; a source
  // added to the wiring but not here would draw a line that pops in mid-chart.
  it should "seed every source the wiring builds a resolution cache for" in {
    WorkerResolutionMetrics.Sources should contain theSameElementsAs
      Seq("resolve_tmdb", "resolve_imdb", "resolve_rt", "resolve_mc", "resolve_filmweb").map(ResolutionOutcome.sourceOf)
  }
}
