package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.HttpOutcome

class PaidEgressMetricsSpec extends AnyFlatSpec with Matchers {

  private def render(r: PrometheusRegistry) = PrometheusExposition.render(r)

  "PaidEgressMetrics" should "seed every country × provider × outcome at 0, so the failure-ratio alert has a denominator from boot" in {
    val registry = new PrometheusRegistry()
    val _ = new PaidEgressMetrics(Seq("pl", "uk"), registry)
    val text = render(registry)
    for (c <- Seq("pl", "uk"); p <- PaidEgressMetrics.Provider.all; o <- HttpOutcome.all)
      text should include (s"""kinowo_worker_paid_egress_total{country="$c",outcome="$o",provider="$p"} 0""")
  }

  it should "count each attempt on its own country and provider" in {
    val registry = new PrometheusRegistry()
    val metrics  = new PaidEgressMetrics(Seq("pl", "uk"), registry)
    val ukZyte   = metrics.recorderFor("uk", PaidEgressMetrics.Provider.Zyte)

    ukZyte.record(HttpOutcome.Http401)
    ukZyte.record(HttpOutcome.Http401)
    metrics.recorderFor("uk", PaidEgressMetrics.Provider.Decodo).record(HttpOutcome.Success)

    val text = render(registry)
    text should include ("""kinowo_worker_paid_egress_total{country="uk",outcome="http_401",provider="zyte"} 2""")
    text should include ("""kinowo_worker_paid_egress_total{country="uk",outcome="success",provider="decodo"} 1""")
    text should include ("""kinowo_worker_paid_egress_total{country="uk",outcome="http_401",provider="decodo"} 0""")
    text should include ("""kinowo_worker_paid_egress_total{country="pl",outcome="http_401",provider="zyte"} 0""")
  }
}
