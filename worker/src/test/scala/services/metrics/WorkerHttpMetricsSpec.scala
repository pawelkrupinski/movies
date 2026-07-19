package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.HttpOutcome

class WorkerHttpMetricsSpec extends AnyFlatSpec with Matchers {

  private def render(r: PrometheusRegistry) = PrometheusExposition.render(r)

  "WorkerHttpMetrics" should "seed every country × outcome at 0 so no Grafana line starts absent" in {
    val registry = new PrometheusRegistry()
    val _ = new WorkerHttpMetrics(Seq("de", "pl"), registry)
    val text = render(registry)
    // Every outcome exists for every country from boot.
    HttpOutcome.all.foreach { o =>
      text should include (s"""kinowo_worker_http_total{country="de",outcome="$o"} 0""")
      text should include (s"""kinowo_worker_http_total{country="pl",outcome="$o"} 0""")
    }
  }

  it should "increment the right (country, outcome) cell via its per-country recorder" in {
    val registry = new PrometheusRegistry()
    val metrics  = new WorkerHttpMetrics(Seq("de", "pl"), registry)
    val de = metrics.recorderFor("de")

    de.record(HttpOutcome.Http429)
    de.record(HttpOutcome.Http429)
    de.record(HttpOutcome.Success)

    val text = render(registry)
    text should include ("""kinowo_worker_http_total{country="de",outcome="http_429"} 2""")
    text should include ("""kinowo_worker_http_total{country="de",outcome="success"} 1""")
    // pl untouched — the recorder is bound to its own country's slice.
    text should include ("""kinowo_worker_http_total{country="pl",outcome="http_429"} 0""")
  }
}
