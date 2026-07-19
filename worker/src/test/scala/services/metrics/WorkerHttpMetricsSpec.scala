package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.HttpOutcome

class WorkerHttpMetricsSpec extends AnyFlatSpec with Matchers {

  private def render(r: PrometheusRegistry) = PrometheusExposition.render(r)

  "WorkerHttpMetrics" should "seed every country × phase × outcome at 0 so no Grafana line starts absent" in {
    val registry = new PrometheusRegistry()
    val _ = new WorkerHttpMetrics(Seq("de", "pl"), registry)
    val text = render(registry)
    // Every outcome exists for every country AND every phase from boot.
    HttpOutcome.all.foreach { o =>
      WorkerHttpMetrics.Phase.all.foreach { p =>
        text should include (s"""kinowo_worker_http_total{country="de",outcome="$o",phase="$p"} 0""")
        text should include (s"""kinowo_worker_http_total{country="pl",outcome="$o",phase="$p"} 0""")
      }
    }
  }

  it should "increment the right (country, phase, outcome) cell via its per-country recorder" in {
    val registry = new PrometheusRegistry()
    val metrics  = new WorkerHttpMetrics(Seq("de", "pl"), registry)
    val deScrape = metrics.recorderFor("de", WorkerHttpMetrics.Phase.Scrape)

    deScrape.record(HttpOutcome.Http429)
    deScrape.record(HttpOutcome.Http429)
    deScrape.record(HttpOutcome.Success)

    val text = render(registry)
    text should include ("""kinowo_worker_http_total{country="de",outcome="http_429",phase="scrape"} 2""")
    text should include ("""kinowo_worker_http_total{country="de",outcome="success",phase="scrape"} 1""")
    // pl untouched — the recorder is bound to its own country's slice.
    text should include ("""kinowo_worker_http_total{country="pl",outcome="http_429",phase="scrape"} 0""")
  }

  it should "keep scrape and enrich attempts on separate phase series" in {
    val registry = new PrometheusRegistry()
    val metrics  = new WorkerHttpMetrics(Seq("de"), registry)
    metrics.recorderFor("de", WorkerHttpMetrics.Phase.Scrape).record(HttpOutcome.Http403)
    metrics.recorderFor("de", WorkerHttpMetrics.Phase.Enrich).record(HttpOutcome.Http404)

    val text = render(registry)
    // The whole point of the phase label: a cinema-site 403 and a metadata-API 404
    // land on different series so a per-phase Grafana panel can read each alone.
    text should include ("""kinowo_worker_http_total{country="de",outcome="http_403",phase="scrape"} 1""")
    text should include ("""kinowo_worker_http_total{country="de",outcome="http_404",phase="enrich"} 1""")
    text should include ("""kinowo_worker_http_total{country="de",outcome="http_403",phase="enrich"} 0""")
    text should include ("""kinowo_worker_http_total{country="de",outcome="http_404",phase="scrape"} 0""")
  }
}
