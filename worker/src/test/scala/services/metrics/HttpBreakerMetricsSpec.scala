package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{HostCircuitBreakerHttpFetch, HttpFetch, HttpStatusException}

import java.time.Instant
import java.util.concurrent.atomic.AtomicReference

/** An open breaker was a log line only: a Decodo outage or a TMDB 5xx spell the breaker absorbed
 *  showed on no panel. These pin the two series a real breaker now feeds, by bounded leg. */
class HttpBreakerMetricsSpec extends AnyFlatSpec with Matchers {

  private val clock = new AtomicReference(Instant.parse("2026-10-04T10:00:00Z"))
  private val down: HttpFetch = new HttpFetch {
    def get(url: String): String = throw new HttpStatusException(503, "GET", url, None)
    def post(url: String, body: String, contentType: String): String = get(url)
  }
  private def breakerOn(metrics: HttpBreakerMetrics, leg: String) =
    new HostCircuitBreakerHttpFetch(down, failureThreshold = 2, now = () => clock.get(), report = Some(_ => ()),
      meter = metrics.meterFor("uk", leg))

  "the breaker metrics" should "count the hosts open now and each opening, on the breaker's leg only" in {
    val registry = new PrometheusRegistry()
    val metrics  = new HttpBreakerMetrics(Seq("pl", "uk"), registry)
    val decodo   = breakerOn(metrics, HttpBreakerMetrics.Leg.Decodo)
    val another  = breakerOn(metrics, HttpBreakerMetrics.Leg.Decodo)   // a second proxied chain, same leg

    (1 to 2).foreach(_ => intercept[Exception](decodo.get("https://a.example/x")))
    (1 to 2).foreach(_ => intercept[Exception](another.get("https://b.example/x")))

    val text = PrometheusExposition.render(registry)
    text should include ("""kinowo_worker_http_breaker_open_hosts{country="uk",leg="decodo"} 2""")
    text should include ("""kinowo_worker_http_breaker_open_hosts{country="uk",leg="scrape"} 0""")
    text should include ("""kinowo_worker_http_breaker_open_hosts{country="pl",leg="decodo"} 0""")
    text should include ("""kinowo_worker_http_breaker_opens_total{country="uk",leg="decodo"} 2""")
    text should include ("""kinowo_worker_http_breaker_opens_total{country="uk",leg="enrich"} 0""")

    clock.set(clock.get().plusSeconds(120))   // cooldown over: half-open, no longer open
    metrics.openHosts("uk", HttpBreakerMetrics.Leg.Decodo) shouldBe 0
  }
}
