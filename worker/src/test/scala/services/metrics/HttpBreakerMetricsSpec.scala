package services.metrics

import clients.tools.{AnyMethodByUrlHttpFetch, FailingHttpFetch}
import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{HostCircuitBreakerHttpFetch, HttpFetch, HttpStatusException}

import java.time.Instant

/** An open breaker was a log line only: a Decodo outage or a TMDB 5xx spell the breaker absorbed
 *  showed on no panel. These pin the two series a real breaker now feeds, by bounded leg. */
class HttpBreakerMetricsSpec extends AnyFlatSpec with Matchers {

  private val clock = new tools.MutableClock(Instant.parse("2026-10-04T10:00:00Z"))
  private val down: HttpFetch = new FailingHttpFetch(503)
  private def breakerOn(metrics: HttpBreakerMetrics, leg: String) =
    new HostCircuitBreakerHttpFetch(down, failureThreshold = 2, clock = clock, report = Some(_ => ()),
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

    clock.advanceSeconds(120)   // cooldown over: half-open, no longer open
    metrics.openHosts("uk", HttpBreakerMetrics.Leg.Decodo) shouldBe 0
  }

  it should "name each unrecovered host, through the half-open gap, and drop it once it answers" in {
    val registry = new PrometheusRegistry()
    val metrics  = new HttpBreakerMetrics(Seq("pl"), registry)
    var healthy  = false
    val flaky: HttpFetch = new AnyMethodByUrlHttpFetch(url =>
      if (healthy) "ok" else throw new HttpStatusException(503, "GET", url, None))
    val breaker = new HostCircuitBreakerHttpFetch(flaky, failureThreshold = 2, clock = clock, report = Some(_ => ()),
      meter = metrics.meterFor("pl", HttpBreakerMetrics.Leg.Scrape))

    PrometheusExposition.render(registry) should not include "kinowo_worker_http_circuit_open{"

    (1 to 2).foreach(_ => intercept[Exception](breaker.get("https://restapi.helios.pl/x")))
    PrometheusExposition.render(registry) should include (
      """kinowo_worker_http_circuit_open{country="pl",host="restapi.helios.pl",leg="scrape"} 1""")

    clock.advanceSeconds(120)   // cooldown over, no call since: half-open, but not recovered
    PrometheusExposition.render(registry) should include ("""host="restapi.helios.pl"""")

    healthy = true
    breaker.get("https://restapi.helios.pl/x")
    PrometheusExposition.render(registry) should not include "kinowo_worker_http_circuit_open{"
  }
}
