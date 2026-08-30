package modules

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.libs.streams.Accumulator
import play.api.mvc.{EssentialAction, RequestHeader, Result, Results}
import play.api.routing.{HandlerDef, Router}
import play.api.test.FakeRequest
import services.metrics.{PrometheusExposition, WebHttpMetrics}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
 * Locks the request metrics the web tier publishes on its own `/metrics`
 * (`kinowo_web_http_requests_total`, `kinowo_web_http_request_duration_seconds`)
 * — the replacement for the dead `fly_app_http_*` proxy series.
 *
 * The load-bearing assertion is the CARDINALITY one: a per-film request must
 * label with the route PATTERN (`/:city/movie/:slug`), never the raw URI. Getting
 * that wrong doesn't fail anything visible — the panels still draw — it just
 * mints one permanent series per film slug per city until the 4 GB monitoring
 * box falls over weeks later. So it is asserted two ways: the slug must not
 * appear anywhere in the exposition, and two DIFFERENT films must collapse onto
 * one series rather than two.
 *
 * Route-path strings below are copied verbatim from the generated
 * `router.Routes` (`this.prefix + "$" + "city<[^/]+>/movie/" + ...`), so the
 * normalisation is exercised against Play's real spelling rather than a
 * convenient one.
 */
class HttpMetricsFilterSpec extends AnyFlatSpec with Matchers {

  private implicit val sys: ActorSystem  = ActorSystem("http-metrics-filter-spec")
  private implicit val mat: Materializer = Materializer(sys)
  private implicit val executionContext: ExecutionContext = sys.dispatcher

  private val Counter   = "kinowo_web_http_requests_total"
  private val Histogram = "kinowo_web_http_request_duration_seconds"

  // Exactly what the router generates for `GET /:city/movie/:slug`.
  private val FilmBySlugPath = "/$city<[^/]+>/movie/$slug<[^/]+>"
  private val CityIndexPath  = "/$city<[^/]+>/"
  private val MetricsPath    = "/metrics"

  /** A fresh registry + filter per scenario — a Prometheus metric name may be
   *  registered only once per registry, and counters never reset, so sharing one
   *  would make every assertion depend on test order.
   *
   *  `nanoTime` is a fixed 250 ms step so the histogram lands in a known bucket
   *  and the duration assertions can't flake on a slow machine. */
  private class Harness(elapsedNanos: Long = 250_000_000L) {
    private val registry = new PrometheusRegistry()
    private val metrics  = new WebHttpMetrics(registry, country = "pl")
    private var ticks    = 0L
    private val filter   = new HttpMetricsFilter(metrics, () => { ticks += elapsedNanos; ticks })

    def run(request: RequestHeader, upstream: Result = Results.Ok("ok")): Result = {
      val action = EssentialAction(_ => Accumulator.done(upstream))
      Await.result(filter(action)(request).run(), 5.seconds)
    }

    /** Runs a handler whose future FAILS, the way an unhandled controller
     *  exception reaches the filter chain (Play's error handler is outside it). */
    def runFailing(request: RequestHeader, error: Throwable): Unit = {
      val action = EssentialAction(_ => Accumulator.done(Future.failed[Result](error)))
      Await.ready(filter(action)(request).run(), 5.seconds)
    }

    def exposition: String = PrometheusExposition.render(registry)

    /** The value on the exposition line whose `name{labels}` matches exactly. */
    def valueOf(series: String): Option[Double] =
      exposition.linesIterator
        .find(_.startsWith(series + " "))
        .map(_.substring(series.length).trim.toDouble)

    def counterLines: Seq[String] =
      exposition.linesIterator.filter(_.startsWith(Counter + "{")).toSeq
  }

  private def routed(method: String, uri: String, routePath: String,
    controller: String = "controllers.MovieController", action: String = "index"): RequestHeader =
    FakeRequest(method, uri).addAttr(
      Router.Attrs.HandlerDef,
      HandlerDef(getClass.getClassLoader, "router", controller, action,
        Seq(classOf[String]), method, routePath, "", Seq.empty))

  // ── 1. It records at all ──────────────────────────────────────────────────

  "HttpMetricsFilter" should "increment the request counter and observe the latency histogram" in {
    val harness = new Harness()
    harness.run(routed("GET", "/poznan/", CityIndexPath))

    harness.valueOf(s"""$Counter{country="pl",method="GET",route="/:city/",status="2xx"}""") shouldBe Some(1.0)
    harness.valueOf(s"""${Histogram}_count{country="pl",method="GET",route="/:city/"}""") shouldBe Some(1.0)
    // 250 ms of injected clock, so the sum is the real observation, not a zero
    // that a broken `observe` would also produce.
    harness.valueOf(s"""${Histogram}_sum{country="pl",method="GET",route="/:city/"}""") shouldBe Some(0.25)
    // …and it landed in the 0.25s bucket, i.e. the buckets actually bracket a
    // page-render workload rather than saturating at the first boundary.
    harness.valueOf(s"""${Histogram}_bucket{country="pl",method="GET",route="/:city/",le="0.25"}""") shouldBe Some(1.0)
    harness.valueOf(s"""${Histogram}_bucket{country="pl",method="GET",route="/:city/",le="0.15"}""") shouldBe Some(0.0)
  }

  it should "pass the upstream result through untouched" in {
    val harness = new Harness()
    val result  = harness.run(routed("GET", "/poznan/", CityIndexPath), Results.Created("made"))
    result.header.status shouldBe 201
    Await.result(result.body.consumeData, 5.seconds).utf8String shouldBe "made"
  }

  // ── 2. Cardinality: the route PATTERN, never the raw URI ──────────────────

  it should "label a parameterised route with its pattern, not the requested URI" in {
    val harness = new Harness()
    harness.run(routed("GET", "/poznan/movie/interstellar-2014", FilmBySlugPath, action = "filmBySlug"))

    harness.valueOf(
      s"""$Counter{country="pl",method="GET",route="/:city/movie/:slug",status="2xx"}""") shouldBe Some(1.0)
    // The whole point: no user- or corpus-derived token reaches a label value.
    harness.exposition should not include "interstellar-2014"
    harness.exposition should not include "poznan"
  }

  it should "collapse different films and cities onto ONE series" in {
    val harness = new Harness()
    harness.run(routed("GET", "/poznan/movie/interstellar-2014", FilmBySlugPath, action = "filmBySlug"))
    harness.run(routed("GET", "/wroclaw/movie/diuna-2021", FilmBySlugPath, action = "filmBySlug"))
    harness.run(routed("GET", "/warszawa/movie/pulp-fiction-1994", FilmBySlugPath, action = "filmBySlug"))

    withClue(s"expected one series, got:\n${harness.counterLines.mkString("\n")}\n") {
      harness.counterLines should have size 1
    }
    harness.valueOf(
      s"""$Counter{country="pl",method="GET",route="/:city/movie/:slug",status="2xx"}""") shouldBe Some(3.0)
  }

  it should "bucket an unrouted request as `other` rather than minting a series per probed path" in {
    val harness = new Harness()
    // No HandlerDef: nothing matched, which is what a scanner's 404 looks like.
    harness.run(FakeRequest("GET", "/wp-admin/setup-config.php"), Results.NotFound)
    harness.run(FakeRequest("GET", "/.env"), Results.NotFound)

    harness.counterLines should have size 1
    harness.valueOf(s"""$Counter{country="pl",method="GET",route="other",status="4xx"}""") shouldBe Some(2.0)
    harness.exposition should not include "wp-admin"
  }

  it should "clamp an unknown request method, which is client-controlled" in {
    val harness = new Harness()
    harness.run(FakeRequest("BREW", "/nope"), Results.NotFound)

    harness.valueOf(s"""$Counter{country="pl",method="other",route="other",status="4xx"}""") shouldBe Some(1.0)
    harness.exposition should not include "BREW"
  }

  // ── 3. The scrape must not measure itself ─────────────────────────────────

  it should "not count the /metrics endpoint itself" in {
    val harness = new Harness()
    harness.run(routed("GET", "/metrics", MetricsPath, "controllers.MetricsController", "metrics"))

    // A family with no data points is omitted from the exposition entirely, so
    // the absence of the name IS the assertion that nothing was recorded.
    harness.exposition should not include Counter
    harness.exposition should not include Histogram
  }

  it should "not count /metrics even when the request arrives without a matched route" in {
    val harness = new Harness()
    harness.run(FakeRequest("GET", "/metrics"))

    harness.exposition should not include Counter
  }

  // ── 4. Status classes ─────────────────────────────────────────────────────

  it should "record the status class rather than the code" in {
    val harness = new Harness()
    harness.run(routed("GET", "/poznan/", CityIndexPath), Results.Redirect("/poznan/", 301))
    harness.run(routed("GET", "/poznan/", CityIndexPath), Results.NotModified)
    harness.run(routed("GET", "/poznan/", CityIndexPath), Results.InternalServerError("boom"))

    harness.valueOf(s"""$Counter{country="pl",method="GET",route="/:city/",status="3xx"}""") shouldBe Some(2.0)
    harness.valueOf(s"""$Counter{country="pl",method="GET",route="/:city/",status="5xx"}""") shouldBe Some(1.0)
    // 301 and 304 share a series — the documented cost of the class label.
    harness.counterLines should have size 2
  }

  it should "count a handler that throws as a 5xx" in {
    val harness = new Harness()
    harness.runFailing(routed("GET", "/poznan/", CityIndexPath), new RuntimeException("kaboom"))

    // Play's error handler renders the 500 outside the filter chain, so without
    // an explicit failure branch the app's own 500s would be the only responses
    // never counted.
    harness.valueOf(s"""$Counter{country="pl",method="GET",route="/:city/",status="5xx"}""") shouldBe Some(1.0)
    harness.valueOf(s"""${Histogram}_count{country="pl",method="GET",route="/:city/"}""") shouldBe Some(1.0)
  }
}
