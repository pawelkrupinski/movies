package services.metrics

import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.mvc.RequestHeader
import play.api.routing.{HandlerDef, Router}
import play.api.test.FakeRequest
import tools.MutableClock

import java.time.{Duration, Instant}

/**
 * Locks the retirement signal: WHEN each route pattern was last called, by WHICH
 * kind of client, plus the inventory of every defined route so one nobody calls
 * still shows up on the "longest uncalled" panel instead of being absent.
 */
class WebRouteUsageMetricsSpec extends AnyFlatSpec with Matchers {

  private val LastCalled = "kinowo_web_route_last_called_seconds"
  private val Defined    = "kinowo_web_route_defined"
  private val Start      = Instant.parse("2026-10-11T12:00:00Z")

  // Exactly what the generated router spells for `GET /:city/api/repertoire`.
  private val RepertoirePath = "/$city<[^/]+>/api/repertoire"

  private def routed(path: String, routePattern: String, userAgent: Option[String]): RequestHeader = {
    val handler = HandlerDef(getClass.getClassLoader, "router", "controllers.MovieApiController", "apiRepertoire",
                             Nil, "GET", routePattern)
    val base = FakeRequest("GET", path).addAttr(Router.Attrs.HandlerDef, handler)
    userAgent.fold(base)(agent => base.withHeaders(play.api.mvc.Headers("User-Agent" -> agent)))
  }

  private def sample(body: String, series: String): Option[Double] =
    body.linesIterator.find(_.startsWith(series + " ")).map(_.split(' ').last.toDouble)

  "WebRouteUsageMetrics" should "stamp the latest call per route pattern and client kind, never the raw path" in {
    val registry = new PrometheusRegistry()
    val clock    = new MutableClock(Start)
    val metrics  = new WebRouteUsageMetrics(registry, "pl", clock)

    metrics.record(routed("/poznan/api/repertoire", RepertoirePath, Some("KinowoIOS/1.0")))
    clock.advance(Duration.ofHours(3))
    metrics.record(routed("/wroclaw/api/repertoire", RepertoirePath, Some("KinowoIOS/1.0")))
    metrics.record(routed("/wroclaw/api/repertoire", RepertoirePath, Some("Mozilla/5.0 (compatible; Googlebot/2.1)")))

    val body = PrometheusExposition.render(registry)
    body should not include "poznan"
    sample(body, s"""$LastCalled{client="ios_app",country="pl",route="/:city/api/repertoire"}""") shouldBe
      Some(Start.plus(Duration.ofHours(3)).getEpochSecond.toDouble)
    sample(body, s"""$LastCalled{client="bot",country="pl",route="/:city/api/repertoire"}""") shouldBe defined
  }

  it should "publish every declared route as defined, called or not" in {
    val registry = new PrometheusRegistry()
    val metrics  = new WebRouteUsageMetrics(registry, "pl", new MutableClock(Start))

    metrics.declareRoutes(Seq(RepertoirePath, "/api/catalog", RepertoirePath))

    val body = PrometheusExposition.render(registry)
    sample(body, s"""$Defined{country="pl",route="/:city/api/repertoire"}""") shouldBe Some(1.0)
    sample(body, s"""$Defined{country="pl",route="/api/catalog"}""") shouldBe Some(1.0)
    body should not include LastCalled + "{"
  }

  "WebRouteUsageMetrics.clientKind" should "sort a User-Agent into a closed set" in {
    import WebRouteUsageMetrics.clientKind
    clientKind(Some("KinowoIOS/1.0")) shouldBe "ios_app"
    clientKind(Some("KinowoAndroid/1.0")) shouldBe "android_app"
    // Since 2026-10-24 each app names its build and OS behind the same prefix.
    clientKind(Some("KinowoIOS/2.1.3 (iOS 18.2.1)")) shouldBe "ios_app"
    clientKind(Some("KinowoAndroid/2.1.3 (Android 15; API 35)")) shouldBe "android_app"
    clientKind(Some("Mozilla/5.0 (iPhone; CPU iPhone OS 18_0 like Mac OS X) AppleWebKit/605.1.15 Safari/604.1")) shouldBe "browser"
    clientKind(Some("Mozilla/5.0 (compatible; Googlebot/2.1; +http://www.google.com/bot.html)")) shouldBe "bot"
    clientKind(Some("meta-externalagent/1.1")) shouldBe "bot"
    clientKind(Some("facebookexternalhit/1.1")) shouldBe "bot"
    clientKind(Some("curl/8.7.1")) shouldBe "bot"
    clientKind(Some("python-requests/2.32")) shouldBe "bot"
    clientKind(Some("SomethingElse")) shouldBe "other"
    clientKind(None) shouldBe "other"
  }
}
