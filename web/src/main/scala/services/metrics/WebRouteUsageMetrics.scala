package services.metrics

import io.prometheus.metrics.core.metrics.GaugeWithCallback
import io.prometheus.metrics.model.registry.PrometheusRegistry
import play.api.mvc.RequestHeader

import java.time.{Clock, Instant}
import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

/**
 * The retirement signal for EVERY route: when each route pattern was last called,
 * by which kind of client.
 *
 * `kinowo_web_http_requests_total` already says HOW OFTEN a route is hit; it can't
 * say "safe to retire", for two reasons this closes:
 *
 *  - WHO. A legacy URL shape keeps being crawled for months after the last human
 *    or app stopped using it. A route only bots still call is retirable; one an old
 *    app build still calls is not. Hence the `client` label.
 *  - NEVER. A counter series only exists once something calls the route, so a
 *    route nobody calls is ABSENT, not zero — invisible on exactly the panel that
 *    should rank it first. `kinowo_web_route_defined` publishes the full routes
 *    inventory so the dashboard can show "not called in the whole window".
 *
 * A last-called TIMESTAMP rather than a counter because pods restart on every
 * deploy: `increase()` drops the first increment of a series born after a restart,
 * which is precisely the lone call to a nearly-dead route this exists to see.
 * Read through `max_over_time(…[30d])` the gauge survives restarts — the
 * dashboard query does that, not this process.
 *
 * Cardinality: `route` is the matched pattern (the closed set of lines in the
 * routes file, `other` for unmatched — see [[WebHttpMetrics.routeLabel]]) and
 * `client` is one of five values, so the ceiling is routes × 5 per country and in
 * practice far lower: most routes are only ever hit by one or two client kinds.
 *
 * The mobile apps send a fixed `KinowoIOS/1.0` / `KinowoAndroid/1.0` with no
 * build number, so this tells "an app still calls it" but not WHICH app version.
 */
class WebRouteUsageMetrics(registry: PrometheusRegistry, country: String, clock: Clock) {

  private val lastCalled = new ConcurrentHashMap[(String, String), Instant]()
  private val defined    = ConcurrentHashMap.newKeySet[String]()

  GaugeWithCallback.builder()
    .name("kinowo_web_route_last_called_seconds")
    .help("Unix time each matched route pattern was last called since this process booted, by " +
      "country and client kind (ios_app, android_app, browser, bot, other). Absent until the first " +
      "call; read with max_over_time across restarts. Excludes the /metrics scrape and the " +
      "synthetic probe.")
    .labelNames("country", "route", "client")
    .callback { callback =>
      lastCalled.forEach { case ((route, client), instant) =>
        callback.call(instant.getEpochSecond.toDouble, country, route, client)
      }
    }
    .register(registry)

  GaugeWithCallback.builder()
    .name("kinowo_web_route_defined")
    .help("1 for every route pattern this deployment's router defines, by country — the inventory " +
      "kinowo_web_route_last_called_seconds is compared against, so a route nobody calls still shows.")
    .labelNames("country", "route")
    .callback(callback => defined.asScala.foreach(route => callback.call(1.0, country, route)))
    .register(registry)

  /** Called once per measured request by [[modules.HttpMetricsFilter]]. */
  def record(request: RequestHeader): Unit =
    lastCalled.put(
      (WebHttpMetrics.routeLabel(request),
       WebRouteUsageMetrics.clientKind(request.headers.get(play.api.http.HeaderNames.USER_AGENT))),
      clock.instant())

  /** Publishes the router's inventory, in Play's generated spelling
   *  (`/$city<[^/]+>/api/repertoire`), as the same `/:city/…` labels requests get. */
  def declareRoutes(routePatterns: Seq[String]): Unit =
    routePatterns.map(WebHttpMetrics.readablePattern).foreach(defined.add)
}

object WebRouteUsageMetrics {

  /** Self-declared crawlers, link-preview fetchers and scripted HTTP clients. Checked
   *  BEFORE `Mozilla/`, since most crawlers wear a Mozilla prefix. */
  private val BotAgent =
    """(?i)bot|crawl|spider|slurp|externalagent|externalhit|preview|curl|wget|python|go-http|java/|okhttp|headless|scrapy|httpclient""".r

  /** Sorts a User-Agent into the closed `client` label set. */
  def clientKind(userAgent: Option[String]): String = userAgent match {
    case Some(agent) if agent.startsWith("KinowoIOS/")             => "ios_app"
    case Some(agent) if agent.startsWith("KinowoAndroid/")         => "android_app"
    case Some(agent) if BotAgent.findFirstIn(agent).isDefined      => "bot"
    case Some(agent) if agent.startsWith("Mozilla/")               => "browser"
    case _                                                         => "other"
  }
}
