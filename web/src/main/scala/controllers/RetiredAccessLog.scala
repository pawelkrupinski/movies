package controllers

import play.api.Logger
import play.api.mvc.RequestHeader

/**
 * The only request-level record a RETIRED deployment keeps (see
 * [[modules.RetiredComponents]] for why it has no other telemetry — no Mongo,
 * no Prometheus scrape, `/metrics` itself answers 404). Its whole purpose is
 * deciding WHEN kinowo.fly.dev's traffic has died down enough to turn the host
 * off for good: before this, the only visibility into it was Fly's own
 * built-in `fly_app_http_responses_count` (status codes, no path) and a live
 * `fly logs` tail that never carried a single per-request line, because
 * [[RetiredSiteController]] never logged anything.
 *
 * One line per request that actually reaches a handler — `outcome` is
 * `notice` / `redirect` / `upgrade`, matching the three-way split the class
 * doc of [[RetiredSiteController]] describes. `health`/`metrics` are
 * deliberately NOT logged here: a platform health check fires every few
 * seconds forever and would drown the real traffic this exists to see.
 *
 * Fixed name, not class-derived, for the same reason `kinowo.removal-audit`
 * is (see `services.movies.RemovalAudit`): `controllers` does not appear in
 * `web/logback.xml`, so a `Logger(classOf[RetiredSiteController])` would
 * silently inherit the root's WARN and never emit anything.
 * `LogbackConfigSpec` holds the line that keeps this name explicit there.
 */
object RetiredAccessLog {

  val LoggerName = "kinowo.retired-access"
  private val logger = Logger(LoggerName)

  def hit(outcome: String, request: RequestHeader): Unit =
    logger.info(s"$outcome ${request.method} ${request.path} ua=${request.headers.get("User-Agent").getOrElse("-")}")
}
