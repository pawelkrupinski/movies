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
 * Carries the fields that actually let a human tell bot traffic from real
 * visitors apart — `ip`, `ua`, `referer`, `lang` — the same signal shape
 * `/var/log/caddy/access-*.log` gives for the live site. `ip` reads
 * `Fly-Client-IP`, NOT `X-Forwarded-For`: Fly's own docs say that header is
 * authoritative only when no OTHER reverse proxy sits in front of the app,
 * which is true here — unlike kinowo.net (behind Cloudflare + Caddy),
 * kinowo.fly.dev is a bare Fly hostname with Fly Proxy as the only hop.
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

  def hit(outcome: String, request: RequestHeader): Unit = {
    def header(name: String): String = request.headers.get(name).getOrElse("-")
    logger.info(s"$outcome ${request.method} ${request.path} ip=${header("Fly-Client-IP")} " +
      s"ua=${header("User-Agent")} referer=${header("Referer")} lang=${header("Accept-Language")}")
  }
}
