package modules.wiring

import services.metrics.EnvGatedFeature

/**
 * The worker's external integrations that a missing secret switches off WITHOUT failing the
 * boot — each reads `Env.get(…)` and quietly runs without when it is `None`. All of them are
 * set in production (worker-secrets, plus the two Facebook keys picked from web-secrets), so
 * any one reading off there is a degraded pipeline nobody would otherwise be told about:
 *
 *   - tmdb               no TMDB resolution: newcomers stay unresolved and never reach the site
 *   - omdb               no OMDb rating tier or backfill
 *   - residential_proxy  no Decodo egress: Cloudflare-fronted chains fall to Zyte or fail
 *   - zyte               no paid fallback behind the proxy
 *   - sentry             errors go nowhere but the log (logback's appender reads SENTRY_DSN)
 *   - facebook_rescrape  a changed share card is never re-scraped, so shared links keep the old image
 *
 * Deliberately NOT here, because a missing key does not disable anything: the share-card
 * budget and directory, the scrape levers and the heap-dump dir all fall back to a working
 * default. `MONGODB_URI` is not either: without it the worker has no corpus at all, which
 * every other alert already sees.
 */
object WorkerIntegrations {

  def features(read: String => Option[String]): Seq[EnvGatedFeature] = Seq(
    EnvGatedFeature.requiring("tmdb", Seq("TMDB_API_KEY"), read),
    EnvGatedFeature.requiring("omdb", Seq("OMDB_API_KEY"), read),
    EnvGatedFeature.requiring("residential_proxy", Seq("KINOWO_PROXY_USER", "KINOWO_PROXY_PASS"), read),
    EnvGatedFeature.requiring("zyte", Seq("ZYTE_API_KEY"), read),
    EnvGatedFeature.requiring("sentry", Seq("SENTRY_DSN"), read),
    EnvGatedFeature.requiring("facebook_rescrape", Seq("FACEBOOK_APP_ID", "FACEBOOK_APP_SECRET"), read))
}
