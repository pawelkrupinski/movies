package modules

import org.apache.pekko.stream.Materializer
import play.api.mvc.{Filter, RequestHeader, Result}

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

/**
 * Adds a Content-Security-Policy header (and a small set of related
 * security headers) to every response. CSP shrinks the XSS blast
 * radius: even if an attacker plants a payload inside film data or a
 * search query, the browser refuses to execute it unless it matches
 * one of the allowed sources below.
 *
 * The policy keeps `'unsafe-inline'` for both `script-src` and
 * `style-src` because every template has hand-written inline
 * `<script>` and `<style>` blocks. Tightening to nonces is the next
 * iteration — it would touch every inline block in the view layer.
 *
 * Allow-lists below trace the actual external surfaces the app uses:
 *
 *  - `js.sentry-cdn.com` / `sentry.io` for the optional Sentry SDK
 *    loaded via [[views.html._errorTracking]] when `SENTRY_LOADER_URL`
 *    is set.
 *  - `googletagmanager.com` / `google-analytics.com` for the optional
 *    GA4 tag loaded via [[views.html._analytics]] when
 *    `GA_MEASUREMENT_ID` is set. Both endpoints stay listed even
 *    when the env vars aren't set; an absent script tag means
 *    nothing connects, so the policy is harmless.
 *  - `*.youtube.com` for the trailer `<iframe>`s the /film page
 *    renders.
 *
 * Image sources are wide (`https:` + `data:`) because cinema CDNs
 * resolve to many different hosts and they shift over time — pinning
 * them risks breaking the page for posters when a cinema updates
 * their CDN.
 */
class CspFilter @Inject() (implicit
  override val mat: Materializer,
  ec: ExecutionContext,
) extends Filter {

  private val csp: String = Seq(
    "default-src 'self'",
    "script-src 'self' 'unsafe-inline' https://js.sentry-cdn.com https://browser.sentry-cdn.com https://www.googletagmanager.com https://www.google-analytics.com",
    "style-src 'self' 'unsafe-inline'",
    "img-src 'self' data: https:",
    "font-src 'self' data:",
    "connect-src 'self' https://*.sentry.io https://www.google-analytics.com https://*.googletagmanager.com",
    "frame-src https://www.youtube.com https://www.youtube-nocookie.com",
    "object-src 'none'",
    "base-uri 'self'",
    "form-action 'self'",
    "frame-ancestors 'none'",
  ).mkString("; ")

  override def apply(next: RequestHeader => Future[Result])(rh: RequestHeader): Future[Result] =
    next(rh).map { result =>
      // Don't overwrite an upstream CSP if some controller has set
      // one explicitly — they win. Same goes for the other headers
      // below; most upstreams won't touch them.
      val withCsp =
        if (result.header.headers.contains("Content-Security-Policy")) result
        else result.withHeaders("Content-Security-Policy" -> csp)

      withCsp.withHeaders(
        "X-Content-Type-Options" -> "nosniff",
        "Referrer-Policy"        -> "strict-origin-when-cross-origin",
        // `interest-cohort=()` opts the site out of FLoC / Topics
        // ad-tracking experiments; the rest are unused features the
        // site doesn't need, blocked so a future leaked dependency
        // can't quietly turn them on.
        "Permissions-Policy"     -> "camera=(), microphone=(), geolocation=(), interest-cohort=()",
      )
    }(ec)
}
