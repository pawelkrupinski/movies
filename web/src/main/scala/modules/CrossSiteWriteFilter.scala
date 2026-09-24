package modules

import org.apache.pekko.stream.Materializer
import play.api.mvc.{Filter, RequestHeader, Result, Results}

import scala.concurrent.Future

/**
 * Refuses a state-changing request (POST/PUT/PATCH/DELETE) that comes from
 * another site's page.
 *
 * Every write route is `nocsrf` — the browser JS calls them with `fetch`, the
 * native apps with a plain HTTP client, Meta server-to-server — so Play's token
 * check guards none of them. Without this filter, the only thing standing between
 * another site's page and a visitor's `/api/me` delete, "sign out everywhere", or
 * an admin's `/admin/config/set` is the session cookie's SameSite attribute (and,
 * for PUT/DELETE and JSON bodies, CORS refusing credentials). This is the second
 * layer, independent of the cookie: a cross-site write is refused before any
 * controller sees it, whatever the cookie says.
 *
 * HOW A REQUEST IS JUDGED CROSS-SITE. The browser's own `Sec-Fetch-Site` when it
 * sends one: only `cross-site` is refused — the site's own pages send
 * `same-origin`, and `same-site` can only come from a sibling host we run
 * ourselves. A browser that predates Fetch Metadata (Safari before 16.4) sends
 * none, so the page it names instead decides: `Origin` (which every browser puts
 * on a cross-origin write), else `Referer`, refused unless it is this request's own
 * origin. A request naming no page at all — the native apps (URLSession / OkHttp),
 * Meta's data-deletion callback — passes untouched, as before. Reads and CORS
 * preflights pass.
 *
 * A GET that changes session state is a write any page can issue with an `<img>`,
 * so its route carries the `siteonly` modifier and is held to the same rule — with
 * one allowance: it may come from ANY of our deployed origins, since the far half
 * of a sign-out (`/auth/sso/logout`) is reached by redirect from our other domain,
 * which the browser rightly calls cross-site. A cross-site one naming no page (a
 * stripped referrer) is refused: that is also how a forged one would arrive.
 *
 * Refused, such a GET is not answered with a bare 403: its legitimate caller is a
 * visitor mid-way through a redirect chain (a browser that strips the Referer on
 * the cross-site hop is indistinguishable from a forgery), so it is sent on to
 * where the leg was headed — the validated `next`, else the landing — WITHOUT the
 * state change. The worst a forged one achieves is a redirect.
 */
class CrossSiteWriteFilter()(implicit override val mat: Materializer) extends Filter {

  override def apply(next: RequestHeader => Future[Result])(request: RequestHeader): Future[Result] =
    if (CrossSiteWriteFilter.UnsafeMethods(request.method) && CrossSiteWriteFilter.crossSite(request))
      Future.successful(Results.Forbidden("Cross-site write refused."))
    else if (CrossSiteWriteFilter.siteOnly(request) && CrossSiteWriteFilter.crossSite(request) &&
             !CrossSiteWriteFilter.namedOrigin(request).exists(models.Country.deployedOrigins))
      Future.successful(controllers.PerUserResponse(
        Results.Redirect(controllers.AuthController.ssoLogoutOnward(request.getQueryString("next")))))
    else next(request)
}

object CrossSiteWriteFilter {
  private val UnsafeMethods = Set("POST", "PUT", "PATCH", "DELETE")

  /** The route modifier marking a GET that changes session state. */
  val SiteOnlyModifier = "siteonly"

  private def siteOnly(request: RequestHeader): Boolean =
    request.attrs.get(play.api.routing.Router.Attrs.HandlerDef).exists(_.modifiers.contains(SiteOnlyModifier))

  /** Whether `request` came from another site's page — see the class doc. */
  private[modules] def crossSite(request: RequestHeader): Boolean =
    request.headers.get("Sec-Fetch-Site") match {
      case Some(site) => site == "cross-site"
      case None       => namedOrigin(request).exists(_ != controllers.ForwardedUrl.base(request))
    }

  /** The origin of the page `request` says it came from: `Origin`, else the
   *  origin part of `Referer`. `Origin: null` (a sandboxed or privacy-stripped
   *  context) is kept as "null", which is nobody's origin. */
  private[modules] def namedOrigin(request: RequestHeader): Option[String] =
    request.headers.get("Origin")
      .orElse(request.headers.get("Referer").map(controllers.ForwardedUrl.originOf))
}
