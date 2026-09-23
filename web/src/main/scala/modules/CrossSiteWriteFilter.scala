package modules

import org.apache.pekko.stream.Materializer
import play.api.mvc.{Filter, RequestHeader, Result, Results}

import scala.concurrent.Future

/**
 * Refuses a state-changing request (POST/PUT/PATCH/DELETE) that the browser
 * itself labels `Sec-Fetch-Site: cross-site`.
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
 * Only `cross-site` is refused. The site's own pages send `same-origin`; the
 * native apps (URLSession / OkHttp) and Meta's data-deletion callback send no
 * `Sec-Fetch-*` header at all and pass untouched. `same-site` still passes: the
 * PL deployment and the `showtimes.cc` ones are different sites, so it can only
 * come from a sibling host we run ourselves. Reads and CORS preflights pass.
 */
class CrossSiteWriteFilter()(implicit override val mat: Materializer) extends Filter {

  override def apply(next: RequestHeader => Future[Result])(request: RequestHeader): Future[Result] =
    if (CrossSiteWriteFilter.UnsafeMethods(request.method) &&
        request.headers.get("Sec-Fetch-Site").contains("cross-site"))
      Future.successful(Results.Forbidden("Cross-site write refused."))
    else next(request)
}

object CrossSiteWriteFilter {
  private val UnsafeMethods = Set("POST", "PUT", "PATCH", "DELETE")
}
