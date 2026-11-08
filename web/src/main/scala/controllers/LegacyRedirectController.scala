package controllers

import play.api.mvc._

/** Addresses this site no longer mints but still answers, each with a redirect onto
 *  the one it mints now: the pre-rename Polish `/film…` / `/filmy` paths, the
 *  slash-less `/{city}`, and the city share cards' former asset address. */
class LegacyRedirectController(cc: ControllerComponents,
                               cityRequests: CityRequests,
                              ) extends AbstractController(cc) {

  import cityRequests.{servingCountry, withCity}

  /** `/{city}/film…` and `/{city}/filmy` — the pre-rename Polish spellings of
   *  the detail page and the browse facets, 301'd onto `/{city}/movie…` and
   *  `/{city}/movies`. Kept routable indefinitely for the same reason the
   *  `?title=` form is: search indexes, shared links and installed app builds
   *  all still carry the old address. */
  def filmLegacy(city: String): Action[AnyContent] = Action { request =>
    movedToCityPath(city, "movie", request)
  }

  def filmSubPathLegacy(city: String, rest: String): Action[AnyContent] = Action { request =>
    movedToCityPath(city, s"movie/$rest", request)
  }

  def browseLegacy(city: String): Action[AnyContent] = Action { request =>
    movedToCityPath(city, "movies", request)
  }

  /** `/{city}` — the canonical listing address minus its trailing slash.
   *
   *  The routes file binds the listing at `/{city}/` only, so the slash-less
   *  spelling 404'd. Nothing this app mints carries it — the sitemap, the
   *  canonical tag and every internal link all end in `/` — but crawlers and
   *  people typing a city by hand do: Googlebot asked showtimes.cc for
   *  `/de/karlsruhe`, got a 404, and `/de/karlsruhe/` was 200 the whole time.
   *  That spends crawl budget on an error and drops whatever link pointed
   *  there, so the two spellings are folded together with a 301 onto the one
   *  everything else advertises.
   *
   *  An unknown slug still 404s, because `movedToCityPath` resolves through
   *  `withCity` — the route is a catch-all for any single top-level segment, so
   *  without that every mistyped path would 301 onto a URL that 404s one hop
   *  later. */
  def indexNoTrailingSlash(city: String): Action[AnyContent] = Action { request =>
    movedToCityPath(city, "", request)
  }

  /** 301 onto `/{prefix}/{city}/{tail}`, query string intact — the shared shape
   *  of every redirect here that lands a non-canonical address on its canonical
   *  one. An empty `tail` is the city listing itself.
   *
   *  Resolved through `withCity` so the mount prefix comes off the CITY, the
   *  same way every other URL builder here gets it — Play strips
   *  `play.http.context` before matching, so a redirect assembled from the
   *  route's own `:city` alone would drop the `/uk` and land off-site. It also
   *  means an unknown city 404s here rather than being bounced onto a URL that
   *  404s one hop later.
   *
   *  The query string rides along verbatim: `?title=`, the browse facets
   *  (including the legacy Polish `kraj`/`rezyser`/… spellings) and the shared
   *  filter links all live there, and dropping them would answer 200 with the
   *  wrong content — the failure mode nobody reports. */
  private def movedToCityPath(city: String, tail: String, request: RequestHeader): Result =
    withCity(city, request) { c =>
      val path = s"${c.country.pathPrefix}/${c.slug}/$tail"
      MovedPermanently(if (request.rawQueryString.isEmpty) path else s"$path?${request.rawQueryString}")
    }

  /** `/assets/img/og-<slug>.jpg` — the city card's address while it was a committed asset, kept
   *  because link previews cached it: permanently redirected to the worker's card — THIS country's
   *  city of that slug, never another's. A slug no city here answers to any more goes to this
   *  deployment's home card. */
  def legacyCityCard(file: String): Action[AnyContent] = Action {
    val slug = file.stripPrefix("og-").stripSuffix(".jpg")
    servingCountry.resolve(slug).fold(Found(ShareCardUrl.home(servingCountry)))(city => MovedPermanently(ShareCardUrl.city(city)))
  }

  /** `/share-card-fallback/<cc>` — where Caddy sends a request for a city card the worker has not
   *  drawn yet (a city new to the catalog): the country's home card, never a 404. A 302: the card
   *  will exist soon. */
  def cityCardFallback(country: String): Action[AnyContent] = Action {
    Found(ShareCardUrl.home(models.Country.byCode(country).getOrElse(servingCountry)))
  }
}
