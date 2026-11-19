package controllers

import models.City
import play.api.mvc.{RequestHeader, Result}

/** How the city listing (`/{city}/`, `/{city}/movies`) is served ([[MovieController]]): the plain page as one
 *  pre-rendered, pre-gzipped blob offered to the edge, a `?filter` variant with the same validators but no blob and
 *  browser-only — the body [[ListingHtml]]'s, built from [[MovieControllerService]]'s schedules, its allocation
 *  reported through [[ListingRenders]]. */
final class ListingResponse(cityRequests: CityRequests, movieControllerService: MovieControllerService,
                            html: ListingHtml, renders: ListingRenders) {

  import cityRequests.{cityCookie, conditionalResponse, deploymentMessages}
  import ResponseBody.HtmlContentType

  /** How many listings' JSON-LD the page is holding — for the spec. */
  private[controllers] def structuredDataHeld: Long = html.structuredDataHeld

  // The plain HTML pages (`/{city}/`, `/{city}/movies`) are byte-identical for
  // EVERY visitor at a given cache version — signed in or not, which is the whole
  // point of `_authMenu` no longer knowing — so we serve a pre-rendered,
  // pre-gzipped blob keyed on the request path (which fully determines the
  // output: city and page type).
  //
  // THE PREDICATE USED TO ASK `user.isEmpty`, AND THAT WAS THE CEILING. It meant
  // a signed-in visitor rendered fresh and uncacheably, but far more expensively
  // it meant the response could never be offered to a shared cache at all: the
  // page differed per visitor, so Cloudflare had to be told `private, no-cache`
  // and the edge held nothing but the JSON. Nobody's name reaches this render any
  // more, so the only things left that change the bytes are the ones below.
  //
  // Filter queries are the only thing left that bypasses it: they move the OG
  // meta, and `request.path` — the blob's key — drops the query string. That
  // costs them the blob, not the conditional GET: `renderIndex` still runs them
  // through `conditionalResponse.serve` keyed on the query, for the validators. A client
  // that cannot take gzip no longer bypasses it either, because it never needed
  // to: `conditionalResponse.serve` serves that client the uncompressed body with the
  // same validators, and the `Vary: Accept-Encoding` both branches carry is what
  // keeps the two spellings apart in a shared cache. `/api/repertoire` has been
  // shared-cacheable on exactly those terms since it was first offered to the edge.
  //
  // Query string is the only thing left that can vary these bytes per
  // request — the language picker used to be the other half of this
  // argument (a visitor's resolved language could take the rendered bytes
  // off the deployment default), but language switching is client-side now,
  // so every plain city URL is offered to the edge unconditionally.
  private def cacheablePlainPage(request: RequestHeader): Boolean = request.queryString.isEmpty

  /** The main "Filmy" listing — repertoire view, full corpus, OG meta derived
   *  from `?…` filter parameters. Shared between `/` and `/movies` (no
   *  parameters) so both URLs are interchangeable; `/movies` with one of the
   *  browse-axis parameters still routes through [[BrowsePage]] to the
   *  per-director / per-cast / per-country page.
   *
   *  RENDERED FOR NOBODY — AND THEREFORE OFFERED TO THE EDGE. This handler
   *  cannot ask who is making the request, and that is the safety argument
   *  rather than a side effect of it: the controller holds no `UserRepository`,
   *  `views.html.repertoire` has no parameter to take a `models.User`, and
   *  `_authMenu` has nothing to draw. So the bytes cannot depend on the session
   *  cookie, and a shared cache cannot hand one visitor's page to another — the rule
   *  `CachePolicy.RevalidatedAnywhere` states, met structurally rather than by
   *  inspection.
   *  Whoever is signed in is layered on after first paint, by `shared.js` off
   *  `/api/me` (which is `no-store`, so it cannot be shared either).
   *
   *  `?filter=` variants keep `private, no-cache`: still client-independent, but
   *  combinatorially many and not worth an edge entry each. They do carry the
   *  same validators, so revalidating one comes back 304 rather than re-sending
   *  the listing — what they skip is the shared blob, not the conditional GET. */
  def renderIndex(city: City, request: RequestHeader): Result =
    if (cacheablePlainPage(request)) servePlainListing(city, request)
    else serveFilteredListing(city, request)

  // 304 short-circuits before any work; on a 200 cache hit `renderIndexHtml`
  // (and its data-prep) never runs either.
  // NO `Set-Cookie` HERE, AND THAT IS THE WHOLE POINT OF THE BRANCH.
  // Cloudflare bypasses any response carrying one: measured, this page went
  // `DYNAMIC` -> `BYPASS` the moment a Cache Rule made it eligible, with the
  // `city=` cookie the only thing left on it. The cookie exists so the bare
  // `/` landing can bounce a returning visitor to their city; it is
  // `httpOnly = false` precisely so the client owns it, and `shared.js`
  // writes it on load with the same name, path and lifetime. The filtered
  // branch below is `private, no-cache`, so it keeps setting it server-side
  // and a visitor with no JS is still remembered.
  private def servePlainListing(city: City, request: RequestHeader): Result =
    conditionalResponse.serve(request, HtmlContentType, CachePolicy.RevalidatedAnywhere,
                              city = Some(city))(listingBody(city, request))

  // A FILTER VARIANT STILL GETS VALIDATORS, JUST NOT A BLOB.
  //
  // `private, no-cache` tells the browser to store the page and re-validate
  // before every re-use -- but a re-validation with nothing to validate
  // AGAINST cannot come back 304, so every refresh of a shared
  // `?date=tomorrow` link re-downloaded the whole listing (265 KB gzipped,
  // 3.8 MB of HTML for Manchester). An ETag makes that refresh free when the
  // city has not moved, without promising any cache it may serve the copy
  // unasked.
  //
  // `cacheBody = false` is the other half of the decision above: these
  // variants stay out of the shared gzip cache, which is an LRU over BYTES,
  // so one-off filter combinations cannot evict the bare city pages that
  // earn their place there. The GzipFilter still compresses on the way out.
  //
  // The whole query string is the key, not a normalised subset of it: the
  // page puts the request's own URL in `og:url`, so two spellings that mean
  // the same thing really do render different bytes and must not share a
  // validator. Nothing is stored per key, so an appended `?foo=1` costs a
  // hash and nothing else.
  //
  // WHAT THE ETAG PROMISES, PRECISELY: the read-model version this page was
  // cut from, not a hash of the bytes. With no blob pinning them, a body
  // re-rendered a few minutes later differs in the showtimes that have since
  // started -- under the same validator, so a client holding the earlier
  // copy keeps it until the city's stamp moves or its midnight arrives. That
  // is the same age the branch above serves from its blob between two stamps,
  // and the page is built for it: each slot's expiry prunes the lapsed showtimes
  // client-side and `data-next-day` retires the document at midnight. What
  // it costs that the blob branch does not is byte-identity between two
  // clients holding one validator, which only a SHARED cache could observe
  // -- and `private, no-cache` is exactly the instruction that none may.
  private def serveFilteredListing(city: City, request: RequestHeader): Result =
    conditionalResponse.serve(request, HtmlContentType, CachePolicy.BrowserOnly,
                              cacheKey = "|q=" + request.rawQueryString, city = Some(city),
                              cacheBody = false)(listingBody(city, request))
      .withCookies(cityCookie(city))

  /** The listing's body, its allocation reported as `listing`. */
  private def listingBody(city: City, request: RequestHeader): ResponseBody =
    renders.listing(ResponseBody.html(renderIndexHtml(city, request)))

  private def renderIndexHtml(city: City, request: RequestHeader): play.twirl.api.Html = {
    // One clock for both the filtering and the page's own expiry countdown —
    // `_repertoireView` counts forward from `renderedAt`, so it has to be the
    // instant the schedules were actually pruned at.
    val now       = movieControllerService.nowIn(city)
    // Measured on its own as well as within the whole render, so the histogram says how
    // much of a listing's cost is building its schedules (`listing_schedules`).
    val schedules = renders.schedules(movieControllerService.toSchedules(city, now))
    html.render(city, request, now, schedules)(using deploymentMessages)
  }
}
