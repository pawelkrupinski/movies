package controllers

import models._
import play.api.mvc._
import play.api.Mode


class MovieController( cc: ControllerComponents,
                       // Which cities are ours, the remembered-city cookie, the deployment's
                       // one language and the conditional GET — shared with every other
                       // city-route controller (`CityRequests`).
                       cityRequests: CityRequests,
                       movieControllerService: MovieControllerService,
                       // NO `UserRepository` — deliberately, and it is the strongest
                       // form of the promise `CachePolicy` needs. This
                       // controller renders every page it serves without the means to
                       // find out who asked for it, so no response of its can vary by
                       // session, so a shared cache holding the listing cannot leak
                       // anybody.
                       // Who is signed in is `AuthController`'s question, answered
                       // per client at `/api/me`.
                       oauthProviders: Set[String],
                       environment: Mode,
                       // Rewrites every page's inline `<script>`/`<style>`
                       // blocks — the composition root's choice for this mode
                       // (`Minifier.forMode`), so its memo caches live and die
                       // with the wiring rather than the JVM.
                       minifier: tools.Minifier,
                       // The third-party page tags (`fb:app_id`, GA, Sentry). A
                       // function read per render rather than a value fixed at
                       // boot, so a live config change reaches the page.
                       pageTags: () => PageTags,
                       // Rendered film cards kept across renders of a listing (see
                       // `FilmCardFragments`); the templates find it as their implicit.
                       filmCards: FilmCardFragments = FilmCardFragments.Uncached,
                       // Where each listing render's allocation is reported (`WebRenderMetrics`);
                       // nowhere unless the composition root says.
                       recordRender: (String, Long) => Unit = (_, _) => (),
                     ) extends AbstractController(cc) {
  // The film pages are [[FilmPageController]]'s.

  import cityRequests.{cityCookie, conditionalResponse, deploymentMessages, withCity}

  private implicit val fragments: FilmCardFragments = filmCards

  private val cityStructuredData = new CityStructuredData
  private val renders            = new ListingRenders(recordRender)

  /** How many listings' JSON-LD this controller is holding — for the spec. */
  private[controllers] def structuredDataHeld: Long = cityStructuredData.size

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

  import ResponseBody.{HtmlContentType, okHtml}

  /** The main "Filmy" listing — repertoire view, full corpus, OG meta derived
   *  from `?…` filter parameters. Shared between `/` and `/movies` (no
   *  parameters) so both URLs are interchangeable; `/movies` with one of the
   *  browse-axis parameters still routes through `browse` below to the
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
  private def renderIndex(city: City, request: RequestHeader): Result =
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
  private def listingBody(city: City, request: RequestHeader): ResponseBody = {
    implicit val c: City = city
    implicit val messages: play.api.i18n.Messages = deploymentMessages
    renders.listing(ResponseBody.html(renderIndexHtml(city, request)))
  }

  private def renderIndexHtml(city: City, request: RequestHeader)(implicit c: City, messages: play.api.i18n.Messages): play.twirl.api.Html = {
    // One clock for both the filtering and the page's own expiry countdown —
    // `_repertoireView` counts forward from `renderedAt`, so it has to be the
    // instant the schedules were actually pruned at.
    val now         = movieControllerService.nowIn(city)
    // Measured on its own as well as within the whole render, so the histogram says how
    // much of a listing's cost is building its schedules (`listing_schedules`).
    val schedules   = renders.schedules(movieControllerService.toSchedules(city, now))
    val meta        = FilterDescription.forIndex(city, request.queryString, schedules)
    val isLargeCity = MovieControllerService.totalShowtimes(schedules) > MovieControllerService.LargeCityShowtimeThreshold
    val pageUrl     = PageMeta.canonicalUrl(request)
    views.html.repertoire(
      schedules,
      city.cinemaDisplayNames,
      city.cinemaPillMap,
      devMode, minifier, oauthProviders, renderedAt = now,
      isLargeCity     = isLargeCity,
      pageTitle       = meta.title,
      pageDescription = meta.description,
      pageUrl         = pageUrl,
      pageTags        = pageTags(),
      // og:url keeps the filtered request URL (so a shared filtered link
      // previews the filter), but the canonical folds `/{city}/movies` and every
      // `?filter` variation back to the bare listing.
      canonicalUrl    = PageMeta.origin(request) + CityPath(city) + "/",
      structuredData  = Some(cityStructuredData.of(pageUrl, city, schedules)),
    )
  }

  def index(city: String): Action[AnyContent] = Action { request =>
    withCity(city, request)(c => canonicalGenreLink(c, "", request).getOrElse(renderIndex(c, request)))
  }

  /** A link spelling a genre as a venue printed it 301s onto the vocabulary's spelling ([[GenreLinks]]). */
  private def canonicalGenreLink(c: City, tail: String, request: RequestHeader): Option[Result] =
    GenreLinks.canonical(c, tail, request.queryString).map(MovedPermanently(_))

  private def renderBrowse(city: City, heading: String, films: Seq[FilmSchedule], request: RequestHeader): Result = {
    implicit val c: City = city
    implicit val messages: play.api.i18n.Messages = deploymentMessages
    // Client-independent like the listing (nobody is rendered into it), but a
    // facet URL is one of combinatorially many and earns no edge entry.
    okHtml(views.html.browse(
      films, heading, minifier, oauthProviders,
      pageUrl = PageMeta.canonicalUrl(request),
      pageTags = pageTags(),
      // A FACET IS UI STATE, NOT A PAGE. `?cast=` alone is one URL per cast
      // member per city, so the set of these is combinatorial rather than
      // merely large, and every one of them is a near-duplicate of the city
      // listing built from the same films. og:url keeps the filtered URL (a
      // shared filtered link should preview its filter), the canonical folds
      // them back onto the listing, and `noindex,follow` keeps the crawler
      // walking through to the film pages, which ARE the content.
      //
      // robots.txt already disallows this path; this is the second line, for
      // the crawlers that ignore it — see the note on `_ogTagsApp`.
      canonicalUrl = PageMeta.origin(request) + CityPath(city) + "/",
      robots = MovieController.FacetRobots,
    )).withHeaders("Cache-Control" -> "private, no-cache").withCookies(cityCookie(city))
  }

  /** The four legacy Polish param names (`kraj`/`rezyser`/`aktor`/`gatunek`) are still
   *  bound and still filter. They were the only spelling until the facets were renamed to
   *  English for the shared route table, so every link minted before that — a bookmark, a
   *  shared URL, anything already crawled — carries them. Dropping the binding did not 404
   *  those; it fell through to the no-axis case and rendered the UNFILTERED city listing,
   *  a 200 with the wrong content, which is the failure mode nobody reports. The English
   *  name wins when both are present. */
  def browse(city: String, country: Option[String], director: Option[String], cast: Option[String], genre: Option[String],
             kraj: Option[String] = None, rezyser: Option[String] = None,
             aktor: Option[String] = None, gatunek: Option[String] = None): Action[AnyContent] = Action { request =>
    withCity(city, request) { c => canonicalGenreLink(c, "movies", request).getOrElse {
      val all = movieControllerService.toSchedules(c)
      (country.orElse(kraj), director.orElse(rezyser), cast.orElse(aktor), genre.orElse(gatunek)) match {
        case (Some(name), _, _, _) => renderBrowse(c, name, all.filter(_.movie.countries.contains(name)), request)
        case (_, Some(name), _, _) => renderBrowse(c, name, all.filter(_.director.contains(name)),        request)
        case (_, _, Some(name), _) => renderBrowse(c, name, all.filter(_.cast.contains(name)),            request)
        case (_, _, _, Some(name)) => renderBrowse(c, name, all.filter(_.movie.genres.contains(name)),    request)
        // `/{city}/movies` with no filter axis is the main listing — the same
        // view as `/{city}/`. The browse view only kicks in for the per-axis
        // pages reached from the meta-link rows on /movie.
        case _                     => renderIndex(c, request)
      }
    } }
  }

  private def devMode: Boolean = DevMode.enabled(environment)
}

object MovieController {

  /** What the faceted browse pages tell a crawler about themselves.
   *
   *  `follow` and not `none`: the point is to keep the facet URLs out of an
   *  index, not to hide the film links they carry. Those links are the reason
   *  the page is worth crawling at all, and each one lands on a film page that
   *  IS indexable. */
  val FacetRobots = "noindex,follow"
}
