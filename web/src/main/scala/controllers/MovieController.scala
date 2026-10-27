package controllers

import models._
import play.api.mvc._
import play.api.Mode
import services.readmodel.WebReadModel

import java.time.LocalDate

class MovieController( cc: ControllerComponents,
                       movieControllerService: MovieControllerService,
                       readModel: WebReadModel,
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
                       responseCache: EncodedResponseCache,
                       // The ONE country this deployment serves — which cities are
                       // ours (`withCity`) and which the sitemap advertises. Injected
                       // rather than read from the process configuration at each use so a spec
                       // can exercise a non-Polish host by passing one, instead of
                       // mutating the process-global env that parallel suites share.
                       servingCountry: models.Country,
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
                       // Each film page served — its client's user agent and the page's URL
                       // (`CityPath.filmUrl`) — for the record of the pages Facebook's crawler
                       // fetched (`FacebookFetchRecorder`); nowhere unless the composition root says.
                       filmPageServed: (Option[String], String) => Unit = (_, _) => (),
                       // The build serving the pages — in every validator, so two builds side by
                       // side in a rolling deploy never answer one another's revalidation with a 304.
                       build: settings.CommitSha = settings.CommitSha("unknown"),
                     ) extends AbstractController(cc) {

  private implicit val fragments: FilmCardFragments = filmCards

  // A city listing's JSON-LD, kept under what it is built from (origin, city and the
  // films it names): rebuilding its JSON tree for every film was ~1.3 MB a New York
  // render, and it changes only when the city's films do.
  private val cityStructuredData: com.github.benmanes.caffeine.cache.Cache[StructuredData.CityPageKey, String] =
    tools.BoundedCache.ofSize(512).build[StructuredData.CityPageKey, String]()

  // A city's JSON payloads, each film's object serialized once and kept.
  private[controllers] val jsonPayloads = new CityJsonPayloads

  // robots.txt and the sitemaps, cut from the live schedules and each city's own stamp.
  private val crawlerFiles = new CrawlerFiles(servingCountry, movieControllerService.toSchedules(_), city => readModel.lastModifiedFor(city.slug))

  /** How many listings' JSON-LD this controller is holding — for the spec. */
  private[controllers] def structuredDataHeld: Long = { cityStructuredData.cleanUp(); cityStructuredData.estimatedSize() }

  // The deployment's own, and ONLY, language. Every visitor gets this same
  // rendered `Messages` regardless of `Accept-Language`, cookie, or anything
  // else about the request — an explicit language pick swaps the visible
  // copy client-side instead (`shared.js`'s `applyLanguage`, fed by the
  // language pack `I18nPacks` embeds), so the server never has to branch per
  // visitor and every plain city URL stays edge-cacheable unconditionally
  // (see `cacheablePlainPage` below).
  //
  // BARE ("pl", not "pl-PL"), NOT `Lang(servingCountry.language)` directly —
  // `cc.messagesApi.preferred(...)` always normalises its result down to one
  // of the region-less codes actually registered in `play.i18n.langs`.
  private val deploymentDefaultLang: play.api.i18n.Lang = play.api.i18n.Lang(servingCountry.language.getLanguage)

  // The one `Messages` every render on this deployment uses — memoised, since it is
  // the one language and its fixed labels never change (`MemoisedMessages`).
  private val deploymentMessages: play.api.i18n.Messages = new MemoisedMessages(cc.messagesApi.preferred(Seq(deploymentDefaultLang)))

  // Validators, `Cache-Control`, the 304 short-circuit and the gzip blob for
  // every client-independent response this controller serves. The validator
  // starts from the city's own read-model stamp, or the model-wide one for a
  // payload with no city in it -- see `ConditionalResponse.validatorFor` for why that
  // distinction, and the city's calendar day, decide when a held copy retires.
  private val conditionalResponse = new ConditionalResponse(
    responseCache,
    modelStamp = city => city.fold(readModel.lastModified)(c => readModel.lastModifiedFor(c.slug)),
    now        = () => movieControllerService.now(),
    build      = build,
  )

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

  private val HtmlContentType = "text/html; charset=utf-8"

  /** `page` as a 200, its bytes written fragment by fragment (`ResponseBody.html`)
   *  rather than through Play's `Writeable[Html]`, which flattens the page into one
   *  `String` and then copies it into bytes: two thirds of what a Warsaw film page
   *  allocated, and most of a browse page. Same bytes, same content type. */
  private def okHtml(page: play.twirl.api.Html): Result = Ok(ResponseBody.html(page).plain).as(HtmlContentType)

  // Every city-scoped handler wraps its body in this so resolution + not-found
  // behaviour lives in one place — see `ServedCity` for the country scope. The
  // unknown-city 404 renders in `deploymentMessages` (see above), not a
  // per-request resolution — except on the `showtimes.cc` front door, which speaks
  // the brand's English ([[FrontDoorMessages]]); `ServedCity.resolve` picks which.
  private val frontDoorMessages: play.api.i18n.Messages = FrontDoorMessages(cc.messagesApi)
  private def withCity(slug: String, request: RequestHeader)(f: City => Result): Result =
    ServedCity.resolve(slug, request, servingCountry, deploymentMessages, frontDoorMessages)(f)

  // Persist the viewed city so the bare `/` landing can bounce a returning
  // visitor straight to it. Readable by JS (httpOnly = false) so the client can
  // also honour it; long-lived; scoped to the deployment's MOUNT POINT so it
  // rides every request of this country's site and none of a neighbour's — on
  // the shared brand domain a cookie at "/" would be sent to (and overwritten
  // by) `/de` and `/us`, bouncing a UK visitor's landing to a city that country
  // does not serve.
  private def cityCookie(city: City): Cookie =
    Cookie("city", city.slug, maxAge = Some(60 * 60 * 24 * 365), path = city.country.mountPath, httpOnly = false)

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
    ResponseBody.measured(ResponseBody.html(renderIndexHtml(city, request)), recordRender(services.metrics.WebRenderMetrics.Listing, _))
  }

  private def renderIndexHtml(city: City, request: RequestHeader)(implicit c: City, messages: play.api.i18n.Messages): play.twirl.api.Html = {
    // One clock for both the filtering and the page's own expiry countdown —
    // `_repertoireView` counts forward from `renderedAt`, so it has to be the
    // instant the schedules were actually pruned at.
    val now         = movieControllerService.nowIn(city)
    // Measured on its own as well as within the whole render, so the histogram says how
    // much of a listing's cost is building its schedules (`listing_schedules`).
    val schedules   = tools.ThreadAllocation.measure(recordRender(services.metrics.WebRenderMetrics.ListingSchedules, _))(movieControllerService.toSchedules(city, now))
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
      structuredData  = Some(cityStructuredData.get(StructuredData.CityPageKey(
        StructuredData.originOf(pageUrl, city), city, StructuredData.cityEntries(schedules)), StructuredData.cityPage)),
    )
  }

  def index(city: String): Action[AnyContent] = Action { request => withCity(city, request)(renderIndex(_, request)) }

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
    withCity(city, request) { c =>
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
    }
  }

  // robots.txt — see `CrawlerFiles.robots` for which shape a request wants. The share-card
  // images (`/share-cards/…`, and the old `/*/og-image` + `/*/movie/og-image` addresses that
  // redirect to them) are deliberately NOT disallowed — Facebook honours robots.txt when
  // fetching `og:image`, so blocking them would break every share preview.
  def robotsTxt: Action[AnyContent] = Action { request =>
    Ok(crawlerFiles.robots(PageMeta.host(request), PageMeta.origin(request))).as("text/plain; charset=utf-8")
  }

  /** `sitemap.xml` — the full crawl map, or an index of per-city ones for a large
   *  corpus; see [[CrawlerFiles.sitemap]]. Cached for an hour at the edge/browser;
   *  the corpus changes on the order of scrape cadence, not per request. */
  def sitemap: Action[AnyContent] = Action { request =>
    okSitemap(crawlerFiles.sitemap(PageMeta.host(request), PageMeta.origin(request)))
  }

  /** `/sitemap-root.xml` — the landing-only sitemap a partitioned country's index points at. */
  def sitemapRoot: Action[AnyContent] = Action { request =>
    okSitemap(crawlerFiles.sitemapRoot(PageMeta.origin(request)))
  }

  /** `/{city}/sitemap.xml` — one city's slice of the full crawl map, served
   *  only once a country's flat `sitemap.xml` has been partitioned. */
  def citySitemap(city: String): Action[AnyContent] = Action { request =>
    withCity(city, request)(c => okSitemap(crawlerFiles.citySitemap(PageMeta.origin(request), c)))
  }

  private def okSitemap(body: String): Result =
    Ok(body).as("application/xml; charset=utf-8").withHeaders("Cache-Control" -> "public, max-age=3600")

  /** Conditional-GET wrapper for the JSON API endpoints — the same mechanism as
   *  the HTML pages (see [[ConditionalResponse.serve]]): a current `If-Modified-Since`
   *  yields a bodiless 304 (what warm mobile clients hit), otherwise the payload
   *  is served from the shared gzip cache under `RevalidatedAnywhere`, so the
   *  edge may hold it too. Both the listing and the details payload track the
   *  same city's stamp, so a 304 on one is a 304 on the other. */
  private def conditionalJson(request: Request[AnyContent], city: City, cacheKey: String = "")(body: => ResponseBody): Result =
    conditionalResponse.serve(request, "application/json", CachePolicy.RevalidatedAnywhere,
                              cacheKey = cacheKey, city = Some(city))(body)

  /** Lean listing — everything the grid + filters need, no heavy detail text.
   *  Latency-sensitive; clients hit this on the critical path. */
  def apiRepertoire(city: String, days: Option[Int] = None): Action[AnyContent] = Action { request =>
    withCity(city, request) { c =>
      val window = CityJsonPayloads.dayWindow(days)
      conditionalJson(request, c, CityJsonPayloads.windowCacheKey(window)) {
        jsonPayloads.repertoire(c, today(c), movieControllerService.toSchedules(c), window)
      }
    }
  }

  /** Detail-only payload (synopsis + trailers), keyed by title. Clients fetch
   *  this in parallel with the listing and merge; keeping it off
   *  `/{city}/api/repertoire` halves the listing's gzip size. */
  def apiDetails(city: String): Action[AnyContent] = Action { request =>
    withCity(city, request) { c =>
      conditionalJson(request, c)(jsonPayloads.details(c, today(c), movieControllerService.toSchedules(c)))
    }
  }

  /** The city's cinema universe + area grouping (static). Mobile fetches this
   *  once per city to render the collapsible, per-area cinema filter — the
   *  counterpart of the server-side `CINEMA_AREAS` the web page is handed. */
  def apiCinemas(city: String): Action[AnyContent] = Action { request =>
    withCity(city, request)(c => conditionalJson(request, c)(CityJsonPayloads.cinemas(c, today(c))))
  }

  /** The city's own calendar day, off the injected clock. */
  private def today(city: City): LocalDate = movieControllerService.nowIn(city).toLocalDate

  /** `/{city}/film…` and `/{city}/filmy` — the pre-rename Polish spellings of
   *  the detail page and the browse facets, 301'd onto `/{city}/movie…` and
   *  `/{city}/movies`. Kept routable indefinitely for the same reason the
   *  `?title=` form is: search indexes, shared links and installed app builds
   *  all still carry the old address.
   *
   *  The sub-path form takes the whole remainder rather than a `:slug` because
   *  the rename moved `/film/og-image` too, and one wildcard covers both. */
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

  /** The canonical film page, addressed by slug. */
  def filmBySlug(city: String, slug: String): Action[AnyContent] = Action { request =>
    withCity(city, request) { implicit c =>
      movieControllerService.filmBySlug(c, slug) match {
        // A slug that reached the film but is not its address — a former billing-title
        // address, or a stale fold the fallback re-slugged — 301s onto the address, so a
        // shared link keeps working and crawlers consolidate on one URL per film.
        case Some(schedule) if schedule.slug.exists(_ != slug) =>
          val path = FilmHref.forSlug(schedule.slug, schedule.movie.title)
          MovedPermanently(if (request.rawQueryString.isEmpty) path else s"$path?${request.rawQueryString}")
        case Some(schedule) => renderFilm(schedule, request)
        case None           => NotFound(s"Film not found: $slug")
      }
    }
  }

  /** The pre-slug `?title=…` address. Kept routable indefinitely — it is what
   *  every link minted before the switch carries, including the ~10k URLs the
   *  old sitemap put in search indexes and the share links installed app builds
   *  still generate — but answered with a 301 so crawlers consolidate on the
   *  slug and users land on the canonical address. */
  def film(city: String, title: String): Action[AnyContent] = Action { request =>
    withCity(city, request) { implicit c =>
      movieControllerService.film(c, title) match {
        // A title with no usable slug has no other address to offer, so it
        // renders here rather than 301-ing to itself.
        case Some(schedule) if schedule.slug.isDefined =>
          MovedPermanently(FilmHref.forSlug(schedule.slug, schedule.movie.title))
        case Some(schedule) => renderFilm(schedule, request)
        case None           => NotFound(s"Film not found: $title")
      }
    }
  }

  private def renderFilm(schedule: FilmSchedule, request: Request[AnyContent])(implicit c: City): Result = {
    implicit val messages: play.api.i18n.Messages = deploymentMessages
    // `request.uri` would carry the raw inbound encoding; use the canonical
    // FilmHref form instead so the og:url matches the link the page exposes
    // elsewhere. Scheme/host come from PageMeta so the X-Forwarded-* workaround
    // (Play 3.0's `request.secure` ignores the `trustedProxies` knob on this Fly
    // setup) is in one place.
    val canonicalUrl = PageMeta.origin(request) + FilmHref.forSlug(schedule.slug, schedule.movie.title)
    val ogImageUrl   = shareCardUrl(schedule)
    // Sibling cities currently showing this same film — the cross-links a
    // near-duplicate per-city page needs so it isn't only reachable through
    // the sitemap. See [[MovieControllerService.citiesShowing]].
    val otherCities: Seq[(City, String)] =
      movieControllerService
        .citiesShowing(schedule.resolved._id, c, servingCountry, movieControllerService.nowIn(c))
        .map(sibling => sibling -> FilmHref.forSlug(schedule.slug, schedule.movie.title, sibling))
    // Nobody is rendered into this page either, so `no-cache` (revalidate, keep
    // the browser copy, bfcache works) replaces the `no-store` a signed-in render
    // used to need. It stops short of offering itself to a shared cache only
    // because a per-film edge entry wants its own validator analysis, not because
    // the bytes are anyone's.
    schedule.slug.flatMap(CityPath.filmUrl(c, _)).foreach(filmPageServed(request.headers.get(USER_AGENT), _))
    okHtml(views.html.film(schedule, canonicalUrl, FilmPreviewText.previewDescription(schedule), ogImageUrl, minifier, oauthProviders, otherCities, pageTags()))
      .withHeaders("Cache-Control" -> "private, no-cache")
      .withCookies(cityCookie(c))
  }

  /** The film's share card, or the city's while it has none — see [[ShareCardUrl]]. */
  private def shareCardUrl(schedule: FilmSchedule)(implicit c: City): String =
    ShareCardUrl.forFilm(schedule.resolved, c)

  /** `/:city/movie/og-image?title=…` — the film card's old, web-rendered address. Link previews
   *  already cached it, so it stays, as a temporary redirect to the image the page names now: the
   *  worker's share card, or the city's static card. Temporary because that target changes with
   *  every card version, and a film on the city card today gets its own later. The web decodes no image. */
  def ogImage(city: String, title: String): Action[AnyContent] = Action { request =>
    withCity(city, request) { c =>
      Found(movieControllerService.film(c, title).fold(ShareCardUrl.city(c))(shareCardUrl(_)(using c)))
    }
  }

  /** `/:city/og-image` — the city card's old, web-rendered address, kept because link previews
   *  cached it. Permanently redirected, unlike [[ogImage]]: the city card's address never moves. */
  def cityOgImage(city: String): Action[AnyContent] = Action { request =>
    withCity(city, request)(c => MovedPermanently(ShareCardUrl.city(c)))
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
