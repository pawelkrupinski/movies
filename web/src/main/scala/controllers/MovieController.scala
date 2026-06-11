package controllers

import models._
import play.api.Logging
import play.api.libs.json.{Json, Writes}
import play.api.mvc._
import play.api.Mode
import services.movies.{MovieCache, StoredMovieRecord, TitleNormalizer, TrailerEmbed}

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter

case class CinemaShowtimes(cinema: Cinema, showtimes: Seq[Showtime])

// ── JSON API types ──────────────────────────────────────────────────────
case class ApiShowtime(time: String, format: String, room: Option[String], bookingURL: Option[String])
case class ApiCinemaShowings(cinema: String, cinemaURL: Option[String], showtimes: Seq[ApiShowtime])
case class ApiDayShowings(date: String, label: String, cinemas: Seq[ApiCinemaShowings])
case class ApiRatings(
  imdb: Option[Double], imdbURL: Option[String],
  metascore: Option[Int], metacriticURL: Option[String],
  rottenTomatoes: Option[Int], rottenTomatoesURL: Option[String],
  filmweb: Option[Double], filmwebURL: Option[String]
)
case class ApiFilm(
  title: String, posterURL: Option[String], fallbackPosterURLs: Seq[String],
  runtimeMinutes: Option[Int], releaseYear: Option[Int], genres: Seq[String],
  ratings: ApiRatings,
  countries: Seq[String], directors: Seq[String], cast: Seq[String],
  showings: Seq[ApiDayShowings]
)

/** Detail-only payload for `GET /api/details`: the heavy text (synopsis) and
 *  trailers that the grid / filters never need. Split off the listing so the
 *  latency-sensitive `/api/repertoire` stays lean; clients fetch both in
 *  parallel and merge by `title`. Only films carrying a synopsis or at least
 *  one trailer are emitted. */
case class ApiFilmDetails(
  title: String, originalTitle: Option[String], synopsis: Option[String], trailerURLs: Seq[String]
)

object ApiFilmDetails {
  implicit val writes: Writes[ApiFilmDetails] = Json.writes[ApiFilmDetails]

  def from(fs: FilmSchedule): ApiFilmDetails = ApiFilmDetails(
    title         = fs.movie.title,
    // Only the genuinely-distinct original title (English/production-language
    // name when it differs from the Polish cinema title) — the redundancy
    // check lives on MovieRecord so clients can render it unconditionally.
    originalTitle = fs.enrichment.flatMap(_.distinctOriginalTitle(fs.movie.title)),
    synopsis      = fs.synopsis,
    // Same source + transform the /film detail page uses (raw trailer URLs →
    // embed URLs, deduped) so clients get a ready-to-embed Zwiastun set.
    trailerURLs   = fs.enrichment.toSeq.flatMap(_.trailerUrls).flatMap(TrailerEmbed.embedUrlFor).distinct,
  )

  def hasContent(d: ApiFilmDetails): Boolean =
    d.synopsis.nonEmpty || d.trailerURLs.nonEmpty || d.originalTitle.nonEmpty
}

object ApiFilm {
  implicit val apiShowtimeWrites: Writes[ApiShowtime] = Json.writes[ApiShowtime]
  implicit val apiCinemaShowingsWrites: Writes[ApiCinemaShowings] = Json.writes[ApiCinemaShowings]
  implicit val apiDayShowingsWrites: Writes[ApiDayShowings] = Json.writes[ApiDayShowings]
  implicit val apiRatingsWrites: Writes[ApiRatings] = Json.writes[ApiRatings]
  implicit val apiFilmWrites: Writes[ApiFilm] = Json.writes[ApiFilm]

  private val TimeFmt = DateTimeFormatter.ofPattern("HH:mm")

  def from(fs: FilmSchedule): ApiFilm = {
    val e = fs.enrichment
    val cinemaUrlMap = fs.cinemaFilmUrls.map { case (c, url) => c.displayName -> url }.toMap
    ApiFilm(
      title            = fs.movie.title,
      posterURL        = fs.posterUrl,
      fallbackPosterURLs = e.map(_.fallbackPosterUrls).getOrElse(Seq.empty),
      runtimeMinutes   = fs.movie.runtimeMinutes,
      releaseYear      = fs.movie.releaseYear,
      genres           = fs.movie.genres,
      ratings          = ApiRatings(
        imdb              = e.flatMap(_.imdbRating),
        imdbURL           = e.flatMap(_.imdbUrl),
        metascore         = e.flatMap(_.metascore),
        metacriticURL     = e.map(_.metacriticHref(fs.movie.title)),
        rottenTomatoes    = e.flatMap(_.rottenTomatoes),
        rottenTomatoesURL = e.map(_.rottenTomatoesHref(fs.movie.title)),
        filmweb           = e.flatMap(_.filmwebRating),
        filmwebURL        = e.map(_.filmwebHref(fs.movie.title))
      ),
      countries        = fs.movie.countries,
      directors        = fs.director,
      cast             = fs.cast,
      showings         = fs.showings.map { case (date, cinemas) =>
        ApiDayShowings(
          date    = date.toString,
          label   = DateFormatter.format(date),
          cinemas = cinemas.map { cs =>
            ApiCinemaShowings(
              cinema    = cs.cinema.displayName,
              cinemaURL = cinemaUrlMap.get(cs.cinema.displayName),
              showtimes = cs.showtimes.map { st =>
                ApiShowtime(
                  time       = st.dateTime.format(TimeFmt),
                  format     = st.format.mkString(" "),
                  room       = st.room,
                  bookingURL = st.bookingUrl
                )
              }
            )
          }
        )
      }
    )
  }
}

case class FilmSchedule(
                         movie: Movie,
                         posterUrl: Option[String],
                         synopsis: Option[String],
                         cast: Seq[String],
                         director: Seq[String],
                         cinemaFilmUrls: Seq[(Cinema, String)],
                         showings: Seq[(LocalDate, Seq[CinemaShowtimes])],
                         enrichment: Option[MovieRecord] = None
                       )

class MovieControllerService(
  cache: MovieCache
) {
  def debugData(): Seq[StoredMovieRecord] =
    cache.snapshot().sortBy(_.title.toLowerCase)

  def toSchedules(city: City): Seq[FilmSchedule] =
    toSchedules(city, LocalDateTime.now(city.zoneId))

  /** Overload with an injectable `now` so tests can pin the clock to a
   * fixture's capture date and assert what the / page would render at that
   * moment. Production callers should always use the city-only variant.
   *
   * Scoped to `city`: only this city's cinemas' showtimes count, so a film
   * playing only in another city's cinemas drops out of this city's listing.
   * The cache itself is global (one enriched row per film across all cities). */
  def toSchedules(city: City, now: LocalDateTime): Seq[FilmSchedule] = {
    val cityCinemas = city.cinemaSet
    cache.snapshot().flatMap { case StoredMovieRecord(cleanTitle, _, e) =>
      // Flatten this city's cinemas' future showtimes for this film. Records
      // with no future showings in this city drop out of its list view — they
      // stay in storage per the "keep forever" policy.
      val allShowtimes = e.cinemaData.toSeq
        .filter { case (cinema, _) => cityCinemas.contains(cinema) }
        .flatMap { case (cinema, slot) =>
          slot.showtimes.iterator.filter(_.dateTime.isAfter(now.minusMinutes(30))).map(st => (cinema, st))
        }
      if (allShowtimes.isEmpty) None
      else {
        val earliest = allShowtimes.map(_._2.dateTime).min
        val byDate: Seq[(LocalDate, Seq[CinemaShowtimes])] =
          allShowtimes
            .groupBy(_._2.dateTime.toLocalDate)
            .toSeq.sortBy(_._1)
            .map { case (date, slots) =>
              val perCinema = slots
                .groupBy(_._1)
                // `displayName` is the tiebreaker: when two cinemas share a film
                // at the same earliest showtime (common once a film merges across
                // venues), ordering by time alone leaves their relative order to
                // the upstream `Map` iteration order, which varies with scrape
                // thread-merge timing — non-deterministic across JVM boots and the
                // source of the "Kino Malta vs Kino Meduza" snapshot flake. The
                // total order makes the rendered schedule reproducible.
                .toSeq.sortBy { case (cinema, ss) => (ss.map(_._2.dateTime).min, cinema.displayName) }
                .map { case (cinema, ss) => CinemaShowtimes(cinema, ss.map(_._2).sortBy(_.dateTime)) }
              (date, perCinema)
            }
        val cinemaFilmUrls: Seq[(Cinema, String)] =
          e.cinemaData.toSeq
            .filter { case (cinema, _) => cityCinemas.contains(cinema) }
            .flatMap { case (cinema, slot) => slot.filmUrl.map(cinema -> _) }
            // Sort by display name: `cinemaData` is a small immutable Map
            // (Map1–4 for ≤4 cinemas) whose iteration is INSERTION order, so the
            // raw `.toSeq` order varies with scrape/merge order — the records
            // still compare equal (Map equality ignores order) but the rendered
            // booking-link list wouldn't. A total order makes it reproducible.
            .sortBy { case (cinema, _) => cinema.displayName }
        Some((earliest, FilmSchedule(
          movie = Movie(e.displayTitle(cleanTitle), e.runtimeMinutes, e.releaseYear, countries = e.countries, genres = e.genres),
          posterUrl = e.posterUrl,
          synopsis = e.synopsis,
          cast = e.cast,
          director = e.director,
          cinemaFilmUrls = cinemaFilmUrls,
          showings = byDate,
          enrichment = Some(e)
        )))
      }
    }.sortBy { case (earliest, fs) => (earliest, fs.movie.title) }.map(_._2)
  }

  def film(city: City, title: String): Option[FilmSchedule] = {
    val needle = normalizeTitle(title)
    toSchedules(city).find(s => normalizeTitle(s.movie.title) == needle)
  }

  private def normalizeTitle(title: String): String = TitleNormalizer.normalize(title)

  def rehydrate(): Int = cache.rehydrate()
}

class MovieController( cc: ControllerComponents,
                       movieControllerService: MovieControllerService,
                       movieCache: MovieCache,
                       userRepo: services.users.UserRepo,
                       oauthProviders: Set[String],
                       environment: Mode,
                       responseCache: GzippedResponseCache,
                       ogCardService: tools.OgCardService,
                     ) extends AbstractController(cc) with Logging {

  // Read the session's `userId` (set by `AuthController.callback`) and
  // resolve it to a User if the row is still there. Returns None for
  // anonymous browsers AND for the rare case where a previously
  // authenticated session's user row was deleted out of band — the
  // session is stale, treat it as logged out.
  private def currentUser(request: RequestHeader): Option[models.User] =
    request.session.get("userId").flatMap(userRepo.findById)

  private def acceptsGzip(request: RequestHeader): Boolean =
    request.headers.get("Accept-Encoding").exists(_.toLowerCase.contains("gzip"))

  // The plain HTML pages (`/{city}/`, `/{city}/filmy`) are byte-identical for
  // every anonymous visitor at a given cache version, so we serve a
  // pre-rendered, pre-gzipped blob keyed on the request path (which fully
  // determines the output: city and page type). Logged-in users (personalised
  // navbar) and filter queries (OG meta) must NOT hit the shared blob — they
  // render fresh. Non-gzip clients also bypass it, since the cache stores
  // compressed bytes.
  private def cacheablePlainPage(request: RequestHeader, user: Option[models.User]): Boolean =
    user.isEmpty && request.queryString.isEmpty && acceptsGzip(request)

  private def ifModifiedSinceCurrent(request: RequestHeader, lastMod: java.time.Instant): Boolean =
    request.headers.get("If-Modified-Since").exists { ims =>
      scala.util.Try(java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME.parse(ims))
        .map(java.time.Instant.from)
        .toOption
        .exists(!lastMod.isAfter(_))
    }

  /** Conditional-GET + gzip-cache for a response that is byte-identical for
   *  every client at the current [[MovieCache]] version. A client whose
   *  `If-Modified-Since` is still current gets a bodiless 304 — so a browser
   *  refresh re-validates cheaply and re-uses its cached copy instead of
   *  re-downloading the body. Otherwise the body is served, from the shared
   *  versioned, path-keyed gzip cache when the client accepts gzip (declaring
   *  `Content-Encoding: gzip` makes the GzipFilter pass it through rather than
   *  double-compress). `revalidate` adds `Cache-Control: private, no-cache` so
   *  the browser caches the page yet always re-validates before re-use — the
   *  pages change when showtimes do, so we never want a stale copy served
   *  without a check. */
  private def conditionalGzipped(request: RequestHeader, contentType: String, vary: String, revalidate: Boolean)(body: => String): Result = {
    val lastMod  = movieCache.lastModified.truncatedTo(java.time.temporal.ChronoUnit.SECONDS)
    val httpDate = java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME
      .format(lastMod.atOffset(java.time.ZoneOffset.UTC))
    val validators: Seq[(String, String)] =
      ("Last-Modified" -> httpDate) +: (if (revalidate) Seq("Cache-Control" -> "private, no-cache") else Nil)

    if (ifModifiedSinceCurrent(request, lastMod))
      NotModified.withHeaders(validators*)
    else if (acceptsGzip(request)) {
      val bytes = responseCache.gzippedBody(request.path, lastMod)(body)
      Ok(bytes).as(contentType)
        .withHeaders((Seq("Content-Encoding" -> "gzip", "Vary" -> vary) ++ validators)*)
    } else
      Ok(body).as(contentType).withHeaders((("Vary" -> vary) +: validators)*)
  }

  private val HtmlContentType = "text/html; charset=utf-8"
  private val HtmlVary        = "Accept-Encoding"

  // Resolve the `/{city}/…` slug; 404 on an unknown city. Every city-scoped
  // handler wraps its body in this so resolution + not-found behaviour lives
  // in one place.
  private def withCity(slug: String)(f: City => Result): Result =
    City.bySlug(slug) match {
      case Some(c) => f(c)
      case None    => NotFound(s"Nieznane miasto: $slug")
    }

  // Persist the viewed city so the bare `/` landing can bounce a returning
  // visitor straight to it. Readable by JS (httpOnly = false) so the client
  // can also honour it; long-lived; path "/" so it rides every request.
  private def cityCookie(city: City): Cookie =
    Cookie("city", city.slug, maxAge = Some(60 * 60 * 24 * 365), path = "/", httpOnly = false)

  // Render the main "Filmy" listing — repertoire view, full corpus,
  // OG meta derived from `?…` filter params. Shared between `/` and
  // `/filmy` (no params) so both URLs are interchangeable; `/filmy`
  // with one of the browse-axis params still routes through `browse`
  // below to the per-director / per-cast / per-country page.
  private def renderIndex(city: City, request: RequestHeader): Result = {
    implicit val c: City = city
    val user = currentUser(request)
    if (cacheablePlainPage(request, user)) {
      // 304 short-circuits before any work; on a 200 cache hit `renderIndexHtml`
      // (and its data-prep) never runs either.
      conditionalGzipped(request, HtmlContentType, HtmlVary, revalidate = true)(renderIndexHtml(city, request, user).body)
        .withCookies(cityCookie(city))
    } else {
      Ok(renderIndexHtml(city, request, user)).withCookies(cityCookie(city))
    }
  }

  private def renderIndexHtml(city: City, request: RequestHeader, user: Option[models.User])(implicit c: City): play.twirl.api.Html = {
    val schedules = movieControllerService.toSchedules(city)
    val meta = FilterDescription.forIndex(city, request.queryString, schedules)
    views.html.repertoire(
      schedules, city.cinemaDisplayNames, city.cinemaPillMap,
      devMode, user, oauthProviders,
      pageTitle       = meta.title,
      pageDescription = meta.description,
      pageUrl         = PageMeta.canonicalUrl(request),
      fbAppId         = PageMeta.fbAppId,
    )
  }

  def index(city: String): Action[AnyContent] = Action { request => withCity(city)(c => renderIndex(c, request)) }

  private def renderBrowse(city: City, heading: String, films: Seq[FilmSchedule], request: RequestHeader): Result = {
    implicit val c: City = city
    val user = currentUser(request)
    Ok(views.html.browse(
      films, heading, devMode, user, oauthProviders,
      pageUrl = PageMeta.canonicalUrl(request),
      fbAppId = PageMeta.fbAppId,
    )).withCookies(cityCookie(city))
  }

  def browse(city: String, kraj: Option[String], rezyser: Option[String], aktor: Option[String], gatunek: Option[String]): Action[AnyContent] = Action { request =>
    withCity(city) { c =>
      val all = movieControllerService.toSchedules(c)
      (kraj, rezyser, aktor, gatunek) match {
        case (Some(name), _, _, _) => renderBrowse(c, name, all.filter(_.movie.countries.contains(name)), request)
        case (_, Some(name), _, _) => renderBrowse(c, name, all.filter(_.director.contains(name)),        request)
        case (_, _, Some(name), _) => renderBrowse(c, name, all.filter(_.cast.contains(name)),            request)
        case (_, _, _, Some(name)) => renderBrowse(c, name, all.filter(_.movie.genres.contains(name)),    request)
        // `/{city}/filmy` with no filter axis is the main listing — same view
        // as `/{city}/`. The browse view only kicks in for the per-axis pages
        // reached from the meta-link rows on /film.
        case _                     => renderIndex(c, request)
      }
    }
  }

  // Permissive robots.txt — link-preview scrapers (Facebook's
  // `facebookexternalhit` in particular) treat a 404 here as "site not
  // crawlable" and surface that as a generic 403 to the debugger UI. Serving
  // an explicit allow-all unblocks their preview fetch. No private endpoints
  // to gate.
  def robotsTxt: Action[AnyContent] = Action {
    Ok("User-agent: *\nAllow: /\n").as("text/plain; charset=utf-8")
  }

  /** Conditional-GET wrapper for the JSON API endpoints — the same mechanism as
   *  the HTML pages (see [[conditionalGzipped]]): a current `If-Modified-Since`
   *  yields a bodiless 304 (what warm mobile clients hit), otherwise the payload
   *  is served from the shared gzip cache. The endpoints don't set
   *  `Cache-Control` (mobile manages its own revalidation), so `revalidate` is
   *  off. Both the listing and the details payload track the same cache mtime,
   *  so a 304 on one is a 304 on the other. */
  private def conditionalJson(request: Request[AnyContent])(body: => play.api.libs.json.JsValue): Result =
    conditionalGzipped(request, "application/json", vary = "Accept-Encoding", revalidate = false)(
      play.api.libs.json.Json.stringify(body)
    )

  /** Lean listing — everything the grid + filters need, no heavy detail text.
   *  Latency-sensitive; clients hit this on the critical path. */
  def apiRepertoire(city: String): Action[AnyContent] = Action { request =>
    withCity(city)(c => conditionalJson(request)(Json.toJson(movieControllerService.toSchedules(c).map(ApiFilm.from))))
  }

  /** Detail-only payload (synopsis + trailers), keyed by title. Clients fetch
   *  this in parallel with the listing and merge; keeping it off
   *  `/{city}/api/repertoire` halves the listing's gzip size. */
  def apiDetails(city: String): Action[AnyContent] = Action { request =>
    withCity(city) { c =>
      conditionalJson(request) {
        val details = movieControllerService.toSchedules(c)
          .map(ApiFilmDetails.from)
          .filter(ApiFilmDetails.hasContent)
        Json.toJson(details)
      }
    }
  }

  def debug(city: String): Action[AnyContent] = Action {
    withCity(city) { implicit c =>
      devOnly {
        Ok(views.html.debug(movieControllerService.debugData()))
      }
    }
  }

  /** Dev-only visual-tuning page. Renders the real `_movieCard` partial(s)
   *  inside a `.tune-scope` wrapper plus a slider panel that drives the CSS
   *  custom properties the production card styles read. Self-contained: the
   *  sample films are built in-process so the page works regardless of cache
   *  state. */
  def tune(city: String): Action[AnyContent] = Action {
    withCity(city) { implicit c =>
      devOnly {
        Ok(views.html.tune(MovieController.tuneSampleFilms))
      }
    }
  }

  /** Dev-only tuning page for the film-detail view — live sliders over the real
   *  `_filmDetailContent` for the title / meta / Seanse typography. */
  def tuneFilm(city: String): Action[AnyContent] = Action {
    withCity(city) { implicit c =>
      devOnly {
        Ok(views.html.tuneFilm(MovieController.tuneSampleFilm))
      }
    }
  }

  def film(city: String, title: String): Action[AnyContent] = Action { request =>
    withCity(city) { implicit c =>
      movieControllerService.film(c, title) match {
        case Some(schedule) =>
          // `request.uri` would carry the raw inbound title-encoding; use the
          // canonical FilmHref form instead so the og:url matches the link the
          // page exposes elsewhere. Scheme/host come from PageMeta so the
          // X-Forwarded-* workaround (Play 3.0's `request.secure` ignores the
          // `trustedProxies` knob on this Fly setup) is in one place.
          val canonicalUrl = PageMeta.origin(request) + FilmHref(schedule.movie.title)
          val ogImageUrl   = PageMeta.origin(request) + FilmHref.ogImage(schedule.movie.title)
          val user = currentUser(request)
          Ok(views.html.film(schedule, canonicalUrl, MovieController.previewDescription(schedule), ogImageUrl, devMode, user, oauthProviders))
            .withCookies(cityCookie(c))
        case None => NotFound(s"Film not found: $title")
      }
    }
  }

  /** The 1200×630 Open Graph share card (PNG) for a film — what `og:image` /
   *  `twitter:image` on the film page point at. Composited server-side
   *  ([[tools.OgCardService]]) so the full poster + title + rating badges sit
   *  inside one landscape image that the preview UIs can't crop the poster out
   *  of. Cached a day at the edge (the card only changes when ratings / poster
   *  do, and `OgCardService` memoises the bytes per those inputs). */
  def ogImage(city: String, title: String): Action[AnyContent] = Action {
    withCity(city) { c =>
      movieControllerService.film(c, title) match {
        case Some(schedule) =>
          val bytes = ogCardService.card(
            schedule.movie.title,
            MovieController.cardSubtitle(schedule),
            MovieController.ratingTokens(schedule),
            schedule.posterUrl
          )
          Ok(bytes).as("image/png").withHeaders("Cache-Control" -> "public, max-age=86400")
        case None => NotFound(s"Film not found: $title")
      }
    }
  }

  /** Drop the in-memory positive cache and reload it from Mongo. Available in
   * every mode (unlike the rest of the debug endpoints) so a fly.io instance
   * whose cache drifted from Mongo can be reconciled without a redeploy.
   * The negative cache (24h TTL TMDB-miss markers) is left alone. */
  def rehydrate(city: String): Action[AnyContent] = Action {
    withCity(city) { _ =>
      val count = movieControllerService.rehydrate()
      Ok(s"rehydrated $count rows\n").as("text/plain; charset=utf-8")
    }
  }


  // All /debug/* endpoints return 404 in production so the cache contents
  // aren't exposed on a deployed instance. Mode defaults to Dev in
  // `AppLoader` unless APP_MODE=prod is set explicitly.
  private def devOnly(result: => play.api.mvc.Result): play.api.mvc.Result =
    if (environment == Mode.Prod) NotFound("dev-only endpoint") else result

  // Same flag the regular navbar uses to gate the Debug tab.
  private def devMode: Boolean = environment != Mode.Prod
}

object MovieController {

  /** Deterministic sample cards for the `/debug/tune` page — built in process
   *  so the tuning page renders the real `_movieCard` partial without depending
   *  on live cache contents. The set is a deliberate spread of edge cases so
   *  every pill row, rating variant, and vertical gap is on screen at once:
   *
   *   1. `rich`        — all four ratings (RT fresh), two cinemas, two days.
   *   2. `manyTimes`   — long wrapping title + 3 genres, one cinema with many
   *                      showtimes whose format tokens all differ, so the pills
   *                      wrap across several rows with wide format badges.
   *   3. `rotten`      — RT below 60 (the `.rotten` red variant) + a low
   *                      single-digit IMDb, so the rotten styling and the
   *                      narrowest rating values show.
   *   4. `extremes`    — the widest possible values: IMDb 10.0, Metacritic 100,
   *                      RT 100%, Filmweb 10.0 — stress-tests pill width.
   *   5. `metaOnly`    — only the Metacritic bare-number pill, alone on its row.
   *   6. `noRatings`   — no enrichment at all, so the ratings row is absent and
   *                      the meta→date gap collapses to just the title gap.
   *   7. `seniorClub`  — a programme-prefixed long title (the separate-row case)
   *                      with a single no-booking showtime (the `<span>` badge
   *                      variant, not the `<a>` one).
   *   8. `sparse`      — one rating, one cinema, one showtime: the loosest case.
   */
  private[controllers] def tuneSampleFilms: Seq[FilmSchedule] = {
    val base = LocalDate.of(2026, 6, 4)
    def at(d: LocalDate, h: Int, m: Int): LocalDateTime = d.atTime(h, m)

    def slot(d: LocalDate, h: Int, m: Int, fmt: List[String], booking: Boolean = true): Showtime =
      Showtime(
        at(d, h, m),
        bookingUrl = if (booking) Some("https://example.test/book") else None,
        room       = Some("Sala 1"),
        format     = fmt
      )

    def film(
      title:      String,
      genres:     Seq[String],
      runtime:    Option[Int],
      year:       Option[Int],
      enrichment: Option[MovieRecord],
      showings:   Seq[(LocalDate, Seq[CinemaShowtimes])]
    ): FilmSchedule =
      FilmSchedule(
        movie          = Movie(title, runtimeMinutes = runtime, releaseYear = year, genres = genres),
        posterUrl      = None,
        synopsis       = None,
        cast           = Seq.empty,
        director       = Seq.empty,
        cinemaFilmUrls = Seq.empty,
        showings       = showings,
        enrichment     = enrichment
      )

    val rich = film(
      "Incepcja", Seq("Sci-Fi", "Akcja"), Some(148), Some(2010),
      Some(MovieRecord(imdbId = Some("tt1375666"), imdbRating = Some(8.8), metascore = Some(74), rottenTomatoes = Some(87), filmwebRating = Some(7.6))),
      Seq(
        base -> Seq(
          CinemaShowtimes(Multikino, Seq(slot(base, 17, 30, List("2D", "NAP")), slot(base, 20, 15, List("2D")))),
          CinemaShowtimes(Helios,    Seq(slot(base, 18, 0, List("IMAX", "2D"))))
        ),
        base.plusDays(1) -> Seq(
          CinemaShowtimes(Multikino, Seq(slot(base.plusDays(1), 19, 45, List("2D", "DUB"))))
        )
      )
    )

    // One cinema, eight showtimes, every slot a different format token set so
    // none is stripped as "common" — the badges wrap to several rows and the
    // wide tokens (4DX, VOSE, ATMOS) stress the pill's max width.
    val manyTimes = film(
      "Spider-Man: Poprzez multiwersum (wersja rozszerzona)", Seq("Animacja", "Akcja", "Przygodowy"), Some(140), Some(2023),
      Some(MovieRecord(imdbId = Some("tt9362722"), imdbRating = Some(8.6), metascore = Some(86), rottenTomatoes = Some(95), filmwebRating = Some(7.9))),
      Seq(base -> Seq(CinemaShowtimes(CinemaCityKinepolis, Seq(
        slot(base, 10, 0,  List("2D", "DUB")),
        slot(base, 12, 30, List("3D", "DUB")),
        slot(base, 14, 15, List("IMAX", "NAP")),
        slot(base, 16, 0,  List("4DX")),
        slot(base, 18, 20, List("VOSE")),
        slot(base, 20, 0,  List("ATMOS", "NAP")),
        slot(base, 21, 30, List("2D", "NAP", "ATMOS")),
        slot(base, 23, 0,  List("3D"))
      ))))
    )

    val rotten = film(
      "Morbius", Seq("Akcja", "Horror"), Some(104), Some(2022),
      Some(MovieRecord(imdbId = Some("tt5108870"), imdbRating = Some(4.3), metascore = Some(35), rottenTomatoes = Some(15), filmwebRating = Some(4.1))),
      Seq(base -> Seq(CinemaShowtimes(Helios, Seq(slot(base, 19, 0, List("2D", "NAP"))))))
    )

    val extremes = film(
      "Ojciec chrzestny", Seq("Dramat", "Kryminał"), Some(175), Some(1972),
      Some(MovieRecord(imdbId = Some("tt0068646"), imdbRating = Some(10.0), metascore = Some(100), rottenTomatoes = Some(100), filmwebRating = Some(10.0))),
      Seq(base -> Seq(CinemaShowtimes(KinoPalacowe, Seq(slot(base, 16, 45, List("2D", "NAP"))))))
    )

    val metaOnly = film(
      "Aftersun", Seq("Dramat"), Some(102), Some(2022),
      Some(MovieRecord(metascore = Some(95))),
      Seq(base -> Seq(CinemaShowtimes(KinoMuza, Seq(slot(base, 20, 30, List("NAP"))))))
    )

    val noRatings = film(
      "Pokaz przedpremierowy: Niezatytułowany film", Seq("Dramat"), None, Some(2026),
      None,
      Seq(base -> Seq(CinemaShowtimes(Rialto, Seq(slot(base, 18, 15, List("NAP"))))))
    )

    val seniorClub = film(
      "Kino Seniora: Niebo nad Berlinem", Seq("Dramat", "Fantasy"), Some(128), Some(1987),
      Some(MovieRecord(imdbId = Some("tt0093191"), imdbRating = Some(8.0), filmwebRating = Some(7.8))),
      Seq(base -> Seq(CinemaShowtimes(KinoApollo, Seq(slot(base, 12, 0, List("NAP"), booking = false)))))
    )

    val sparse = film(
      "Cicha noc", Seq("Dramat"), Some(98), Some(2017),
      Some(MovieRecord(filmwebRating = Some(7.1))),
      Seq(base -> Seq(CinemaShowtimes(KinoMuza, Seq(slot(base, 21, 0, List("2D"))))))
    )

    Seq(rich, manyTimes, rotten, extremes, metaOnly, noRatings, seniorClub, sparse)
  }

  /** One fully-populated film (synopsis + cast + director, which the listing
   *  samples leave empty) for the `/debug/tune/film` page, so every meta block
   *  renders and its fonts are tunable. Built off the rich sample's ratings +
   *  multi-cinema showings tree. */
  private[controllers] def tuneSampleFilm: FilmSchedule =
    tuneSampleFilms.head.copy(
      synopsis       = Some(
        "Dom Cobb to wytrawny złodziej, najlepszy w niebezpiecznej sztuce ekstrakcji — " +
        "wykradania cennych sekretów z głębi podświadomości podczas snu. Tym razem dostaje " +
        "szansę na odkupienie: zadanie odwrotne, zaszczepienie idei zamiast jej kradzieży. " +
        "Tekst celowo długi, by dało się dostroić rozmiar i odstępy opisu na ekranie filmu."
      ),
      cast           = Seq("Leonardo DiCaprio", "Joseph Gordon-Levitt", "Elliot Page", "Tom Hardy", "Ken Watanabe"),
      director       = Seq("Christopher Nolan"),
      cinemaFilmUrls = Seq(Multikino -> "https://example.test/incepcja")
    )

  /** Build the `og:description` / `twitter:description` text for the film
   * page. Format: rating summary ("IMDb 8.7 · RT 86% · Metacritic 79 ·
   * Filmweb 7.5") prefixed to the synopsis, truncated to keep WhatsApp /
   * Messenger / Telegram previews readable. Skips ratings that aren't set;
   * the whole string may be empty for films with no enrichment + no
   * synopsis. */
  private[controllers] def previewDescription(film: FilmSchedule): String = {
    val ratings  = ratingTokens(film).mkString(" · ")
    val synopsis = film.synopsis.getOrElse("").trim
    val joined =
      if (ratings.nonEmpty && synopsis.nonEmpty) ratings + " — " + synopsis
      else if (ratings.nonEmpty) ratings
      else synopsis
    // 300 chars is the practical cap most preview UIs render before
    // truncating; we add an ellipsis to make truncation visible.
    if (joined.length > 300) joined.take(297) + "…" else joined
  }

  /** The rating summary as individual tokens ("IMDb 8.8", "RT 87%", …),
   *  skipping sources that aren't set. Shared by the text `og:description`
   *  ([[previewDescription]], joined with " · ") and the OG-card image
   *  ([[tools.OgCardRenderer]], one pill per token). */
  private[controllers] def ratingTokens(film: FilmSchedule): Seq[String] = Seq(
    film.enrichment.flatMap(_.imdbRating).map(r => f"IMDb $r%.1f"),
    film.enrichment.flatMap(_.rottenTomatoes).map(s => s"RT $s%"),
    film.enrichment.flatMap(_.metascore).map(s => s"Metacritic $s"),
    film.enrichment.flatMap(_.filmwebRating).map(r => f"Filmweb $r%.1f")
  ).flatten

  /** The "2026 · Dramat, Kryminał" line under the title on the OG card —
   *  release year then genres, each part omitted when absent. */
  private[controllers] def cardSubtitle(film: FilmSchedule): String =
    (film.movie.releaseYear.map(_.toString).toSeq ++ Seq(film.movie.genres.mkString(", ")).filter(_.nonEmpty))
      .mkString(" · ")
}
