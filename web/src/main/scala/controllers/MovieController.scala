package controllers

import models._
import play.api.Logging
import play.api.libs.json.{Json, Writes}
import play.api.mvc._
import play.api.Mode
import services.movies.{MovieRepo, TitleNormalizer}
import services.readmodel.WebReadModel

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
    // The genuinely-distinct original title and the embed-ready trailer URLs are
    // pre-resolved on the read-model doc (the redundancy check + URL transform
    // ran at projection time), so clients render them unconditionally.
    originalTitle = fs.resolved.originalTitle,
    synopsis      = fs.synopsis,
    trailerURLs   = fs.resolved.trailerUrls,
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
    val r = fs.resolved
    val cinemaUrlMap = fs.cinemaFilmUrls.map { case (c, url) => c.displayName -> url }.toMap
    ApiFilm(
      title            = fs.movie.title,
      posterURL        = fs.posterUrl,
      fallbackPosterURLs = r.fallbackPosterUrls,
      runtimeMinutes   = fs.movie.runtimeMinutes,
      releaseYear      = fs.movie.releaseYear,
      genres           = fs.movie.genres,
      ratings          = ApiRatings(
        imdb              = r.ratings.imdb,
        imdbURL           = r.ratings.imdbUrl,
        metascore         = r.ratings.metascore,
        metacriticURL     = Some(r.ratings.metacriticUrl),
        rottenTomatoes    = r.ratings.rottenTomatoes,
        rottenTomatoesURL = Some(r.ratings.rottenTomatoesUrl),
        filmweb           = r.ratings.filmweb,
        filmwebURL        = Some(r.ratings.filmwebUrl)
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
                         // The fully-resolved metadata doc this schedule was built from —
                         // ratings, poster fallbacks, original title, trailers. Replaces the
                         // old `Option[MovieRecord]`: the web no longer holds MovieRecords.
                         resolved: ResolvedMovie
                       )

/**
 * Builds the per-city [[FilmSchedule]] view from the denormalised read model:
 * this city's [[CityScreening]] docs joined to their [[ResolvedMovie]]. The web
 * never touches the `movies` collection or a MovieRecord — the merge already
 * happened at projection time.
 */
class MovieControllerService(readModel: WebReadModel) {

  def toSchedules(city: City): Seq[FilmSchedule] =
    toSchedules(city, LocalDateTime.now(city.zoneId))

  /** Overload with an injectable `now` so tests can pin the clock to a fixture's
   * capture date. Scoped to `city`: `readModel.screeningsForCity` already
   * returns only this city's cinemas' screenings, so a film playing only
   * elsewhere drops out here.
   *
   * Ordering-tolerant join: a screening doc whose `ResolvedMovie` hasn't landed
   * yet (the movie-before-screenings write order can still be observed in the
   * reverse order over two independent change streams) simply contributes
   * nothing until the movie doc arrives — no half-rendered card. */
  def toSchedules(city: City, now: LocalDateTime): Seq[FilmSchedule] = {
    readModel.screeningsForCity(city.slug).groupBy(_.filmId).toSeq.flatMap { case (filmId, screenings) =>
      readModel.movie(filmId).flatMap { resolved =>
        // Flatten this city's future showtimes. A film with no future showing in
        // this city drops out of its list view (its docs stay in the store).
        val allShowtimes: Seq[(Cinema, Showtime)] = screenings.flatMap { sc =>
          MovieControllerService.cinemaByName(sc.cinema).toSeq.flatMap { cinema =>
            sc.showtimes.iterator.filter(_.dateTime.isAfter(now.minusMinutes(30))).map(st => (cinema, st))
          }
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
                  // `displayName` is the tiebreaker so two cinemas sharing a film at
                  // the same earliest showtime render in a stable order (the
                  // "Kino Malta vs Kino Meduza" snapshot-flake fix).
                  .toSeq.sortBy { case (cinema, ss) => (ss.map(_._2.dateTime).min, cinema.displayName) }
                  .map { case (cinema, ss) => CinemaShowtimes(cinema, ss.map(_._2).sortBy(_.dateTime)) }
                (date, perCinema)
              }
          val cinemaFilmUrls: Seq[(Cinema, String)] =
            screenings
              .flatMap(sc => MovieControllerService.cinemaByName(sc.cinema).flatMap(c => sc.filmUrl.map(c -> _)))
              .sortBy(_._1.displayName)
          Some((earliest, FilmSchedule(
            movie = Movie(resolved.title, resolved.runtimeMinutes, resolved.releaseYear, countries = resolved.countries, genres = resolved.genres),
            posterUrl = resolved.posterUrl,
            synopsis = resolved.synopsis,
            cast = resolved.cast,
            director = resolved.directors,
            cinemaFilmUrls = cinemaFilmUrls,
            showings = byDate,
            resolved = resolved
          )))
        }
      }
    }.sortBy { case (earliest, fs) => (earliest, fs.movie.title) }.map(_._2)
  }

  def film(city: City, title: String): Option[FilmSchedule] = {
    val needle = normalizeTitle(title)
    toSchedules(city).find(s => normalizeTitle(s.movie.title) == needle)
  }

  private def normalizeTitle(title: String): String = TitleNormalizer.normalize(title)
}

object MovieControllerService {
  /** displayName → Cinema (cinemas are `Source`s, so reuse the shared map). */
  private def cinemaByName(name: String): Option[Cinema] =
    Source.byDisplayName.get(name).collect { case c: Cinema => c }
}

class MovieController( cc: ControllerComponents,
                       movieControllerService: MovieControllerService,
                       readModel: WebReadModel,
                       // Read-only, on-demand only: the /debug corpus dump. The web
                       // never keeps this warm (no `movies` change stream) — it pulls
                       // a fresh snapshot when the dev endpoint is hit.
                       movieRepo: MovieRepo,
                       // Enqueue target for the dev-only /debug row "re-enrich"
                       // button — the same durable queue the worker drains.
                       taskQueue: services.tasks.TaskQueue,
                       userRepo: services.users.UserRepo,
                       // Gate for the state-mutating /…/debug/rehydrate trigger
                       // (the other /debug pages are dev-only; rehydrate runs in
                       // every mode, so it needs the admin gate instead).
                       adminAction: AdminAction,
                       oauthProviders: Set[String],
                       environment: Mode,
                       responseCache: GzippedResponseCache,
                       ogCardService: tools.OgCardService,
                       // `cinema displayName -> public source-page URL`, the same
                       // links /uptime shows, sourced from the UptimeMonitor tag
                       // snapshot. Evaluated per request so it tracks live retags;
                       // used only by the /debug table to link cinema names.
                       cinemaSourceUrls: () => Map[String, String] = () => Map.empty,
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
    val lastMod  = readModel.lastModified.truncatedTo(java.time.temporal.ChronoUnit.SECONDS)
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

  def debug(): Action[AnyContent] = Action {
    devOnly {
      // The debug table is the global corpus; the only thing the view needs a
      // city for is the /film fallback link on a row with no live showtimes
      // anywhere — give it the default city for that edge case.
      implicit val c: City = City.all.head
      // Pulled on demand from Mongo: the web doesn't keep the `movies` model
      // warm, so the corpus dump reads the source rows the read model is
      // projected from directly.
      Ok(views.html.debug(movieRepo.findAll().sortBy(_.title.toLowerCase), cinemaSourceUrls()))
    }
  }

  /** Dev-only: the active tasks in the durable queue (oldest-first), so the
   *  /debug "pending work" sections can show, per movie, whether an enrichment
   *  task already exists and its place in the queue. The page polls this; it's a
   *  bounded, index-backed `monitor` read (the same one `/tasks/data` serves),
   *  so the cost scales with viewers-while-open, not queue churn. Only the
   *  fields the page matches on are shipped — type, dedup key, state; submission
   *  order is already encoded by the oldest-first list position. */
  def debugQueue(): Action[AnyContent] = Action {
    devOnly {
      val snap = taskQueue.monitor(MovieController.DebugQueueActiveLimit)
      Ok(play.api.libs.json.Json.obj(
        "active" -> snap.active.map { t =>
          play.api.libs.json.Json.obj(
            "taskType" -> t.taskType,
            "dedupKey" -> t.dedupKey,
            "state"    -> t.state
          )
        }
      ))
    }
  }

  /** Dev-only: dump the warm read cache the web actually serves from — the
   *  `WebReadModel`'s in-memory `web_movies` + `web_screenings` views — so you
   *  can see exactly what a request would resolve against (vs `/debug`, which
   *  pulls the source `movies` corpus from Mongo on demand). */
  def debugReadModel(): Action[AnyContent] = Action {
    devOnly {
      implicit val c: City = City.all.head
      val movies     = readModel.allMovies().sortBy(_.title.toLowerCase)
      val screenings = readModel.allScreenings().groupBy(_.filmId)
      Ok(views.html.debugReadModel(movies, screenings, readModel.lastModified))
    }
  }

  /** Dev-only: force a TMDB re-enrich of one film from the /debug row button.
   *  Enqueues a `ResolveTmdb` task the worker's `ResolveTmdbHandler` consumes;
   *  that re-resolves the row and publishes `TmdbResolved`, so every downstream
   *  rating refresher re-runs off the existing event chain — the "followed by
   *  all the other enrichments" hook. Idempotent per (title, year): a repeat
   *  click while one is queued returns `duplicate`. Returns JSON for the page's
   *  fetch. */
  def reenrich(title: String, year: Option[Int]): Action[AnyContent] = Action {
    devOnly {
      if (title.isEmpty) BadRequest(play.api.libs.json.Json.obj("error" -> "missing title"))
      else {
        val result = taskQueue.enqueue(
          services.tasks.TaskType.ResolveTmdb,
          services.tasks.EnrichTaskKeys.resolveTmdbDedup(title, year),
          services.tasks.EnrichTaskKeys.moviePayload(title, year)
        )
        Ok(play.api.libs.json.Json.obj(
          "title"     -> title,
          "year"      -> year,
          "enqueued"  -> (result == services.tasks.EnqueueResult.Added),
          "duplicate" -> (result == services.tasks.EnqueueResult.Duplicate)
        ))
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
            MovieController.cardRatingBadges(schedule),
            schedule.posterUrl
          )
          Ok(bytes).as("image/png").withHeaders("Cache-Control" -> "public, max-age=86400")
        case None => NotFound(s"Film not found: $title")
      }
    }
  }

  /** Reload the in-memory read-model caches from Mongo. Available in every mode
   * (unlike the rest of the debug endpoints, which are dev-only) so a fly.io
   * instance whose caches drifted from the derived collections can be reconciled
   * without a redeploy — but since it runs in prod and mutates state, it's gated
   * by [[AdminAction]] (login session + ADMIN_ALLOWLIST) rather than left open. */
  def rehydrate(city: String): Action[AnyContent] = adminAction {
    withCity(city) { _ =>
      val count = readModel.reload()
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

  /** Cap on the active tasks `/debug/queue` returns per poll — high enough to
   *  cover a backed-up enrichment queue so a pending movie's place is still
   *  resolvable, without an unbounded scan. */
  private val DebugQueueActiveLimit = 1000

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

    // Build a resolved-movie sample directly (the web no longer holds
    // MovieRecords). Rating hrefs are placeholders — this page tunes layout, not
    // links — and `weightedRating` uses the production formula so the grid's
    // data-rating sort behaves as in prod.
    def res(
      title:     String,
      genres:    Seq[String],
      runtime:   Option[Int],
      year:      Option[Int],
      imdb:      Option[Double] = None,
      metascore: Option[Int]    = None,
      rt:        Option[Int]    = None,
      filmweb:   Option[Double] = None
    ): ResolvedMovie = {
      val weighted = {
        val ns = Seq(imdb, filmweb, metascore.map(_ / 10.0), rt.map(_ / 10.0)).flatten
        if (ns.isEmpty) 0.0 else ns.sum / ns.size
      }
      ResolvedMovie(
        _id = title, title = title, originalTitle = None, posterUrl = None, fallbackPosterUrls = Seq.empty,
        runtimeMinutes = runtime, releaseYear = year, genres = genres, countries = Seq.empty,
        directors = Seq.empty, cast = Seq.empty, synopsis = None, trailerUrls = Seq.empty,
        ratings = ResolvedRatings(
          imdb = imdb, imdbUrl = imdb.map(_ => "https://www.imdb.com/"),
          metascore = metascore, metacriticUrl = "https://www.metacritic.com/",
          rottenTomatoes = rt, rottenTomatoesUrl = "https://www.rottentomatoes.com/",
          filmweb = filmweb, filmwebUrl = "https://www.filmweb.pl/"
        ),
        weightedRating = weighted
      )
    }

    def film(resolved: ResolvedMovie, showings: Seq[(LocalDate, Seq[CinemaShowtimes])]): FilmSchedule =
      FilmSchedule(
        movie          = Movie(resolved.title, runtimeMinutes = resolved.runtimeMinutes, releaseYear = resolved.releaseYear, genres = resolved.genres),
        posterUrl      = resolved.posterUrl,
        synopsis       = resolved.synopsis,
        cast           = resolved.cast,
        director       = resolved.directors,
        cinemaFilmUrls = Seq.empty,
        showings       = showings,
        resolved       = resolved
      )

    val rich = film(
      res("Incepcja", Seq("Sci-Fi", "Akcja"), Some(148), Some(2010), imdb = Some(8.8), metascore = Some(74), rt = Some(87), filmweb = Some(7.6)),
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
      res("Spider-Man: Poprzez multiwersum (wersja rozszerzona)", Seq("Animacja", "Akcja", "Przygodowy"), Some(140), Some(2023), imdb = Some(8.6), metascore = Some(86), rt = Some(95), filmweb = Some(7.9)),
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
      res("Morbius", Seq("Akcja", "Horror"), Some(104), Some(2022), imdb = Some(4.3), metascore = Some(35), rt = Some(15), filmweb = Some(4.1)),
      Seq(base -> Seq(CinemaShowtimes(Helios, Seq(slot(base, 19, 0, List("2D", "NAP"))))))
    )

    val extremes = film(
      res("Ojciec chrzestny", Seq("Dramat", "Kryminał"), Some(175), Some(1972), imdb = Some(10.0), metascore = Some(100), rt = Some(100), filmweb = Some(10.0)),
      Seq(base -> Seq(CinemaShowtimes(KinoPalacowe, Seq(slot(base, 16, 45, List("2D", "NAP"))))))
    )

    val metaOnly = film(
      res("Aftersun", Seq("Dramat"), Some(102), Some(2022), metascore = Some(95)),
      Seq(base -> Seq(CinemaShowtimes(KinoMuza, Seq(slot(base, 20, 30, List("NAP"))))))
    )

    val noRatings = film(
      res("Pokaz przedpremierowy: Niezatytułowany film", Seq("Dramat"), None, Some(2026)),
      Seq(base -> Seq(CinemaShowtimes(Rialto, Seq(slot(base, 18, 15, List("NAP"))))))
    )

    val seniorClub = film(
      res("Kino Seniora: Niebo nad Berlinem", Seq("Dramat", "Fantasy"), Some(128), Some(1987), imdb = Some(8.0), filmweb = Some(7.8)),
      Seq(base -> Seq(CinemaShowtimes(KinoApollo, Seq(slot(base, 12, 0, List("NAP"), booking = false)))))
    )

    val sparse = film(
      res("Cicha noc", Seq("Dramat"), Some(98), Some(2017), filmweb = Some(7.1)),
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
   *  skipping sources that aren't set. Feeds the text `og:description`
   *  ([[previewDescription]], joined with " · "). */
  private[controllers] def ratingTokens(film: FilmSchedule): Seq[String] = {
    val r = film.resolved.ratings
    Seq(
      r.imdb.map(x => f"IMDb $x%.1f"),
      r.rottenTomatoes.map(s => s"RT $s%"),
      r.metascore.map(s => s"Metacritic $s"),
      r.filmweb.map(x => f"Filmweb $x%.1f")
    ).flatten
  }

  /** The rating badges for the OG-card image — the same per-source brand-coloured
   *  two-segment pills the web/iOS/Android render (see [[tools.OgCardRenderer.ratingBadges]]). */
  private[controllers] def cardRatingBadges(film: FilmSchedule): Seq[tools.OgCardRenderer.Badge] =
    tools.OgCardRenderer.ratingBadges(
      imdb           = film.resolved.ratings.imdb,
      metascore      = film.resolved.ratings.metascore,
      rottenTomatoes = film.resolved.ratings.rottenTomatoes,
      filmweb        = film.resolved.ratings.filmweb
    )

  /** The "2026 · Dramat, Kryminał" line under the title on the OG card —
   *  release year then genres, each part omitted when absent. */
  private[controllers] def cardSubtitle(film: FilmSchedule): String =
    (film.movie.releaseYear.map(_.toString).toSeq ++ Seq(film.movie.genres.mkString(", ")).filter(_.nonEmpty))
      .mkString(" · ")
}
