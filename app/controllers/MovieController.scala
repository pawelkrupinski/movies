package controllers

import models._
import play.api.Logging
import play.api.libs.json.{Json, Writes}
import play.api.mvc._
import play.api.Mode
import services.movies.{MovieCache, MovieService, StoredMovieRecord, TitleNormalizer, TrailerEmbed}

import java.time.{LocalDate, LocalDateTime, ZoneId}
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
  runtimeMinutes: Option[Int], ratings: ApiRatings,
  countries: Seq[String], directors: Seq[String], cast: Seq[String],
  synopsis: Option[String], trailerURLs: Seq[String],
  showings: Seq[ApiDayShowings]
)

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
      synopsis         = fs.synopsis,
      // Same source + transform the /film detail page uses (raw trailer
      // URLs → YouTube embed URLs, deduped) so JSON clients render the
      // identical Zwiastun set without re-deriving it.
      trailerURLs      = e.toSeq.flatMap(_.trailerUrls).flatMap(TrailerEmbed.embedUrlFor).distinct,
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

case class CinemaMovieSchedule(
                                movie: Movie,
                                posterUrl: Option[String],
                                filmUrl: Option[String],
                                showings: Seq[(LocalDate, Seq[Showtime])],
                                enrichment: Option[MovieRecord] = None
                              )

case class CinemaSchedule(cinema: Cinema, movies: Seq[CinemaMovieSchedule])

class MovieControllerService(movieService: MovieService) {
  def debugData(): Seq[StoredMovieRecord] = {
    val rows = movieService.snapshot().sortBy(_.title.toLowerCase)
    rows
  }

  def reenrich(title: String, year: Option[Int]): Unit = {
    val hint = movieService.get(title, year)
    movieService.reEnrich(
      title,
      year,
      hint.flatMap(_.cinemaOriginalTitle),
      hint.map(_.director).filter(_.nonEmpty).map(_.mkString(", "))
    )
  }

  def toSchedules(): Seq[FilmSchedule] =
    toSchedules(LocalDateTime.now(ZoneId.of("Europe/Warsaw")))

  /** Overload with an injectable `now` so tests can pin the clock to a
   * fixture's capture date and assert what the / page would render at that
   * moment. Production callers should always use the no-arg variant. */
  def toSchedules(now: LocalDateTime): Seq[FilmSchedule] = {
    movieService.snapshot().flatMap { case StoredMovieRecord(cleanTitle, _, e) =>
      // Flatten every cinema's future showtimes for this film. Records with
      // no future showings (film stopped playing everywhere) drop out of the
      // list view — they stay in storage per the "keep forever" policy.
      val allShowtimes = e.cinemaData.toSeq.flatMap { case (cinema, slot) =>
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
                .toSeq.sortBy { case (_, ss) => ss.map(_._2.dateTime).min }
                .map { case (cinema, ss) => CinemaShowtimes(cinema, ss.map(_._2).sortBy(_.dateTime)) }
              (date, perCinema)
            }
        val cinemaFilmUrls: Seq[(Cinema, String)] =
          e.cinemaData.toSeq.flatMap { case (cinema, slot) => slot.filmUrl.map(cinema -> _) }
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
    }.sortBy(_._1).map(_._2)
  }

  def toCinemaSchedules(): Seq[CinemaSchedule] =
    toCinemaSchedules(LocalDateTime.now(ZoneId.of("Europe/Warsaw")))

  /** Overload with an injectable `now` — see `toSchedules(now)` for the same
   *  pattern. Production callers should always use the no-arg variant. */
  def toCinemaSchedules(now: LocalDateTime): Seq[CinemaSchedule] = {
    Cinema.all.flatMap { cinema =>
      val moviesForCinema = movieService.snapshot().flatMap { case StoredMovieRecord(cleanTitle, _, e) =>
        e.cinemaData.get(cinema).flatMap { slot =>
          val future = slot.showtimes.filter(_.dateTime.isAfter(now.minusMinutes(30)))
          if (future.isEmpty) None
          else {
            val byDate = future
              .groupBy(_.dateTime.toLocalDate)
              .toSeq.sortBy(_._1)
              .map { case (date, sts) => (date, sts.sortBy(_.dateTime)) }
            Some(CinemaMovieSchedule(
              movie = Movie(e.displayTitle(cleanTitle), e.runtimeMinutes, e.releaseYear, countries = e.countries, genres = e.genres),
              // Per-cinema view shows that cinema's own poster (fidelity over
              // merge); fall back to the merged best only if this cinema
              // didn't ship one.
              posterUrl = slot.posterUrl.orElse(e.posterUrl),
              filmUrl = slot.filmUrl,
              showings = byDate,
              enrichment = Some(e)
            ))
          }
        }
      }.sortBy(_.showings.head._1)
      if (moviesForCinema.isEmpty) None
      else Some(CinemaSchedule(cinema, moviesForCinema))
    }
  }

  def film(title: String): Option[FilmSchedule] = {
    val needle = normalizeTitle(title)
    toSchedules().find(s => normalizeTitle(s.movie.title) == needle)
  }

  private def normalizeTitle(title: String): String = TitleNormalizer.normalize(title)

  def rehydrate(): Int = movieService.rehydrate()
}

class MovieController( cc: ControllerComponents,
                       movieControllerService: MovieControllerService,
                       movieCache: MovieCache,
                       userRepo: services.users.UserRepo,
                       oauthProviders: Set[String],
                       environment: Mode
                     ) extends AbstractController(cc) with Logging {

  // Read the session's `userId` (set by `AuthController.callback`) and
  // resolve it to a User if the row is still there. Returns None for
  // anonymous browsers AND for the rare case where a previously
  // authenticated session's user row was deleted out of band — the
  // session is stale, treat it as logged out.
  private def currentUser(request: RequestHeader): Option[models.User] =
    request.session.get("userId").flatMap(userRepo.findById)

  // Render the main "Filmy" listing — repertoire view, full corpus,
  // OG meta derived from `?…` filter params. Shared between `/` and
  // `/filmy` (no params) so both URLs are interchangeable; `/filmy`
  // with one of the browse-axis params still routes through `browse`
  // below to the per-director / per-cast / per-country page.
  private def renderIndex(request: RequestHeader): Result = {
    val user      = currentUser(request)
    val schedules = movieControllerService.toSchedules()
    val meta      = FilterDescription.forIndex(request.queryString, schedules)
    Ok(views.html.repertoire(
      schedules, Cinema.all.map(_.displayName), Cinema.pillMap,
      devMode, user, oauthProviders,
      pageTitle       = meta.title,
      pageDescription = meta.description,
      pageUrl         = PageMeta.canonicalUrl(request),
      fbAppId         = PageMeta.fbAppId,
    ))
  }

  def index(): Action[AnyContent] = Action(renderIndex)

  private def renderBrowse(heading: String, films: Seq[FilmSchedule], request: RequestHeader): Result = {
    val user = currentUser(request)
    Ok(views.html.browse(
      films, heading, devMode, user, oauthProviders,
      pageUrl = PageMeta.canonicalUrl(request),
      fbAppId = PageMeta.fbAppId,
    ))
  }

  def browse(kraj: Option[String], rezyser: Option[String], aktor: Option[String]): Action[AnyContent] = Action { request =>
    val all = movieControllerService.toSchedules()
    (kraj, rezyser, aktor) match {
      case (Some(name), _, _) => renderBrowse(name, all.filter(_.movie.countries.contains(name)), request)
      case (_, Some(name), _) => renderBrowse(name, all.filter(_.director.contains(name)),      request)
      case (_, _, Some(name)) => renderBrowse(name, all.filter(_.cast.contains(name)),          request)
      // `/filmy` with no filter axis is the main listing — same view as
      // `/`. The browse view only kicks in for the per-axis pages reached
      // from the meta-link rows on /film.
      case _                  => renderIndex(request)
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

  def kina(): Action[AnyContent] = renderKina(None)

  // `/kina/:cinema` — pin the grid to a single cinema by display name. The
  // pinned cinema persists across refreshes because it lives in the URL;
  // clicking a pill on the page rewrites the path so the URL and the pin
  // never drift apart. Unknown labels are ignored (the page renders as
  // unpinned) — see `renderKina`.
  def kinaPinned(cinema: String): Action[AnyContent] = renderKina(Some(cinema))

  private def renderKina(pinnedCinema: Option[String]): Action[AnyContent] = Action { request =>
    val user = currentUser(request)
    val allCinemas = Cinema.all.map(_.displayName)
    val pinned = pinnedCinema.filter(allCinemas.contains)
    Ok(views.html.kina(
      movieControllerService.toCinemaSchedules(), allCinemas, Cinema.pillMap,
      devMode, user, oauthProviders, pinned,
      pageUrl = PageMeta.canonicalUrl(request),
      fbAppId = PageMeta.fbAppId,
    ))
  }

  def apiRepertoire(): Action[AnyContent] = Action { request =>
    val lastMod = movieCache.lastModified.truncatedTo(java.time.temporal.ChronoUnit.SECONDS)
    val httpDate = java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME
      .format(lastMod.atOffset(java.time.ZoneOffset.UTC))

    val notModified = request.headers.get("If-Modified-Since").exists { ims =>
      scala.util.Try(java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME.parse(ims))
        .map(java.time.Instant.from)
        .toOption
        .exists(!lastMod.isAfter(_))
    }

    if (notModified) NotModified
    else {
      val films = movieControllerService.toSchedules().map(ApiFilm.from)
      Ok(Json.toJson(films)).withHeaders("Last-Modified" -> httpDate)
    }
  }

  def debug(): Action[AnyContent] = Action {
    devOnly {

      Ok(views.html.debug(movieControllerService.debugData()))
    }
  }

  def film(title: String): Action[AnyContent] = Action { request =>
    movieControllerService.film(title) match {
      case Some(schedule) =>
        // `request.uri` would carry the raw inbound title-encoding; use the
        // canonical FilmHref form instead so the og:url matches the link the
        // page exposes elsewhere. Scheme/host come from PageMeta so the
        // X-Forwarded-* workaround (Play 3.0's `request.secure` ignores the
        // `trustedProxies` knob on this Fly setup) is in one place.
        val canonicalUrl = PageMeta.origin(request) + FilmHref(schedule.movie.title)
        val user = currentUser(request)
        Ok(views.html.film(schedule, canonicalUrl, MovieController.previewDescription(schedule), devMode, user, oauthProviders))
      case None => NotFound(s"Film not found: $title")
    }
  }

  /** Drop a single row from cache + Mongo and re-fetch every upstream source
   * (TMDB, IMDb rating, Filmweb, Metacritic, Rotten Tomatoes). Writes happen
   * incrementally on the worker pool — the request returns immediately.
   *
   * Looks up cinema-side hints (`director`, `originalTitle`) from the live
   * showtime cache so the re-resolve uses the same signals the bus-driven
   * `MovieRecordCreated` path uses. Without these, a TMDB title search alone can
   * re-elect a same-title-different-film hit and silently undo earlier
   * corrections (e.g. Rialto's "On drive" resolving back to the LEGO F1
   * doc instead of the Ukrainian war drama whose director the cinema does
   * report). */
  def reEnrich(title: String, year: Option[Int]): Action[AnyContent] = Action {
    devOnly {
      movieControllerService.reenrich(title, year)
      NoContent
    }
  }

  /** Drop the in-memory positive cache and reload it from Mongo. Available in
   * every mode (unlike the rest of the debug endpoints) so a fly.io instance
   * whose cache drifted from Mongo can be reconciled without a redeploy.
   * The negative cache (24h TTL TMDB-miss markers) is left alone. */
  def rehydrate(): Action[AnyContent] = Action {
    val count = movieControllerService.rehydrate()
    Ok(s"rehydrated $count rows\n").as("text/plain; charset=utf-8")
  }


  // All /debug/* endpoints return 404 in production so the cache contents and
  // the re-enrichment trigger aren't exposed on a deployed instance. Mode
  // defaults to Dev in `AppLoader` unless APP_MODE=prod is set explicitly.
  private def devOnly(result: => play.api.mvc.Result): play.api.mvc.Result =
    if (environment == Mode.Prod) NotFound("dev-only endpoint") else result

  // Same flag the regular navbar uses to gate the Debug tab.
  private def devMode: Boolean = environment != Mode.Prod
}

object MovieController {
  /** Build the `og:description` / `twitter:description` text for the film
   * page. Format: rating summary ("IMDb 8.7 · RT 86% · Metacritic 79 ·
   * Filmweb 7.5") prefixed to the synopsis, truncated to keep WhatsApp /
   * Messenger / Telegram previews readable. Skips ratings that aren't set;
   * the whole string may be empty for films with no enrichment + no
   * synopsis. */
  private[controllers] def previewDescription(film: FilmSchedule): String = {
    val ratings = Seq(
      film.enrichment.flatMap(_.imdbRating).map(r => f"IMDb $r%.1f"),
      film.enrichment.flatMap(_.rottenTomatoes).map(s => s"RT $s%"),
      film.enrichment.flatMap(_.metascore).map(s => s"Metacritic $s"),
      film.enrichment.flatMap(_.filmwebRating).map(r => f"Filmweb $r%.1f")
    ).flatten.mkString(" · ")
    val synopsis = film.synopsis.getOrElse("").trim
    val joined =
      if (ratings.nonEmpty && synopsis.nonEmpty) ratings + " — " + synopsis
      else if (ratings.nonEmpty) ratings
      else synopsis
    // 300 chars is the practical cap most preview UIs render before
    // truncating; we add an ellipsis to make truncation visible.
    if (joined.length > 300) joined.take(297) + "…" else joined
  }
}
