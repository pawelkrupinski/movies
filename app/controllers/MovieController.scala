package controllers

import models._
import play.api.Logging
import play.api.mvc._
import play.api.Mode
import services.movies.{MovieService, StoredMovieRecord, TitleNormalizer}

import java.time.{LocalDate, LocalDateTime, ZoneId}

case class CinemaShowtimes(cinema: Cinema, showtimes: Seq[Showtime])

case class FilmSchedule(
                         movie: Movie,
                         posterUrl: Option[String],
                         synopsis: Option[String],
                         cast: Option[String],
                         director: Option[String],
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
      hint.flatMap(_.director)
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
          movie = Movie(e.displayTitle(cleanTitle), e.runtimeMinutes, e.releaseYear, countries = e.countries),
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
              movie = Movie(e.displayTitle(cleanTitle), e.runtimeMinutes, e.releaseYear, countries = e.countries),
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
                       userRepo: services.users.UserRepo,
                       userStateRepo: services.users.UserStateRepo,
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

  // Pre-fetch the logged-in user's favourites so the page renders with
  // `.is-fav` classes already applied — avoids the flash where a user
  // with server-stored favourites lands on a fresh tab and sees their
  // stars unlit until the boot fetch completes. Anonymous users get
  // empty sets; the client-side `paintFavourites` reads localStorage
  // and adds the class itself (idempotent against the server-applied
  // result, so the two paths converge cleanly).
  private def favouriteSets(user: Option[models.User]): (Set[String], Set[String]) =
    user.flatMap(u => userStateRepo.find(u.id)) match {
      case Some(s) => (s.favouriteMovies, s.favouriteScreenings)
      case None    => (Set.empty,         Set.empty)
    }

  def index(): Action[AnyContent] = Action { request =>
    val user = currentUser(request)
    val (favMovies, favScreenings) = favouriteSets(user)
    Ok(views.html.repertoire(movieControllerService.toSchedules(), Cinema.all.map(_.displayName), devMode, user, oauthProviders, favMovies, favScreenings, favouritesMode = false))
  }

  private def renderBrowse(heading: String, films: Seq[FilmSchedule], request: RequestHeader): Result = {
    val user = currentUser(request)
    val (favMovies, favScreenings) = favouriteSets(user)
    Ok(views.html.browse(films, heading, devMode, user, oauthProviders, favMovies, favScreenings))
  }

  def browse(kraj: Option[String], rezyser: Option[String], aktor: Option[String]): Action[AnyContent] = Action { request =>
    val all = movieControllerService.toSchedules()
    val (heading, films) = (kraj, rezyser, aktor) match {
      case (Some(name), _, _) => name -> all.filter(_.movie.countries.contains(name))
      case (_, Some(name), _) => name -> all.filter(_.director.exists(_.split(",").map(_.trim).contains(name)))
      case (_, _, Some(name)) => name -> all.filter(_.cast.exists(_.split(",").map(_.trim).contains(name)))
      case _                  => "Filmy" -> all
    }
    renderBrowse(heading, films, request)
  }

  def favourites(): Action[AnyContent] = Action { request =>
    val user = currentUser(request)
    val (favMovies, favScreenings) = favouriteSets(user)
    // For logged-in users the server already knows their favourite set
    // — pre-filter `toSchedules()` to just those films instead of
    // shipping the whole 2 MB catalogue and asking the browser to hide
    // 99 % of it client-side. Anonymous users keep the full payload
    // because their favourites live in localStorage and the server
    // can't see them; the existing JS filter (IS_FAVOURITES_PAGE)
    // still narrows the visible set on the client. A page with 0
    // favourites and a logged-in user collapses to a tiny "no
    // favourites yet" render.
    val schedules =
      if (user.isDefined)
        movieControllerService.toSchedules().filter { s =>
          favMovies.contains(s.movie.title) ||
            // Screening id format must match `_filmShowings.scala.html`
            // and `badgeScreeningId` in `_sharedJs`: `title|cinema|datetime`.
            // A user can favourite a single screening without favouriting
            // the whole film, so the parent film must still be rendered
            // for the favourited screening to be reachable.
            s.enrichment.exists(_.cinemaData.exists { case (cinema, slot) =>
              slot.showtimes.exists(st =>
                favScreenings.contains(s"${s.movie.title}|${cinema.displayName}|${st.dateTime}")
              )
            })
        }
      else movieControllerService.toSchedules()
    Ok(views.html.repertoire(schedules, Cinema.all.map(_.displayName), devMode, user, oauthProviders, favMovies, favScreenings, favouritesMode = true))
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
    val (favMovies, favScreenings) = favouriteSets(user)
    val allCinemas = Cinema.all.map(_.displayName)
    val pinned = pinnedCinema.filter(allCinemas.contains)
    Ok(views.html.kina(movieControllerService.toCinemaSchedules(), allCinemas, devMode, user, oauthProviders, favMovies, favScreenings, pinned))
  }

  def debug(): Action[AnyContent] = Action {
    devOnly {

      Ok(views.html.debug(movieControllerService.debugData()))
    }
  }

  def film(title: String): Action[AnyContent] = Action { request =>
    movieControllerService.film(title) match {
      case Some(schedule) =>
        // Build absolute URL for og:url. Trust the proxy: Fly terminates TLS
        // and forwards X-Forwarded-Proto, so request.secure is correct in
        // production.
        val proto = if (request.secure) "https" else "http"
        val canonicalUrl = s"$proto://${request.host}${FilmHref(schedule.movie.title)}"
        val user = currentUser(request)
        val (favMovies, favScreenings) = favouriteSets(user)
        val isFavourite = favMovies.contains(schedule.movie.title)
        Ok(views.html.film(schedule, canonicalUrl, MovieController.previewDescription(schedule), isFavourite, favScreenings, devMode, user, oauthProviders))
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
