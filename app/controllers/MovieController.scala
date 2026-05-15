package controllers

import models._
import play.api.mvc._
import play.api.{Environment, Mode}
import services.movies.MovieService

import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.util.Try

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

class MovieController(
  cc:           ControllerComponents,
  movieService: MovieService,
  env:          Environment
) extends AbstractController(cc) {

  def index(): Action[AnyContent] = Action { request =>
    Ok(views.html.repertoire(toSchedules(), Cinema.all.map(_.displayName), devMode))
  }

  // Permissive robots.txt — link-preview scrapers (Facebook's
  // `facebookexternalhit` in particular) treat a 404 here as "site not
  // crawlable" and surface that as a generic 403 to the debugger UI. Serving
  // an explicit allow-all unblocks their preview fetch. No private endpoints
  // to gate.
  def robotsTxt: Action[AnyContent] = Action {
    Ok("User-agent: *\nAllow: /\n").as("text/plain; charset=utf-8")
  }

  def kina(): Action[AnyContent] = Action { request =>
    Ok(views.html.kina(toCinemaSchedules(), Cinema.all.map(_.displayName), devMode))
  }

  def debug(): Action[AnyContent] = Action {
    devOnly {
      val rows = movieService.snapshot().sortBy { case (t, _, _) => t.toLowerCase }
      Ok(views.html.debug(rows))
    }
  }

  def film(title: String): Action[AnyContent] = Action { request =>
    toSchedules().find(_.movie.title == normalizeTitle(title)) match {
      case Some(schedule) =>
        // Build absolute URL for og:url. Trust the proxy: Fly terminates TLS
        // and forwards X-Forwarded-Proto, so request.secure is correct in
        // production. URL-encode the title so unusual characters round-trip.
        val proto        = if (request.secure) "https" else "http"
        val encodedTitle = java.net.URLEncoder.encode(schedule.movie.title, "UTF-8")
        val canonicalUrl = s"$proto://${request.host}/film?title=$encodedTitle"
        Ok(views.html.film(schedule, canonicalUrl, MovieController.previewDescription(schedule)))
      case None => NotFound(s"Film not found: $title")
    }
  }

  /** Drop a single row from cache + Mongo and re-fetch every upstream source
   *  (TMDB, IMDb rating, Filmweb, Metacritic, Rotten Tomatoes). Writes happen
   *  incrementally on the worker pool — the request returns immediately.
   *
   *  Looks up cinema-side hints (`director`, `originalTitle`) from the live
   *  showtime cache so the re-resolve uses the same signals the bus-driven
   *  `MovieRecordCreated` path uses. Without these, a TMDB title search alone can
   *  re-elect a same-title-different-film hit and silently undo earlier
   *  corrections (e.g. Rialto's "On drive" resolving back to the LEGO F1
   *  doc instead of the Ukrainian war drama whose director the cinema does
   *  report). */
  def reEnrich(title: String, year: Option[Int]): Action[AnyContent] = Action {
    devOnly {
      val hint = movieService.get(title, year)
      movieService.reEnrich(
        title,
        year,
        hint.flatMap(_.cinemaOriginalTitle),
        hint.flatMap(_.director)
      )
      NoContent
    }
  }

  // All /debug/* endpoints return 404 in production so the cache contents and
  // the re-enrichment trigger aren't exposed on a deployed instance. Mode
  // defaults to Dev in `AppLoader` unless APP_MODE=prod is set explicitly.
  private def devOnly(result: => play.api.mvc.Result): play.api.mvc.Result =
    if (env.mode == Mode.Prod) NotFound("dev-only endpoint") else result

  // Same flag the regular navbar uses to gate the Debug tab.
  private def devMode: Boolean = env.mode != Mode.Prod

  // Phase 4: reads now walk the unified `MovieCache` directly. Each
  // record carries its own per-cinema slots in `cinemaShowings`; there's no
  // cross-cinema mergeKey pass at read time because the merge already happened
  // at write time (phase 2's stable docId + phase 3's `recordCinemaScrape`).

  private def toSchedules(): Seq[FilmSchedule] =
    toSchedules(LocalDateTime.now(ZoneId.of("Europe/Warsaw")))

  /** Overload with an injectable `now` so tests can pin the clock to a
   *  fixture's capture date and assert what the / page would render at that
   *  moment. Production callers should always use the no-arg variant. */
  def toSchedules(now: LocalDateTime): Seq[FilmSchedule] = {
    movieService.snapshot().flatMap { case (_, _, e) =>
      // Flatten every cinema's future showtimes for this film. Records with
      // no future showings (film stopped playing everywhere) drop out of the
      // list view — they stay in storage per the "keep forever" policy.
      val allShowtimes = e.cinemaShowings.toSeq.flatMap { case (cinema, slot) =>
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
          e.cinemaShowings.toSeq.flatMap { case (cinema, slot) => slot.filmUrl.map(cinema -> _) }
        Some((earliest, FilmSchedule(
          movie          = Movie(e.displayTitle, e.runtimeMinutes, e.releaseYear),
          posterUrl      = e.posterUrl,
          synopsis       = e.synopsis,
          cast           = e.cast,
          director       = e.director,
          cinemaFilmUrls = cinemaFilmUrls,
          showings       = byDate,
          enrichment     = Some(e)
        )))
      }
    }.sortBy(_._1).map(_._2)
  }

  private def toCinemaSchedules(): Seq[CinemaSchedule] = {
    val now = LocalDateTime.now(ZoneId.of("Europe/Warsaw"))
    Cinema.all.flatMap { cinema =>
      val moviesForCinema = movieService.snapshot().flatMap { case (_, _, e) =>
        e.cinemaShowings.get(cinema).flatMap { slot =>
          val future = slot.showtimes.filter(_.dateTime.isAfter(now.minusMinutes(30)))
          if (future.isEmpty) None
          else {
            val byDate = future
              .groupBy(_.dateTime.toLocalDate)
              .toSeq.sortBy(_._1)
              .map { case (date, sts) => (date, sts.sortBy(_.dateTime)) }
            Some(CinemaMovieSchedule(
              movie      = Movie(e.displayTitle, e.runtimeMinutes, e.releaseYear),
              // Per-cinema view shows that cinema's own poster (fidelity over
              // merge); fall back to the merged best only if this cinema
              // didn't ship one.
              posterUrl  = slot.posterUrl.orElse(e.posterUrl),
              filmUrl    = slot.filmUrl,
              showings   = byDate,
              enrichment = Some(e)
            ))
          }
        }
      }.sortBy(_.showings.head._1)
      if (moviesForCinema.isEmpty) None
      else Some(CinemaSchedule(cinema, moviesForCinema))
    }
  }

  private def normalizeTitle(title: String): String = TitleNormalizer.normalize(title)
}

object MovieController {
  /** Build the `og:description` / `twitter:description` text for the film
   *  page. Format: rating summary ("IMDb 8.7 · RT 86% · Metacritic 79 ·
   *  Filmweb 7.5") prefixed to the synopsis, truncated to keep WhatsApp /
   *  Messenger / Telegram previews readable. Skips ratings that aren't set;
   *  the whole string may be empty for films with no enrichment + no
   *  synopsis. */
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
      else if (ratings.nonEmpty)                 ratings
      else                                       synopsis
    // 300 chars is the practical cap most preview UIs render before
    // truncating; we add an ellipsis to make truncation visible.
    if (joined.length > 300) joined.take(297) + "…" else joined
  }
}
