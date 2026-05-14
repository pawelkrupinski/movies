package controllers

import models._
import play.api.mvc._
import play.api.{Environment, Mode}
import services.ShowtimeCache
import services.enrichment.EnrichmentService

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
                         enrichment: Option[Enrichment] = None
                       )

case class CinemaMovieSchedule(
                                movie: Movie,
                                posterUrl: Option[String],
                                filmUrl: Option[String],
                                showings: Seq[(LocalDate, Seq[Showtime])],
                                enrichment: Option[Enrichment] = None
                              )

case class CinemaSchedule(cinema: Cinema, movies: Seq[CinemaMovieSchedule])

class MovieController(
  cc:                ControllerComponents,
  cache:             ShowtimeCache,
  enrichmentService: EnrichmentService,
  env:               Environment
) extends AbstractController(cc) {

  def index(): Action[AnyContent] = Action { request =>
    Ok(views.html.repertoire(toSchedules(cache.get()), Cinema.all.map(_.displayName), devMode))
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
    Ok(views.html.kina(toCinemaSchedules(cache.get()), Cinema.all.map(_.displayName), devMode))
  }

  def debug(): Action[AnyContent] = Action {
    devOnly {
      val movies = cache.get()
      // One enrichment lookup per unique title (same grouping key the debug
      // view uses) — schedule the same year as toSchedules: first non-empty
      // year seen.
      val enrichmentByTitle: Map[String, Option[Enrichment]] =
        movies.groupBy(cm => cm.movie.title.toLowerCase.trim).map { case (k, group) =>
          val title = group.head.movie.title
          val year  = group.flatMap(_.movie.releaseYear).headOption
          k -> enrichmentService.get(title, year)
        }
      Ok(views.html.debug(movies, enrichmentByTitle))
    }
  }

  def debugEnrichment(): Action[AnyContent] = Action {
    devOnly {
      // Existing rows from the enrichment cache.
      val enriched: Seq[(String, Option[Int], Option[Enrichment])] =
        enrichmentService.snapshot().map { case (t, y, e) => (t, y, Some(e)) }
      val enrichedKeys: Set[(String, Option[Int])] = enriched.map { case (t, y, _) => (t, y) }.toSet

      // Showtime titles currently in the cache, normalised to the same key the
      // enrichment cache uses (searchTitle strips decoration like anniversary
      // suffixes). Anything not yet enriched gets a row with no Enrichment.
      val pending: Seq[(String, Option[Int], Option[Enrichment])] =
        cache.get()
          .map(cm => (services.enrichment.EnrichmentService.searchTitle(cm.movie.title), cm.movie.releaseYear))
          .distinct
          .filterNot(enrichedKeys.contains)
          .map { case (t, y) => (t, y, None) }

      val rows = (enriched ++ pending).sortBy { case (t, _, _) => t.toLowerCase }
      Ok(views.html.debugEnrichment(rows))
    }
  }

  def film(title: String): Action[AnyContent] = Action { request =>
    toSchedules(cache.get()).find(_.movie.title == normalizeTitle(title)) match {
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
   *  incrementally on the worker pool — the request returns immediately. */
  def reEnrich(title: String, year: Option[Int]): Action[AnyContent] = Action {
    devOnly {
      enrichmentService.reEnrich(title, year)
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

  private def toSchedules(cinemaMovies: Seq[CinemaMovie]): Seq[FilmSchedule] = {
    // Pre-compute canonical→merge-key lookup once (O(N)). Without this, mergeKey
    // scans all titles on every call → O(N²) over the groupBy below.
    val keyFor = TitleNormalizer.mergeKeyLookup(cinemaMovies.map(_.movie.title))
    cinemaMovies
      .groupBy(cm => keyFor(cm.movie.title))
      .toSeq
      .flatMap { case (_, entries) =>
        val now = LocalDateTime.now(ZoneId.of("Europe/Warsaw"))

        // Prefer non-Bułgarska titles: that cinema uses sentence case which may differ from others.
        // Fall back to Bułgarska only if it is the sole source for this film.
        val nonBulgarska   = entries.filter(_.cinema != KinoBulgarska)
        val titleSource    = if (nonBulgarska.isEmpty) entries else nonBulgarska
        // Normalise to Roman numerals before deduplicating so "II" and "2" collapse to one title.
        // preferredDisplay picks the " i " spelling over " & " when both occur.
        val distinctTitles = titleSource.map(e => normalizeTitle(e.movie.title)).distinct
        val displayTitle   = TitleNormalizer.preferredDisplay(distinctTitles)
                              .getOrElse(normalizeTitle(titleSource.head.movie.title))

        val allShowtimes = entries.flatMap(entry => entry.showtimes.map(st => (entry.cinema, st)))
          .filter(_._2.dateTime.isAfter(now.minusMinutes(30)))
        if (allShowtimes.isEmpty) None
        else {
          val earliest = allShowtimes.map(_._2.dateTime).min

          val byDate: Seq[(LocalDate, Seq[CinemaShowtimes])] =
            allShowtimes
              .groupBy(_._2.dateTime.toLocalDate)
              .toSeq
              .sortBy(_._1)
              .map { case (date, cinemaSlotsOnDate) =>
                val cinemaShowtimes = cinemaSlotsOnDate
                  .groupBy(_._1)
                  .toSeq
                  .sortBy { case (_, slots) => slots.map(_._2.dateTime).min }
                  .map { case (cinema, slots) =>
                    CinemaShowtimes(cinema, slots.map(_._2).sortBy(_.dateTime))
                  }
                (date, cinemaShowtimes)
              }

          // Multikino first so its metadata takes priority; others fill gaps
          val metaPriority = entries.sortBy(e => if (e.cinema == Multikino) 0 else 1)

          val runtimeMinutes: Option[Int] = entries.flatMap(_.movie.runtimeMinutes).headOption
          val releaseYear:    Option[Int] = entries.flatMap(_.movie.releaseYear).headOption
          Some((earliest, FilmSchedule(
            movie = Movie(displayTitle, runtimeMinutes, releaseYear),
            posterUrl = metaPriority.flatMap(_.posterUrl).headOption,
            synopsis = metaPriority.flatMap(_.synopsis).headOption,
            cast = metaPriority.flatMap(_.cast).headOption,
            director = metaPriority.flatMap(_.director).headOption,
            cinemaFilmUrls = entries.flatMap(entry => entry.filmUrl.map(url => (entry.cinema, url))),
            showings = byDate,
            // Fall back to the raw cinema-reported titles in case displayTitle
            // diverges from storage — e.g. all cinemas write "Diabeł …
            // u Prady 2" with Arabic, but TitleNormalizer.normalize promotes
            // the merged title to "Prady II".
            enrichment = enrichmentService.getForMerge(displayTitle, entries.map(_.movie.title), releaseYear)
          )))
        }
      }
      .sortBy(_._1)
      .map(_._2)
  }

  private def toCinemaSchedules(cinemaMovies: Seq[CinemaMovie]): Seq[CinemaSchedule] = {
    val now = LocalDateTime.now(ZoneId.of("Europe/Warsaw"))
    Cinema.all.flatMap { cinema =>
      val movies = cinemaMovies
        .filter(_.cinema == cinema)
        .flatMap { entry =>
          val future = entry.showtimes.filter(_.dateTime.isAfter(now.minusMinutes(30)))
          if (future.isEmpty) None
          else {
            val byDate = future
              .groupBy(_.dateTime.toLocalDate)
              .toSeq.sortBy(_._1)
              .map { case (date, sts) => (date, sts.sortBy(_.dateTime)) }
            Some(CinemaMovieSchedule(
              movie      = entry.movie.copy(title = normalizeTitle(entry.movie.title)),
              posterUrl  = entry.posterUrl,
              filmUrl    = entry.filmUrl,
              showings   = byDate,
              // Same Arabic→Roman miss applies on the per-cinema view: fall
              // back to the raw cinema title if the normalised one doesn't hit.
              enrichment = enrichmentService.getForMerge(
                normalizeTitle(entry.movie.title), Seq(entry.movie.title), entry.movie.releaseYear)
            ))
          }
        }
        .sortBy(_.showings.head._1)
      if (movies.isEmpty) None
      else Some(CinemaSchedule(cinema, movies))
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
