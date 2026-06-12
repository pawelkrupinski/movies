package services.readmodel

import models._
import services.movies.{StoredMovieRecord, TitleNormalizer, TrailerEmbed}

/**
 * Pure projection from a stored `MovieRecord` row to the denormalised read
 * model the web serves: one [[ResolvedMovie]] (merged metadata, no source
 * data) plus one [[CityScreening]] per (city, cinema) the film currently
 * screens in.
 *
 * No clock, no I/O — every value is a deterministic function of the input row,
 * so the same row always projects to identical documents. That determinism is
 * what the projector's minimal-write diff relies on: a row whose metadata is
 * unchanged projects to the same `ResolvedMovie` and is skipped; a row where
 * one cinema's showtimes changed projects to the same docs except that
 * cinema's `CityScreening`.
 *
 * Resolution logic itself stays on [[MovieRecord]] — this object only
 * *materialises* its accessors, so there's still one source of truth for how a
 * field is merged across sources.
 */
object ReadModelProjection {

  /** The film identity — identical to the source `movies` `_id`
   *  (`sanitize(title)|year`, matching `MongoMovieRepo.docId`), so screening
   *  docs join back to their `ResolvedMovie` by it. */
  def filmId(stored: StoredMovieRecord): String =
    s"${TitleNormalizer.sanitize(stored.title)}|${stored.year.map(_.toString).getOrElse("")}"

  /** Materialise the merged metadata view. `stored.title` is the cache-key
   *  anchor (`StoredMovieRecord.fromStorage` derives it from the `_id`); we
   *  pass it through `displayTitle` exactly as the web's `toSchedules` does so
   *  the resolved title is byte-identical to the pre-split output. */
  def resolve(stored: StoredMovieRecord): ResolvedMovie = {
    val r     = stored.record
    val title = r.displayTitle(stored.title)
    ResolvedMovie(
      _id                = filmId(stored),
      title              = title,
      originalTitle      = r.distinctOriginalTitle(title),
      posterUrl          = r.posterUrl,
      fallbackPosterUrls = r.fallbackPosterUrls,
      runtimeMinutes     = r.runtimeMinutes,
      releaseYear        = r.resolvedYear,   // TMDB's year is authoritative for display, as for the key
      genres             = r.genres,
      countries          = r.countries,
      directors          = r.director,
      cast               = r.cast,
      synopsis           = r.synopsis,
      trailerUrls        = r.trailerUrls.flatMap(TrailerEmbed.embedUrlFor).distinct,
      ratings            = ratingsFor(r, title),
      weightedRating     = r.weightedRating
    )
  }

  /** Materialise a record's per-source ratings + their click-through URLs into
   *  the flat [[ResolvedRatings]] the web renders. Shared by [[resolve]] and the
   *  `/debug` table so both show identical rating links. `title` is the display
   *  title the `*Href` fallbacks key off when a source supplies no direct URL. */
  def ratingsFor(r: MovieRecord, title: String): ResolvedRatings =
    ResolvedRatings(
      imdb              = r.imdbRating,
      imdbUrl           = r.imdbUrl,
      metascore         = r.metascore,
      metacriticUrl     = r.metacriticHref(title),
      rottenTomatoes    = r.rottenTomatoes,
      rottenTomatoesUrl = r.rottenTomatoesHref(title),
      filmweb           = r.filmwebRating,
      filmwebUrl        = r.filmwebHref(title)
    )

  /** One `CityScreening` per (city, cinema) the film currently screens in.
   *  A cinema slot with no showtimes, or one that maps to no city, contributes
   *  nothing — matching the web's existing rule that only this-city cinemas'
   *  showtimes surface. Showtimes are sorted into a canonical order so the doc
   *  is a pure function of the showtime *set*, not of upstream scrape order —
   *  reordering upstream can't churn the diff. */
  def screenings(stored: StoredMovieRecord): Seq[CityScreening] = {
    val fid = filmId(stored)
    stored.record.cinemaData.toSeq.flatMap { case (cinema, slot) =>
      if (slot.showtimes.isEmpty) None
      else City.forCinema(cinema).map { city =>
        CityScreening(
          _id       = s"$fid|${city.slug}|${cinema.displayName}",
          filmId    = fid,
          city      = city.slug,
          cinema    = cinema.displayName,
          filmUrl   = slot.filmUrl,
          showtimes = slot.showtimes
            .sortBy(st => (st.dateTime.toString, st.bookingUrl.getOrElse(""), st.format.mkString(",")))
        )
      }
    }.sortBy(_._id)
  }

  /** Both halves of the projection for one row. */
  def project(stored: StoredMovieRecord): (ResolvedMovie, Seq[CityScreening]) =
    (resolve(stored), screenings(stored))
}
