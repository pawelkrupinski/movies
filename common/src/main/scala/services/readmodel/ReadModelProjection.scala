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
 * one cinema's showtimes changed projects to the same documents except that
 * cinema's `CityScreening`.
 *
 * Resolution logic itself stays on [[MovieRecord]] — this object only
 * *materialises* its accessors, so there's still one source of truth for how a
 * field is merged across sources.
 */
object ReadModelProjection {

  /** The film identity — `sanitize(title)|year`, where `year` is the film's
   *  RESOLVED (TMDB-authoritative) year, the same notion the served
   *  `releaseYear` uses. Keying on the resolved year — NOT the source `_id`'s
   *  raw cinema-reported year, which a scrape pins before enrichment and which
   *  disagrees venue-to-venue — means a film maps to ONE stable read-model id
   *  even while the source holds it under several raw-year keys (the worker
   *  cache churns same-tmdbId year variants faster than `settle` collapses
   *  them). So the same physical film never splits into two cards: source rows
   *  `kumotry|2025` and `kumotry|2026` both project to `kumotry|2026`, and their
   *  screenings (whose `_id` embeds this id) collapse onto the one film. */
  def filmId(stored: StoredMovieRecord): String =
    s"${TitleNormalizer.sanitize(stored.title)}|${stored.record.resolvedYear.map(_.toString).getOrElse("")}"

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
      synopsis           = r.synopsisNonCinema,
      synopsisByCity     = synopsisByCity(r),
      trailerUrls        = r.trailerUrls.flatMap(TrailerEmbed.embedUrlFor).distinct,
      ratings            = ratingsFor(r, title),
      weightedRating     = r.weightedRating
    )
  }

  /** Per-city synopsis overrides for the read model, keyed by `City.slug`. For
   *  each city the film screens in, the city-scoped pick ([[MovieRecord.synopsisForCity]]
   *  — that city's cinemas + TMDB/IMDb) is stored ONLY when it differs from the
   *  city-independent `synopsisNonCinema` fallback `ResolvedMovie.synopsis` holds.
   *  Most cities have no cinema blurb richer than TMDB's, so they tie the fallback
   *  and are omitted — the map carries only the genuine per-city exceptions, and
   *  `ResolvedMovie.synopsisFor` falls back for the rest. */
  private def synopsisByCity(r: MovieRecord): Map[String, String] = {
    val fallback = r.synopsisNonCinema
    r.cities.flatMap { city =>
      val scoped = r.synopsisForCity(city)
      if (scoped != fallback) scoped.map(city.slug -> _) else None
    }.toMap
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
   *  showtimes surface. Showtimes are sorted into a canonical order so the document
   *  is a pure function of the showtime *set*, not of upstream scrape order —
   *  reordering upstream can't churn the diff. */
  def screenings(stored: StoredMovieRecord): Seq[CityScreening] =
    screeningsFor(stored.record.cinemaData, filmId(stored))

  /** One `CityScreening` per (city, cinema) for the given cinema slots, keyed
   *  under `fid`. Shared by [[screenings]] (all of a row's cinemas) and the
   *  per-variant split (only the cinemas that reported one display title). */
  private def screeningsFor(cinemaData: Map[Cinema, SourceData], fid: String): Seq[CityScreening] =
    cinemaData.toSeq.flatMap { case (cinema, slot) =>
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

  /** Display-title VARIANTS of a row: the row's cinema slots grouped by the
   *  SANITIZED form of their reported title — the exact key the `movies`
   *  collection used to split rows by before same-film records were merged
   *  across titles. So a film a cinema lists under a Cyrillic / English /
   *  banner-prefixed title forms its own group even though it now shares one
   *  stored record (one tmdbId, one set of merged facts) with the Polish
   *  listing. Sorted by key for deterministic output. A cinema slot with no
   *  reported title falls into the record's anchor key (`sanitize(stored.title)`). */
  private def variants(stored: StoredMovieRecord): Seq[(String, Set[Cinema])] = {
    val anchorKey = TitleNormalizer.sanitize(stored.title)
    stored.record.cinemaData.toSeq
      .groupBy { case (_, slot) => slot.title.map(TitleNormalizer.sanitize).getOrElse(anchorKey) }
      .view.mapValues(_.map(_._1).toSet).toSeq
      .sortBy(_._1)
  }

  /** Every read-model film id a row projects to — one per display-title variant.
   *  The read-model reconcile uses this to know which `web_movies` ids are still
   *  live for a row, so a split-off variant card isn't pruned as an orphan. */
  def filmIds(stored: StoredMovieRecord): Seq[String] = {
    val groups = variants(stored)
    if (groups.sizeIs <= 1) Seq(filmId(stored))
    else groups.map { case (_, cinemas) => variantFilmId(stored, cinemas) }
  }

  /** The split projection for a row: one `(ResolvedMovie, screenings)` per
   *  display-title variant. A row whose cinemas all report one title-key (the
   *  overwhelming common case), or a TMDB-only row with no cinema slots, yields
   *  exactly ONE entry, byte-identical to [[resolve]]/[[screenings]] — so an
   *  unsplit film never churns. Only a genuinely multi-title record (Cyrillic /
   *  English-alias / banner-prefixed listings of one film) fans out into
   *  several cards that share year/director/cast/ratings but carry their own
   *  title, synopsis and screening subset. */
  def projectAll(stored: StoredMovieRecord): Seq[(ResolvedMovie, Seq[CityScreening])] = {
    val groups = variants(stored)
    if (groups.sizeIs <= 1) Seq(project(stored))
    else groups.map { case (_, cinemas) => projectVariant(stored, cinemas) }
  }

  /** The film id for one display-title variant: `sanitize(variantTitle)|year`,
   *  where the variant title is derived from only that group's cinemas (so two
   *  groups never collide — distinct sanitize keys) and the year is the shared
   *  resolved year. */
  private def variantFilmId(stored: StoredMovieRecord, cinemas: Set[Cinema]): String = {
    val scoped = stored.record.scopedToCinemas(cinemas)
    s"${TitleNormalizer.sanitize(scoped.displayTitle(stored.title))}|${stored.record.resolvedYear.map(_.toString).getOrElse("")}"
  }

  /** Project one display-title variant. Shared facts (poster, year, genres,
   *  countries, director, cast, runtime, trailers, rating values, weighted
   *  rating) come from the FULL record via [[resolve]]; only the title, the
   *  synopsis pool (this variant's cinemas + the shared TMDB/IMDb fallback) and
   *  the screening subset are scoped to the group. */
  private def projectVariant(stored: StoredMovieRecord, cinemas: Set[Cinema]): (ResolvedMovie, Seq[CityScreening]) = {
    val r      = stored.record
    val scoped = r.scopedToCinemas(cinemas)
    val title  = scoped.displayTitle(stored.title)
    val fid    = s"${TitleNormalizer.sanitize(title)}|${r.resolvedYear.map(_.toString).getOrElse("")}"
    val movie  = resolve(stored).copy(
      _id            = fid,
      title          = title,
      originalTitle  = r.distinctOriginalTitle(title),
      // Synopsis is the one field scoped to the shown title: the city overrides
      // come from THIS variant's cinemas, the city-independent fallback stays
      // the shared TMDB/IMDb blurb (`synopsisNonCinema` ignores cinema sources,
      // so it's identical across variants).
      synopsis       = scoped.synopsisNonCinema,
      synopsisByCity = synopsisByCity(scoped),
      ratings        = ratingsFor(r, title)
    )
    (movie, screeningsFor(scoped.cinemaData, fid))
  }

  /** Both halves of the projection for ONE display-title variant — the
   *  single-card view. [[projectAll]] is the split-aware entry point production
   *  serves from; this stays the building block for the common single-title row
   *  and for callers that materialise one card (the `/debug` table, view specs). */
  def project(stored: StoredMovieRecord): (ResolvedMovie, Seq[CityScreening]) =
    (resolve(stored), screenings(stored))
}
