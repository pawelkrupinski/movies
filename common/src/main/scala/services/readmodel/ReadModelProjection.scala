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

  /** The film identity: the source row's permanent [[services.movies.FilmId]], as is.
   *
   *  It used to be re-derived here as `sanitize(title)|resolvedYear`, which made the
   *  card follow the row's spelling: every settle that re-spelled a title — the slot
   *  vote flipping between two decorated Cineworld listings, a shouting variant winning
   *  for a tick — pruned the card and its screenings and wrote them again under a new
   *  id, ~290 prunes a day. The row's id never changes, so neither does the card's:
   *  a retitle now re-projects the same documents with a new title. (The resolved-year
   *  trick that collapsed a not-yet-settled `kumotry|2025` + `kumotry|2026` pair onto
   *  one card is not needed any more — a row that concludes its year keeps its id.) */
  def filmId(stored: StoredMovieRecord, normalizer: TitleNormalizer): String = stored.id.value

  /** A cheap content hash over EXACTLY the inputs the projected METADATA depends on —
   *  everything the row carries EXCEPT `SourceData.showtimes`. The metadata half of a
   *  projection ([[resolve]] / [[synopsisByCity]] / [[ratingsFor]] and the display-title
   *  [[variants]] partition) never reads showtimes: `MovieRecord.cities` / `synopsisForCity`
   *  key off cinema-slot PRESENCE (`cinemaData.keySet`), and `variants` groups slots by their
   *  reported title — so a showtime-only change at an already-present cinema leaves this hash
   *  UNCHANGED, while a rating / synopsis / new-cinema (→ new city, new title-variant) change
   *  SHIFTS it. [[ReadModelProjector]] keys its metadata cache on this to reuse the projected
   *  `ResolvedMovie` across showtime-only churn and recompute only the cheap screenings half.
   *
   *  Hashing a showtimes-stripped view of the WHOLE record — not an enumerated field list — is
   *  deliberate: the load-bearing danger is a MISSED metadata input serving stale metadata, and
   *  a whole-record hash cannot miss one (no metadata accessor reads showtimes, so stripping only
   *  them is provably a superset of every metadata input). A 32-bit collision would reuse stale
   *  metadata until the row's next change re-projects it — self-healing, the same tolerance the
   *  projector's `##` document diff already accepts. */
  def metadataHash(stored: StoredMovieRecord): Int = {
    val metaSlots = stored.record.data.view.mapValues(_.copy(showtimes = Nil)).toMap
    (stored.title, stored.year, stored.record.copy(data = metaSlots)).##
  }

  /** Materialise the merged metadata view. `stored.title` is the cache-key
   *  anchor (`StoredMovieRecord.fromStorage` derives it from the `_id`); we
   *  pass it through `displayTitle` exactly as the web's `toSchedules` does so
   *  the resolved title is byte-identical to the pre-split output. */
  def resolve(stored: StoredMovieRecord, normalizer: TitleNormalizer): ResolvedMovie = {
    val r     = stored.record
    val title = r.displayTitle(stored.title, normalizer)
    ResolvedMovie(
      _id                = filmId(stored, normalizer),
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
      weightedRating     = r.weightedRating,
      ageRating          = r.ageRating
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
  def screenings(stored: StoredMovieRecord, normalizer: TitleNormalizer): Seq[CityScreening] =
    screeningsFor(stored.record.cinemaShowings, filmId(stored, normalizer))

  /** One `CityScreening` per (city, cinema) for the given cinema slots, keyed
   *  under `fid`. Shared by [[screenings]] (all of a row's cinema slots) and the
   *  per-variant split (only the slots that reported one display title). Within a
   *  variant a venue has at most one slot, so no two screenings collide on `_id`. */
  private def screeningsFor(showings: Seq[(Cinema, SourceData)], fid: String): Seq[CityScreening] =
    showings.flatMap { case (cinema, slot) =>
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
  private def variants(stored: StoredMovieRecord, normalizer: TitleNormalizer): Seq[(String, Set[Source])] = {
    val anchorKey = normalizer.sanitize(stored.title)
    stored.record.cinemaSlots
      .groupBy { case (_, slot) => slot.title.map(normalizer.sanitize).getOrElse(anchorKey) }
      .view.mapValues(_.map(_._1).toSet).toSeq
      .sortBy(_._1)
  }

  /** Every read-model film id a row projects to — one per display-title variant.
   *  The read-model reconcile uses this to know which `web_movies` ids are still
   *  live for a row, so a split-off variant card isn't pruned as an orphan. */
  def filmIds(stored: StoredMovieRecord, normalizer: TitleNormalizer): Seq[String] = {
    val groups = variants(stored, normalizer)
    if (groups.sizeIs <= 1) Seq(filmId(stored, normalizer))
    else groups.map { case (_, sources) => variantFilmId(stored, sources, normalizer) }
  }

  /** The split projection for a row: one `(ResolvedMovie, screenings)` per
   *  display-title variant. A row whose cinemas all report one title-key (the
   *  overwhelming common case), or a TMDB-only row with no cinema slots, yields
   *  exactly ONE entry, byte-identical to [[resolve]]/[[screenings]] — so an
   *  unsplit film never churns. Only a genuinely multi-title record (Cyrillic /
   *  English-alias / banner-prefixed listings of one film) fans out into
   *  several cards that share year/director/cast/ratings but carry their own
   *  title, synopsis and screening subset. */
  def projectAll(stored: StoredMovieRecord, normalizer: TitleNormalizer): Seq[(ResolvedMovie, Seq[CityScreening])] = {
    val groups = variants(stored, normalizer)
    if (groups.sizeIs <= 1) Seq(project(stored, normalizer))
    else groups.map { case (_, sources) => projectVariant(stored, sources, normalizer) }
  }

  /** The SCREENINGS half of [[projectAll]] — one screenings list per display-title
   *  variant — WITHOUT materialising the `ResolvedMovie` metadata. Byte-identical to
   *  `projectAll(stored).map(_._2)`, but skips the costly `resolve` /
   *  [[synopsisByCity]] / ratings work per row. For callers that only need the
   *  per-(city,cinema) showtime buckets — the source-films census counts qualifying
   *  cards per city and never looks at the metadata half, so re-projecting it over
   *  the whole corpus on a timer was pure waste. */
  def screeningsAll(stored: StoredMovieRecord, normalizer: TitleNormalizer): Seq[Seq[CityScreening]] = {
    val groups = variants(stored, normalizer)
    if (groups.sizeIs <= 1) Seq(screenings(stored, normalizer))
    else groups.map { case (_, sources) =>
      screeningsFor(stored.record.scopedToSources(sources).cinemaShowings, variantFilmId(stored, sources, normalizer))
    }
  }

  /** The film id for one display-title variant. The variant that carries the row's
   *  own title keeps the plain film id — so the card an unsplit film already has is
   *  unchanged the moment a second variant (a banner-prefixed listing) joins — and
   *  every other variant is the film id plus its sanitized variant title, which two
   *  groups never share. `~` is not a character `sanitize` emits, nor one any read-model
   *  id composition uses (`|` joins city and cinema onto a screening id). */
  private def variantFilmId(stored: StoredMovieRecord, sources: Set[Source], normalizer: TitleNormalizer): String = {
    val scoped  = stored.record.scopedToSources(sources)
    val variant = normalizer.sanitize(scoped.displayTitle(stored.title, normalizer))
    if (variant == normalizer.sanitize(stored.title)) filmId(stored, normalizer)
    else s"${filmId(stored, normalizer)}~$variant"
  }

  /** Project one display-title variant. Shared facts (poster, year, genres,
   *  countries, director, cast, runtime, trailers, rating values, weighted
   *  rating) come from the FULL record via [[resolve]]; only the title, the
   *  synopsis pool (this variant's cinemas + the shared TMDB/IMDb fallback) and
   *  the screening subset are scoped to the group. */
  private def projectVariant(stored: StoredMovieRecord, sources: Set[Source], normalizer: TitleNormalizer): (ResolvedMovie, Seq[CityScreening]) = {
    val r      = stored.record
    val scoped = r.scopedToSources(sources)
    val title  = scoped.displayTitle(stored.title, normalizer)
    val fid    = variantFilmId(stored, sources, normalizer)
    val movie  = resolve(stored, normalizer).copy(
      _id            = fid,
      title          = title,
      originalTitle  = r.distinctOriginalTitle(title),
      // Synopsis is the one field scoped to the shown title: the city overrides
      // come from THIS variant's cinemas, the city-independent fallback stays
      // the shared TMDB/IMDb blurb (`synopsisNonCinema` ignores cinema sources,
      // so it's identical across variants).
      synopsis       = scoped.synopsisNonCinema,
      synopsisByCity = synopsisByCity(scoped),
      ratings        = ratingsFor(r, title),
      // Scope the certificate to this variant's sources too — a split row's badge
      // should come from the cinema actually shown in the variant.
      ageRating      = scoped.ageRating
    )
    (movie, screeningsFor(scoped.cinemaShowings, fid))
  }

  /** Both halves of the projection for ONE display-title variant — the
   *  single-card view. [[projectAll]] is the split-aware entry point production
   *  serves from; this stays the building block for the common single-title row
   *  and for callers that materialise one card (the `/debug` table, view specs). */
  def project(stored: StoredMovieRecord, normalizer: TitleNormalizer): (ResolvedMovie, Seq[CityScreening]) =
    (resolve(stored, normalizer), screenings(stored, normalizer))
}
