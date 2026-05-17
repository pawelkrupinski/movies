package models

/**
 * Cross-cinema metadata about a film — ratings, IDs, and the per-source
 * content slots used to derive the merged display values. Looked up by
 * (cleaned title + year) once and then shared across every cinema-screening
 * of that same film.
 *
 * `imdbId` is optional because TMDB sometimes returns a hit for which it has
 * no IMDb cross-reference yet (very recent releases, niche films). The
 * MovieRecord is still useful — TMDB id + Filmweb / Metacritic / RT URLs all
 * work without an IMDb id; only the IMDb rating + IMDb link are unavailable.
 * The daily TMDB-retry tick re-checks rows with imdbId=None so they get
 * filled when IMDb eventually indexes the film.
 *
 * Per-source content (synopsis, cast, director, year, countries, …) lives in
 * `data: Map[Source, SourceData]`. Cinemas contribute slots via
 * `recordCinemaScrape`; the TMDB and IMDb enrichment stages contribute their
 * own slots keyed by `Tmdb` / `Imdb`. Merged values are derived on the fly
 * by the accessors below — Multikino first, then the rest of `Cinema.all`,
 * then `Tmdb`, then `Imdb`. Data is *added* to the map, never folded into
 * top-level fields, so each source's contribution stays inspectable.
 */
case class MovieRecord(
  // ── Single-source IDs, ratings, and external-site URLs ────────────────────
  // Each of these has exactly one canonical writer in the codebase, so they
  // stay top-level Option rather than going through the per-source map.
  imdbId:            Option[String]   = None,
  imdbRating:        Option[Double]   = None,
  metascore:         Option[Int]      = None,
  filmwebUrl:        Option[String]   = None,
  filmwebRating:     Option[Double]   = None,
  rottenTomatoes:    Option[Int]      = None,
  tmdbId:            Option[Int]      = None,
  metacriticUrl:     Option[String]   = None,
  rottenTomatoesUrl: Option[String]   = None,

  // Every (cinema, raw title, raw year) tuple that has scraped into this
  // record. Used by `recordCinemaScrape` to detect repeat scrapes — when a
  // cinema reports the same `(title, year)` it reported last tick, no event
  // is published (TMDB / IMDb / rating fetchers already did their work for
  // this combination, so re-publishing would just churn no-op event
  // dispatches across every listener). New scrape tuples — a freshly-seen
  // cinema, a new title spelling, or a year correction — still emit so
  // enrichment can pick up any context that wasn't present before. Also
  // doubles as the source of `cinemaTitles`: every raw title is derived
  // from the scrape tuples — no separately-stored Set is necessary.
  cinemaScrapes:     Set[CinemaScrape] = Set.empty,

  // Per-source data from the most recent refresh. Cinemas contribute on
  // every scrape tick (their slot gets replaced wholesale, and dropped if
  // the film leaves their listings); `Tmdb`/`Imdb` slots come from the
  // enrichment pipeline (and are kept until something re-enriches the row).
  // Merged top-level values (posterUrl, synopsis, …) are computed on the
  // fly from this map — see the accessors below.
  data:              Map[Source, SourceData] = Map.empty
) {
  def imdbUrl: Option[String] = imdbId.map(id => s"https://www.imdb.com/title/$id/")
  def tmdbUrl: Option[String] = tmdbId.map(id => s"https://www.themoviedb.org/movie/$id")

  // ── Slices and views ──────────────────────────────────────────────────────

  /** Cinema-only view of `data` — used by the per-cinema controller paths
   *  (filmUrl per cinema, showtimes per cinema) where TMDB/IMDb slots are
   *  irrelevant. */
  def cinemaData: Map[Cinema, SourceData] =
    data.collect { case (c: Cinema, sd) => c -> sd }

  /** Derived view of every raw cinema-reported title that has scraped into
   *  this record. */
  def cinemaTitles: Set[String] = cinemaScrapes.map(_.title)

  // ── Merged top-level values derived from `data` ──────────────────────────
  //
  // Two flavours of merge:
  //   - **Priority-first** (`director`, `runtimeMinutes`, `releaseYear`,
  //     `posterUrl`, `countries`): iterate sources in `Source.priority`
  //     order — Multikino, then the rest of `Cinema.all`, then `Tmdb`, then
  //     `Imdb` — and pick the first non-empty value (or union for
  //     `countries`). This preserves the prior cinema-only behaviour while
  //     letting TMDB/IMDb fill in fields no cinema reported.
  //   - **Longest-wins** (`synopsis`, `cast`): pick the longest non-empty
  //     value across all sources. Different sources write different-length
  //     blurbs; the longest tends to be the most complete.

  /** Iterate `data` in source-priority order — Multikino first, then the
   *  rest of `Cinema.all`, then `Tmdb`, then `Imdb`. Used by every "first
   *  non-empty" merged accessor. */
  private def prioritized: Seq[(Source, SourceData)] =
    data.toSeq.sortBy { case (s, _) => Source.priority.getOrElse(s, Int.MaxValue) }

  /** Cinema-only iteration in the same priority order — used by accessors
   *  that should *not* fall back to TMDB/IMDb (e.g. `cinemaOriginalTitle`,
   *  which is specifically the cinema-reported English title used as a
   *  TMDB-search hint). */
  private def prioritizedCinema: Seq[(Cinema, SourceData)] =
    cinemaData.toSeq.sortBy { case (c, _) => Source.priority.getOrElse(c, Int.MaxValue) }

  /** Display title chosen across the cinema-reported variants plus the
   *  record's `cleanTitle` anchor (which is the post-decoration-strip form
   *  the cache keys the row by). Caller supplies cleanTitle because the
   *  record itself doesn't carry it — that anchor only exists on the
   *  surrounding CacheKey. Falls back to cleanTitle when there are no
   *  variants yet (TMDB resolved with no cinema scrape yet). */
  def displayTitle(cleanTitle: String): String =
    services.movies.TitleNormalizer.preferredDisplay((cinemaTitles + cleanTitle).toSeq)
      .getOrElse(cleanTitle)

  /** TMDB-resolved original (production-language) title. None when TMDB
   *  hasn't filled this row yet. */
  def originalTitle: Option[String] = data.get(Tmdb).flatMap(_.originalTitle)

  /** First non-empty poster URL across sources in priority order. */
  def posterUrl: Option[String] =
    prioritized.iterator.flatMap(_._2.posterUrl).nextOption()

  /** Longest non-empty synopsis across all sources. */
  def synopsis: Option[String] =
    data.values.flatMap(_.synopsis).toSeq.sortBy(-_.length).headOption

  /** Longest non-empty cast string across all sources. */
  def cast: Option[String] =
    data.values.flatMap(_.cast).toSeq.sortBy(-_.length).headOption

  /** First non-empty director across sources in priority order. */
  def director: Option[String] =
    prioritized.iterator.flatMap(_._2.director).nextOption()

  /** First non-None runtime across sources in priority order. */
  def runtimeMinutes: Option[Int] =
    prioritized.iterator.flatMap(_._2.runtimeMinutes).nextOption()

  /** First non-None release year across sources in priority order. */
  def releaseYear: Option[Int] =
    prioritized.iterator.flatMap(_._2.releaseYear).nextOption()

  /** Production countries — union across sources, deduplicated
   *  case-insensitively while preserving the priority-first order. Sources
   *  spell country names in their own way, so the same country might appear
   *  twice ("USA" + "Stany Zjednoczone") if two of them disagree on spelling
   *  — that's accepted; the dedup only catches exact-match dupes modulo
   *  case. */
  def countries: Seq[String] = {
    val seen = scala.collection.mutable.LinkedHashSet.empty[String]
    prioritized.flatMap(_._2.countries).foreach { c =>
      val key = c.toLowerCase
      if (!seen.exists(_.toLowerCase == key)) seen += c
    }
    seen.toSeq
  }

  /** Cinema → film deep-link, when that cinema reports one. */
  def filmUrlFor(cinema: Cinema): Option[String] =
    cinemaData.get(cinema).flatMap(_.filmUrl)

  /** Cinema → showtimes (empty when that cinema isn't screening). */
  def showtimesFor(cinema: Cinema): Seq[Showtime] =
    cinemaData.get(cinema).map(_.showtimes).getOrElse(Seq.empty)

  /** Cinema-reported original/international title — first non-empty with
   *  Multikino preferred. Separate from `originalTitle` (the TMDB-resolved
   *  production-language title): this is what the cinema's own API exposed,
   *  used as a fallback hint when `reEnrich` triggers a fresh TMDB search. */
  def cinemaOriginalTitle: Option[String] =
    prioritizedCinema.iterator.flatMap(_._2.originalTitle).nextOption()

  /** Display-time URL: validated stored URL if we have one, else an on-the-fly
   *  search URL (for legacy records that pre-date URL persistence). */
  def metacriticHref(fallbackTitle: String): String = metacriticUrl.getOrElse {
    val q = java.net.URLEncoder.encode(originalTitle.getOrElse(fallbackTitle), "UTF-8")
    s"https://www.metacritic.com/search/$q/?category=2"
  }

  def rottenTomatoesHref(fallbackTitle: String): String = rottenTomatoesUrl.getOrElse {
    val q = java.net.URLEncoder.encode(originalTitle.getOrElse(fallbackTitle), "UTF-8")
    s"https://www.rottentomatoes.com/search?search=$q"
  }

  def filmwebHref(fallbackTitle: String): String = filmwebUrl.getOrElse {
    val q = java.net.URLEncoder.encode(originalTitle.getOrElse(fallbackTitle), "UTF-8")
    s"https://www.filmweb.pl/search?query=$q"
  }
}
