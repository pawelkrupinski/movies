package models

/**
 * Cross-cinema metadata about a film — ratings + the URLs you need to link out.
 * Looked up by (cleaned title + year) once and then shared across every
 * cinema-screening of that same film.
 *
 * `imdbId` is optional because TMDB sometimes returns a hit for which it has
 * no IMDb cross-reference yet (very recent releases, niche films). The
 * MovieRecord is still useful — TMDB id + Filmweb / Metacritic / RT URLs all
 * work without an IMDb id; only the IMDb rating + IMDb link are unavailable.
 * The daily TMDB-retry tick re-checks rows with imdbId=None so they get
 * filled when IMDb eventually indexes the film.
 */
case class MovieRecord(
  imdbId:            Option[String],
  imdbRating:        Option[Double],
  metascore:         Option[Int],
  originalTitle:     Option[String],
  filmwebUrl:        Option[String] = None,
  filmwebRating:     Option[Double] = None,
  rottenTomatoes:    Option[Int]    = None,
  tmdbId:            Option[Int]    = None,
  metacriticUrl:     Option[String] = None,
  rottenTomatoesUrl: Option[String] = None,
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
  // Per-cinema data from the most recent scrape tick. Replaces wholesale per
  // cinema per tick. Empty Map for rows that exist only because the TMDB
  // stage resolved them (no cinema is currently screening). Merged top-level
  // values (posterUrl, synopsis, …) are computed on the fly from this map —
  // see the helper methods below — so dropped-cinema cleanup costs nothing.
  cinemaShowings:    Map[Cinema, CinemaShowings]   = Map.empty
) {
  def imdbUrl: Option[String] = imdbId.map(id => s"https://www.imdb.com/title/$id/")
  def tmdbUrl: Option[String] = tmdbId.map(id => s"https://www.themoviedb.org/movie/$id")

  // ── Merged top-level values derived from cinemaShowings ──────────────────
  //
  // Computed on the fly so dropped-cinema cleanup doesn't need a separate
  // sync step. Ordering rule for "prefer one cinema's value" is Multikino
  // first, then the rest in `Cinema.all` order — matches today's
  // `MovieController.metaPriority`.

  /** Iterate cinemaShowings with Multikino first, then everything else in
   *  Cinema.all's stable order. The output is what's used to pick "best"
   *  value for fields that have a single source-of-truth across cinemas. */
  private def prioritizedShowings: Seq[(Cinema, CinemaShowings)] = {
    val priority: Cinema => Int = c => if (c == Multikino) 0 else 1
    cinemaShowings.toSeq.sortBy { case (c, _) => (priority(c), Cinema.all.indexOf(c)) }
  }

  /** Derived view of every raw cinema-reported title that has scraped into
   *  this record. The cache stores per-(cinema, title, year) provenance in
   *  `cinemaScrapes`; the title set is just the projection. */
  def cinemaTitles: Set[String] = cinemaScrapes.map(_.title)

  /** Display title chosen across the cinema-reported variants plus the
   *  record's `cleanTitle` anchor (which is the post-decoration-strip form
   *  the cache keys the row by). Caller supplies cleanTitle because the
   *  record itself doesn't carry it — that anchor only exists on the
   *  surrounding CacheKey. Falls back to cleanTitle when there are no
   *  variants yet (TMDB resolved with no cinema scrape yet). */
  def displayTitle(cleanTitle: String): String =
    services.movies.TitleNormalizer.preferredDisplay((cinemaTitles + cleanTitle).toSeq)
      .getOrElse(cleanTitle)

  /** First non-empty poster URL with Multikino preferred. */
  def posterUrl: Option[String] =
    prioritizedShowings.iterator.flatMap(_._2.posterUrl).nextOption()

  /** Longest non-empty synopsis across cinemas (different cinemas write
   *  different-length blurbs; the longest tends to be the most complete). */
  def synopsis: Option[String] =
    cinemaShowings.values.flatMap(_.synopsis).toSeq.sortBy(-_.length).headOption

  /** Longest non-empty cast string across cinemas. Same rationale as
   *  `synopsis`. */
  def cast: Option[String] =
    cinemaShowings.values.flatMap(_.cast).toSeq.sortBy(-_.length).headOption

  /** First non-empty director with Multikino preferred. */
  def director: Option[String] =
    prioritizedShowings.iterator.flatMap(_._2.director).nextOption()

  /** First non-None runtime (cinemas tend to agree; if not, prefer Multikino). */
  def runtimeMinutes: Option[Int] =
    prioritizedShowings.iterator.flatMap(_._2.runtimeMinutes).nextOption()

  /** First non-None release year across cinemas. */
  def releaseYear: Option[Int] =
    prioritizedShowings.iterator.flatMap(_._2.releaseYear).nextOption()

  /** Production country (or co-production list) — first non-None cinema in
   *  priority order. Cinemas that don't parse a country (Multikino, Cinema
   *  City, Charlie Monroe, Kino Apollo) contribute None and are skipped, so
   *  whichever cinema does carry one fills the merged value. */
  def country: Option[String] =
    prioritizedShowings.iterator.flatMap(_._2.country).nextOption()

  /** Cinema → film deep-link, when that cinema reports one. */
  def filmUrlFor(cinema: Cinema): Option[String] = cinemaShowings.get(cinema).flatMap(_.filmUrl)

  /** Cinema → showtimes (empty when that cinema isn't screening). */
  def showtimesFor(cinema: Cinema): Seq[Showtime] =
    cinemaShowings.get(cinema).map(_.showtimes).getOrElse(Seq.empty)

  /** Cinema-reported original/international title — first non-empty with
   *  Multikino preferred. Separate from `originalTitle` (the TMDB-resolved
   *  production-language title): this is what the cinema's own API exposed,
   *  used as a fallback hint when `reEnrich` triggers a fresh TMDB search. */
  def cinemaOriginalTitle: Option[String] =
    prioritizedShowings.iterator.flatMap(_._2.originalTitle).nextOption()

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
