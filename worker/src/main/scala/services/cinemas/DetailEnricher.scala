package services.cinemas

import models.{Cinema, CinemaMovie, Source, SourceData}

/**
 * The per-film detail fields a cinema fetches *separately* from its listing —
 * synopsis, cast, director, runtime, original title, genres, countries, poster,
 * trailer. These are what gets deferred out of `fetch()` and filled in later by
 * an `EnrichDetails` task. Detail is purely enrichment: a deferred client always
 * carries its showtimes on the bare movie (showtimes are identity-bearing, so a
 * client whose showtimes live behind a separate fetch keeps that fetch inline —
 * Rialto's event pages, Helios's screens — rather than deferring them here).
 */
case class FilmDetail(
  synopsis:       Option[String] = None,
  cast:           Seq[String]    = Seq.empty,
  director:       Seq[String]    = Seq.empty,
  runtimeMinutes: Option[Int]    = None,
  releaseYear:    Option[Int]    = None,
  originalTitle:  Option[String] = None,
  countries:      Seq[String]    = Seq.empty,
  genres:         Seq[String]    = Seq.empty,
  posterUrl:      Option[String] = None,
  trailerUrl:     Option[String] = None
) {
  // Both merges treat the LISTING/bare values as authoritative and let the
  // detail only FILL GAPS (a present listing value is never replaced by a detail
  // one). For every field except poster the bare movie is empty, so this is
  // identical to "detail fills it in"; for poster it preserves the cinema's own
  // listing poster (which several clients prefer over the detail-page poster) —
  // matching the pre-split per-client merges.

  /** Fill gaps in an existing cinema `SourceData` slot from these detail fields,
   *  preserving the slot's showtimes/title/filmUrl. Used by the deferred (queue)
   *  path. */
  def mergeInto(slot: SourceData): SourceData = slot.copy(
    synopsis       = slot.synopsis.orElse(synopsis),
    cast           = if (slot.cast.nonEmpty) slot.cast else cast,
    director       = if (slot.director.nonEmpty) slot.director else director,
    runtimeMinutes = slot.runtimeMinutes.orElse(runtimeMinutes),
    releaseYear    = slot.releaseYear.orElse(releaseYear),
    originalTitle  = slot.originalTitle.orElse(originalTitle),
    countries      = if (slot.countries.nonEmpty) slot.countries else countries,
    genres         = if (slot.genres.nonEmpty) slot.genres else genres,
    posterUrl      = slot.posterUrl.orElse(posterUrl),
    trailerUrl     = slot.trailerUrl.orElse(trailerUrl)
  )

  /** Fill gaps in a bare `CinemaMovie` from these detail fields. Used by the
   *  inline path so every client's "merge detail onto the movie" rule lives here
   *  rather than being re-spelled per client. */
  def applyTo(cm: CinemaMovie): CinemaMovie = cm.copy(
    movie = cm.movie.copy(
      runtimeMinutes = cm.movie.runtimeMinutes.orElse(runtimeMinutes),
      releaseYear    = cm.movie.releaseYear.orElse(releaseYear),
      originalTitle  = cm.movie.originalTitle.orElse(originalTitle),
      countries      = if (cm.movie.countries.nonEmpty) cm.movie.countries else countries,
      genres         = if (cm.movie.genres.nonEmpty) cm.movie.genres else genres
    ),
    synopsis   = cm.synopsis.orElse(synopsis),
    cast       = if (cm.cast.nonEmpty) cm.cast else cast,
    director   = if (cm.director.nonEmpty) cm.director else director,
    posterUrl  = cm.posterUrl.orElse(posterUrl),
    trailerUrl = cm.trailerUrl.orElse(trailerUrl)
  )
}

/**
 * A cinema (or chain) whose per-film detail can be fetched independently of its
 * listing scrape, so the detail is deferred to a queue task and deduped per
 * `(detailGroup, film)`. A `CinemaScraper` opts in by also extending this; its
 * `fetch()` then returns bare movies (showtimes + a `filmUrl` reference) and the
 * `EnrichDetails` task fills in the rest by calling `fetchFilmDetail`.
 *
 * `detailGroup` is the dedup/freshness scope: a standalone cinema uses its own
 * id, a chain uses the chain name so all its locations share one detail fetch
 * per film. `detailTarget` is the `SourceData` slot the fetched detail is
 * written to — by default the cinema's own slot, but a chain redirects it to a
 * single network-level source so the detail is stored once and shared across
 * every venue via `MovieRecord`'s film-level merged accessors.
 */
trait DetailEnricher {
  def cinema: Cinema
  def detailGroup: String
  /** The `SourceData` slot the fetched detail is merged into. Defaults to this
   *  cinema's own slot (1:1 case); a chain overrides it to a shared network
   *  source so all its venues read one detail fetch. */
  def detailTarget: Source = cinema
  /** Override the /uptime enrichment-health service name. None → the per-cinema
   *  `"<cinema>|enrichment"` sub-row (the default). A chain sets a single
   *  network-level name so it reports one global entry instead of one per
   *  venue. */
  def enrichmentServiceOverride: Option[String] = None
  /** Fetch + parse one film's detail by the reference the listing scrape left on
   *  the movie (its `filmUrl`). None on failure/absence, so the task stays
   *  stale and is retried rather than recording an empty result as fresh. */
  def fetchFilmDetail(ref: String): Option[FilmDetail]
}
