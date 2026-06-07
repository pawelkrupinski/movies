package services.cinemas

import models.{Cinema, SourceData}

/**
 * The per-film detail fields a cinema fetches *separately* from its listing —
 * synopsis, cast, director, runtime, genres, countries, poster, trailer. These
 * are what gets deferred out of `fetch()` and filled in later by an
 * `EnrichDetails` task.
 */
case class FilmDetail(
  synopsis:       Option[String] = None,
  cast:           Seq[String]    = Seq.empty,
  director:       Seq[String]    = Seq.empty,
  runtimeMinutes: Option[Int]    = None,
  releaseYear:    Option[Int]    = None,
  countries:      Seq[String]    = Seq.empty,
  genres:         Seq[String]    = Seq.empty,
  posterUrl:      Option[String] = None,
  trailerUrl:     Option[String] = None
) {
  /** Merge these detail fields into an existing cinema `SourceData` slot,
   *  preserving the slot's showtimes/title/filmUrl and never overwriting a
   *  present value with an empty one. */
  def mergeInto(slot: SourceData): SourceData = slot.copy(
    synopsis       = synopsis.orElse(slot.synopsis),
    cast           = if (cast.nonEmpty) cast else slot.cast,
    director       = if (director.nonEmpty) director else slot.director,
    runtimeMinutes = runtimeMinutes.orElse(slot.runtimeMinutes),
    releaseYear    = releaseYear.orElse(slot.releaseYear),
    countries      = if (countries.nonEmpty) countries else slot.countries,
    genres         = if (genres.nonEmpty) genres else slot.genres,
    posterUrl      = posterUrl.orElse(slot.posterUrl),
    trailerUrl     = trailerUrl.orElse(slot.trailerUrl)
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
 * per film. `cinema` is the `SourceData` slot the fetched detail is written to.
 */
trait DetailEnricher {
  def cinema: Cinema
  def detailGroup: String
  /** Fetch + parse one film's detail by the reference the listing scrape left on
   *  the movie (its `filmUrl`). None on failure/absence, so the task stays
   *  stale and is retried rather than recording an empty result as fresh. */
  def fetchFilmDetail(ref: String): Option[FilmDetail]
}
