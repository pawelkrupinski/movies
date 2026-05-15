package models

/**
 * One cinema's contribution to a film record: every field this cinema
 * reported on its most recent scrape tick, kept verbatim per-cinema. The
 * merged top-level values on `MovieRecord` (posterUrl, synopsis, …) are
 * derived from these by iterating across cinemas — so when a cinema drops
 * a listing, removing its slot here automatically updates the derived
 * values on read with no separate sync step.
 *
 * Stored as `Map[Cinema, CinemaShowings]` on `MovieRecord`. Each cinema's
 * slot is replaced wholesale on every scrape tick — a film that drops out
 * of cinema C's listings has its `cinemaShowings(C)` slot removed during
 * the prune phase.
 */
case class CinemaShowings(
  filmUrl:        Option[String],
  posterUrl:      Option[String],
  synopsis:       Option[String],
  cast:           Option[String],
  director:       Option[String],
  runtimeMinutes: Option[Int],
  releaseYear:    Option[Int],
  // The cinema's own English/international title — captured because `reEnrich`
  // uses it as a TMDB-search hint for films whose Polish title resolves poorly.
  originalTitle:  Option[String]   = None,
  showtimes:      Seq[Showtime]
)
