package models

/**
 * Fully-resolved, read-only projection of a film's cross-cinema metadata —
 * every merged value materialised from a [[MovieRecord]]'s per-source `data`
 * map, with the source data itself dropped. One document per film in the
 * `web_movies` collection.
 *
 * The web read path serves from these instead of re-deriving the merge on
 * every request, and — crucially — a `web_movies` document changes only when a
 * film's metadata/enrichment actually changes. A showtime-only edit touches no
 * field here, so the web's change-stream delta for it carries nothing from this
 * collection (the screening doc alone moves). See [[CityScreening]].
 *
 * `_id` is the same identity as the source `movies` row (`sanitize(title)|year`
 * — see `ReadModelProjection.filmId`), so a screening doc's `filmId` joins
 * straight back to it.
 */
case class ResolvedMovie(
  _id:                String,
  title:              String,
  // Genuinely-distinct original (production-language) title — already filtered
  // for the case where it merely repeats the display title — so clients render
  // it unconditionally. None when there's nothing worth showing.
  originalTitle:      Option[String],
  posterUrl:          Option[String],
  fallbackPosterUrls: Seq[String],
  runtimeMinutes:     Option[Int],
  releaseYear:        Option[Int],
  genres:             Seq[String],
  countries:          Seq[String],
  directors:          Seq[String],
  cast:               Seq[String],
  synopsis:           Option[String],
  // Ready-to-embed trailer URLs (raw URLs already mapped through
  // `TrailerEmbed.embedUrlFor` and deduped), so clients embed without further
  // transformation.
  trailerUrls:        Seq[String],
  ratings:            ResolvedRatings,
  // Equal-weight average of the present ratings on a 0–10 scale — the grid's
  // "Ocena" sort key (rendered into the card's `data-rating`).
  weightedRating:     Double
) {
  /** Readable alias for the Mongo `_id`. */
  def id: String = _id
}

/**
 * The four external ratings plus their resolved click-through URLs. The
 * Metacritic / Rotten Tomatoes / Filmweb URLs are always populated (each falls
 * back to a search URL when we hold no direct link), so they're plain strings;
 * the IMDb URL is `None` until an IMDb id is known. Numeric scores stay
 * optional. Mirrors the `ApiRatings` shape the web emits.
 */
case class ResolvedRatings(
  imdb:              Option[Double],
  imdbUrl:           Option[String],
  metascore:         Option[Int],
  metacriticUrl:     String,
  rottenTomatoes:    Option[Int],
  rottenTomatoesUrl: String,
  filmweb:           Option[Double],
  filmwebUrl:        String
)
