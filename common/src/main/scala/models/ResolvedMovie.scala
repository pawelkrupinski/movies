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
 * collection (the screening document alone moves). See [[CityScreening]].
 *
 * `_id` is the same identity as the source `movies` row (`sanitize(title)|year`
 * — see `ReadModelProjection.filmId`), so a screening document's `filmId` joins
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
  // City-INDEPENDENT synopsis fallback: the best blurb from TMDB/IMDb/Filmweb
  // only (no cinema). Served wherever `synopsisByCity` carries no override for
  // the city being viewed, so a city with no cinema blurb of its own shows
  // TMDB's rather than another city's cinema text. See `synopsisFor`.
  synopsis:           Option[String],
  // Ready-to-embed trailer URLs (raw URLs already mapped through
  // `TrailerEmbed.embedUrlFor` and deduped), so clients embed without further
  // transformation.
  trailerUrls:        Seq[String],
  ratings:            ResolvedRatings,
  // Equal-weight average of the present ratings on a 0–10 scale — the grid's
  // "Ocena" sort key (rendered into the card's `data-rating`).
  weightedRating:     Double,
  // Per-city synopsis overrides, keyed by `City.slug`: the best blurb chosen
  // among the cinemas screening this film IN that city plus TMDB/IMDb. Only
  // cities whose pick differs from the city-independent `synopsis` fallback are
  // stored (most don't), keeping the document small and a metadata change
  // confined to the affected cities. Read via `synopsisFor(city)`. Defaulted —
  // and placed last — so positional constructors and legacy `web_movies`
  // documents written before this field both stay valid (the codec ignores
  // absent fields, restoring this as `Map.empty`).
  synopsisByCity:     Map[String, String] = Map.empty
) {
  /** Readable alias for the Mongo `_id`. */
  def id: String = _id

  /** The synopsis to show for `city`: its per-city override when one was
   *  projected (a cinema there wrote a better blurb than the TMDB/IMDb
   *  fallback), else the city-independent `synopsis`. Never another city's
   *  cinema text — the fallback is cinema-free by construction. */
  def synopsisFor(city: City): Option[String] =
    synopsisByCity.get(city.slug).orElse(synopsis)
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
