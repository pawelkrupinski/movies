package models

/**
 * One source's contribution to a film record — every field this source
 * reported on its most recent scrape / API call, kept verbatim per-source.
 * Stored as `Map[Source, SourceData]` on `MovieRecord`. Each source's slot
 * is replaced wholesale on the next refresh tick; the merged accessors on
 * `MovieRecord` derive their values by iterating across the map.
 *
 * Cinema slots populate every field that's available from the scrape;
 * `Tmdb`/`Imdb` slots populate the content fields they expose and leave
 * `filmUrl` / `showtimes` empty (those are cinema-only concepts).
 *
 * A film that drops out of a cinema's listings has that cinema's slot
 * removed during the prune phase of `recordCinemaScrape`; the rest of the
 * map stays intact, so the row keeps its TMDB / IMDb data while
 * `cinemaShowings` empties.
 */
case class SourceData(
  title:          Option[String]  = None,
  originalTitle:  Option[String]  = None,
  synopsis:       Option[String]  = None,
  cast:           Option[String]  = None,
  director:       Option[String]  = None,
  runtimeMinutes: Option[Int]     = None,
  releaseYear:    Option[Int]     = None,
  // Production countries — one entry per country, kept verbatim per source.
  // Each source spells names in its own way ("USA" vs "Stany Zjednoczone");
  // the merged-record accessor on `MovieRecord` unions across sources in
  // priority order. Stored already-canonicalised by `recordCinemaScrape`.
  countries:      Seq[String]     = Seq.empty,
  posterUrl:      Option[String]  = None,
  // Cinema-only: empty for `Tmdb` / `Imdb` slots.
  filmUrl:        Option[String]  = None,
  showtimes:      Seq[Showtime]   = Seq.empty
)
