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
  // The cinema's reported title BEFORE any per-cinema rule stripping — the
  // verbatim upstream string. `title` above is the cleaned/display form; this
  // keeps the original so a merge key can be re-derived from scratch when the
  // stripping rules change (enabling backfill and surgical un-merge). Cinema
  // slots only; `None` on Tmdb/Imdb slots and on rows scraped before this field
  // existed (those re-populate on the next scrape).
  rawTitle:       Option[String]  = None,
  originalTitle:  Option[String]  = None,
  synopsis:       Option[String]  = None,
  cast:           Seq[String]     = Seq.empty,
  director:       Seq[String]     = Seq.empty,
  runtimeMinutes: Option[Int]     = None,
  releaseYear:    Option[Int]     = None,
  // Production countries — one entry per country, kept verbatim per source.
  // Each source spells names in its own way ("USA" vs "Stany Zjednoczone");
  // the merged-record accessor on `MovieRecord` unions across sources in
  // priority order. Stored already-canonicalised by `recordCinemaScrape`.
  countries:      Seq[String]     = Seq.empty,
  // Polish-language genre names ("Komedia", "Dramat"). One entry per genre,
  // verbatim per source — each source has its own taxonomy and spelling.
  // `MovieRecord.genres` picks the first non-empty list in source-priority
  // order (TMDB → Filmweb → cinemas) rather than unioning, so the row shows
  // a single coherent taxonomy.
  genres:         Seq[String]     = Seq.empty,
  posterUrl:      Option[String]  = None,
  // Cinema-only: empty for `Tmdb` / `Imdb` slots.
  filmUrl:        Option[String]  = None,
  // Cinema-only: a video URL the cinema's own page surfaces for the film,
  // most commonly a YouTube watch / embed link. Other shapes (vimeo, raw
  // mp4, brightcove) are accepted verbatim; the view layer normalises to
  // an embed URL at display time via `TrailerEmbed.embedUrlFor`.
  trailerUrl:     Option[String]  = None,
  showtimes:      Seq[Showtime]   = Seq.empty
)
