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
  // TMDB's English release title (the en-US `title`, via `TmdbClient.englishTitle`).
  // Tmdb slot only; `None` on cinema/Imdb slots. Captured so a film whose Polish
  // `title` and non-Latin `originalTitle` both differ from the English title a
  // cinema lists it under ("Left-Handed Girl") still folds onto one row — see
  // `MovieRecord.tmdbTitleAliases`.
  englishTitle:   Option[String]  = None,
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
  // Genre names in this source's own language ("Komedia" / "Komödie"). One
  // entry per genre, verbatim per source — each source has its own taxonomy
  // and spelling. `MovieRecord.genres` picks the first non-empty list in
  // source-priority order (TMDB → Filmweb → cinemas) rather than unioning, so
  // the row shows a single coherent taxonomy.
  genres:         Seq[String]     = Seq.empty,
  posterUrl:      Option[String]  = None,
  // Cinema-only: empty for `Tmdb` / `Imdb` slots.
  filmUrl:        Option[String]  = None,
  // Cinema-only: a video URL the cinema's own page surfaces for the film,
  // most commonly a YouTube watch / embed link. Other shapes (vimeo, raw
  // mp4, brightcove) are accepted verbatim; the view layer normalises to
  // an embed URL at display time via `TrailerEmbed.embedUrlFor`.
  trailerUrl:     Option[String]  = None,
  showtimes:      Seq[Showtime]   = Seq.empty,
  // BCP-47 tag of the language this slot's localized text (title, synopsis,
  // genres, country names) was fetched in — `Tmdb` slot only; `None` on cinema
  // and `Imdb` slots, which carry their site's own fixed language.
  //
  // Exists so a slot frozen in the WRONG language can be DETECTED. TMDB details
  // are fetched only at resolve time, so a row resolved before its deployment
  // learned to enrich in its own language (pre-`d0bf10d8e`) kept Polish text
  // forever — Berlin showing "Familijny, Komedia" and a Polish title. The
  // reaper re-resolves any slot whose tag ≠ the deployment's language; `None`
  // means pl-PL, the historical hardcoded default, so Polish rows are already
  // correct and never churn. See `UnresolvedTmdbReaper.staleLanguage`.
  language:       Option[String]  = None,
  // CACHE-ONLY, NEVER PERSISTED. The worker's MovieCache strips `showtimes` (they live
  // in Mongo `screenings`) and keeps this digest so the write-guard + screenings-diff
  // still detect showtime changes without the lists resident. `None` everywhere else.
  showtimesDigest: Option[Int]    = None
) {
  // Record IDENTITY / metadata equality is showtime-AGNOSTIC: canonicalize / settle /
  // divert compare records to decide film identity + row structure, which never depend
  // on showtimes — and a cache record is stripped (Nil showtimes + a digest) while a
  // fresh scrape is full, so a showtime-sensitive `==` would make them differ forever
  // (endless re-divert/re-fold churn). Showtime-CHANGE detection routes through the
  // digest (`ShowtimesDigest.leanEqual` / `slotOps`), never `==`. So `showtimes` and the
  // transient `showtimesDigest` are excluded from equals/hashCode.
  //
  // `language` IS included, unlike those two: the cache write-guard skips the
  // repository write when the new record `==` the stored one, so a re-resolve that
  // corrected ONLY the language stamp (TMDB returning the same text under a freshly
  // -confirmed tag) would never persist — and the row would be re-swept forever.
  override def equals(that: Any): Boolean = that match {
    case o: SourceData =>
      title == o.title && rawTitle == o.rawTitle && originalTitle == o.originalTitle &&
      englishTitle == o.englishTitle && synopsis == o.synopsis && cast == o.cast &&
      director == o.director && runtimeMinutes == o.runtimeMinutes && releaseYear == o.releaseYear &&
      countries == o.countries && genres == o.genres && posterUrl == o.posterUrl &&
      filmUrl == o.filmUrl && trailerUrl == o.trailerUrl && language == o.language
    case _ => false
  }
  override def hashCode(): Int =
    (title, rawTitle, originalTitle, englishTitle, synopsis, cast, director,
     runtimeMinutes, releaseYear, countries, genres, posterUrl, filmUrl, trailerUrl,
     language).hashCode()

  /** The BCP-47 tag this slot's localized text was actually fetched in, reading an
   *  unstamped slot as the historical hardcoded default. Every caller that compares
   *  a slot's language against a deployment's wants this, not the raw `language`:
   *  `None` is not "unknown", it is `pl-PL`. */
  def fetchedLanguageTag: String = language.getOrElse(SourceData.LegacyLanguageTag)
}

object SourceData {
  /** What an unstamped `Tmdb` slot was fetched in: every resolve before the
   *  per-country enrichment language landed hardcoded `pl-PL`. Reading `None` as
   *  this (rather than "unknown, re-resolve") keeps the Polish corpus — where the
   *  stamp is absent and the text is already right — completely still. */
  val LegacyLanguageTag: String = "pl-PL"
}
