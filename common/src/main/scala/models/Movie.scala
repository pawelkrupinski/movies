package models

case class Movie(
  title:          String,
  runtimeMinutes: Option[Int]       = None,
  releaseYear:    Option[Int]       = None,
  // Production countries — one entry per country, in the order the source
  // listed them. Each cinema spells names in its own way ("USA" vs "Stany
  // Zjednoczone", "Wielka Brytania" vs "UK") so this stays verbatim per
  // cinema; the merge logic on `MovieRecord` unifies across cinemas.
  countries:      Seq[String]       = Seq.empty,
  // Polish-language genre names ("Komedia", "Dramat", "Sci-Fi"). One entry
  // per genre, kept verbatim per source — each source has its own taxonomy
  // and spelling (TMDB's "Sci-Fi" vs Helios' "science fiction"); the
  // merge logic on `MovieRecord` picks the highest-priority non-empty
  // source rather than unioning across them, so this stays verbatim.
  genres:         Seq[String]       = Seq.empty,
  // English/international release title when the cinema's API exposes it
  // (Multikino does for niche international shows — Cirque du Soleil, opera,
  // English-language documents). Used as a TMDB-search fallback for titles whose
  // Polish translation doesn't index well; absent for most films, where the
  // Polish title is the canonical entry point.
  originalTitle:  Option[String]    = None,
  // The verbatim upstream title before the client's per-cinema cleanup, when the
  // client cleaned `title`. Carried so `recordCinemaScrape` can persist it as
  // `SourceData.rawTitle` and the merge key stays re-derivable from it when the
  // stripping rules change. None when the client did no cleanup (then `title` is
  // already the raw string).
  rawTitle:       Option[String]    = None
)
