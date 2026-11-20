package models

// A listing names its film, venue and showtimes; whatever else it carries is optional, empty where the venue does
// not say it (TMDB and the detail enrichment fill the rest downstream).
case class CinemaMovie(
  movie:       Movie,
  cinema:      Cinema,
  posterUrl:   Option[String]      = None,
  filmUrl:     Option[String]      = None,
  synopsis:    Option[String]      = None,
  cast:        Seq[String]         = Seq.empty,
  director:    Seq[String]         = Seq.empty,
  showtimes:   Seq[Showtime],
  externalIds: Map[String, String] = Map.empty,
  // URL of a video the cinema page surfaces for the film (YouTube watch / embed,
  // vimeo, raw mp4 — whatever the upstream returns, stored verbatim). Empty for
  // cinemas that don't expose one. Normalised to an embed URL by the view layer
  // via `TrailerEmbed.embedUrlFor`.
  trailerUrl:  Option[String]      = None,
  // Age rating / certificate as the cinema labels it (UK BBFC "15"/"PG"/"12A"/…).
  // Set by the clients whose listing or detail page exposes it; `None` otherwise.
  // Flows to `SourceData.ageRating` and then `MovieRecord.ageRating` (cinema-first).
  ageRating:   Option[String]      = None,
  // The venue's own synopsis where the venue CUT it short (MSI's month page stops a long Description at
  // ~300 characters with "..."): for MATCHING only, never displayed — a half-sentence blurb, being the venue's,
  // would outrank a whole TMDB one on the page, so `synopsis` stays empty for it, while the people its first
  // 300 characters name are still identity evidence. Archived with the listing; never reaches a slot, the
  // read model or the page. `None` wherever the venue's synopsis is whole (it is `synopsis` then).
  synopsisExcerpt: Option[String]  = None
)
