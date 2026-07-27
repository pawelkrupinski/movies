package models

case class CinemaMovie(
  movie:       Movie,
  cinema:      Cinema,
  posterUrl:   Option[String],
  filmUrl:     Option[String],
  synopsis:    Option[String],
  cast:        Seq[String],
  director:    Seq[String],
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
  ageRating:   Option[String]      = None
)
