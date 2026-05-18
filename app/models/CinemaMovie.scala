package models

case class CinemaMovie(
  movie:       Movie,
  cinema:      Cinema,
  posterUrl:   Option[String],
  filmUrl:     Option[String],
  synopsis:    Option[String],
  cast:        Option[String],
  director:    Option[String],
  showtimes:   Seq[Showtime],
  externalIds: Map[String, String] = Map.empty,
  // URL of a video the cinema page surfaces for the film (YouTube watch / embed,
  // vimeo, raw mp4 — whatever the upstream returns, stored verbatim). Empty for
  // cinemas that don't expose one. Normalised to an embed URL by the view layer
  // via `TrailerEmbed.embedUrlFor`.
  trailerUrl:  Option[String]      = None
)
