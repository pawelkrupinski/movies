package services.identity

/** A poster to hash: a venue's, by its URL, or a TMDB film's. */
enum PosterQuestion {
  case Venue(url: String)
  case Film(tmdbId: Int)
}
