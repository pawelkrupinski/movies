package services.review

/** The mirror collections the review reads ([[MongoReviewSource]]'s roles) read, by the names their owners give them --
 *  one place, so the roles name a collection without naming the store that owns it. */
private[review] object ReviewCollections {
  val Slots         = services.movies.SlotsRepository.Collection
  val Movies        = services.movies.MovieRepository.Collection
  val Screenings    = services.movies.ScreeningsRepository.Collection
  val WebMovies     = services.readmodel.MongoReadModelRepository.MoviesCollection
  val VenuePages    = services.DebugMirror.VenuePages
  val Listings      = services.DebugMirror.IdentityListings
  val TmdbFilms     = services.DebugMirror.TmdbFilms
  val FamilyAnswers = services.DebugMirror.FamilyAnswers
}
