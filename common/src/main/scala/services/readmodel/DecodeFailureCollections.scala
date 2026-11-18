package services.readmodel

import services.movies.{DecodeFailureMetrics, ScreeningsRepository, SlotsRepository}

/** Every collection whose undecodable documents [[DecodeFailureMetrics]] counts, the read model's and the corpus's,
 *  seeded at 0 so the first failure moves `increase()`. The last three are counted only by their change streams,
 *  which skip a post-image they cannot decode. */
object DecodeFailureCollections {
  val all: Seq[String] =
    Seq(MongoReadModelRepository.MoviesCollection, MongoReadModelRepository.ScreeningsCollection,
      DecodeFailureMetrics.SourceMoviesCollection, ScreeningsRepository.Collection, SlotsRepository.Collection, "userStates")
}
