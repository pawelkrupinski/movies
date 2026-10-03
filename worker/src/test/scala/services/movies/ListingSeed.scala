package services.movies

import models.{Cinema, CinemaMovie, CinemaShowing, MovieRecord}

/** Seeds a cache with a venue's listing as the identity projection writes it: each film under its
 *  own title and year, the venue's slot for it built by the production [[CinemaSlotBuilder]] and
 *  keyed as the projection keys it. For specs of what READS the film rows (the detail and rating
 *  reapers, the detail handler), which used to seed them through the old scrape landing. */
object ListingSeed {
  def land(cache: CaffeineMovieCache, cinema: Cinema, movies: Seq[CinemaMovie]): Unit = {
    val slots = new CinemaSlotBuilder(java.util.Locale.ROOT, cache.stringPool)
    movies.foreach { cm =>
      val key   = cache.keyOf(cm.movie.title, cm.movie.releaseYear)
      val prior = cache.get(key).getOrElse(MovieRecord())
      cache.put(key, prior.copy(data = prior.data +
        (CinemaShowing.keyFor(cinema, cm.movie.title, cache.normalizer) -> slots.build(cm, cm.movie.title, None))))
    }
  }
}
