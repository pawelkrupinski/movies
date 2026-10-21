package services.movies

import models.{Cinema, CinemaMovie, CinemaShowing, KinoApollo, Movie, MovieRecord, Showtime}

import java.time.LocalDateTime

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

  /** One film listed at a single venue under `title`, then `edit`ed — a row the rating specs shape
   *  (its ids, its ratings) without caring which venue screens it. */
  def row(cache: CaffeineMovieCache, title: String)(edit: MovieRecord => MovieRecord): Unit = {
    land(cache, KinoApollo, Seq(CinemaMovie(
      Movie(title), KinoApollo, None, None, None, Seq.empty, Seq.empty,
      Seq(Showtime(LocalDateTime.of(2026, 6, 8, 18, 0), Some("https://book"))))))
    cache.putIfPresent(cache.keyOf(title, None), edit)
    ()
  }
}
