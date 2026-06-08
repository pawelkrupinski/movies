package services.cinemas

import models.{Cinema, CinemaMovie}
import play.api.Logging
import services.events.{EventBus, MovieRecordCreated}
import services.movies.{CacheKey, MovieCache}

/**
 * The per-cinema scrape core: fetch a cinema's current listings, write them
 * through `MovieCache`, and publish a `MovieRecordCreated` for each genuinely
 * new (cinema, title, year). `recordCinemaScrape` additionally publishes a
 * `CinemaMovieAdded` per new film, which is what drives deferred per-film detail
 * — a `DetailTaskEnqueuer` per cinema enqueues the `EnrichDetails` task off that
 * event, with `DetailReaper` as the periodic refresh/retry backstop. The runner
 * itself no longer enqueues anything.
 *
 * Shared by both schedulers so the "what happens for one cinema" rule lives in
 * one place: the legacy continuous-loop `ShowtimeCache` and the queue-driven
 * `ScrapeCinemaHandler` both call this. It deliberately does NOT catch scrape
 * failures — each caller decides what a failure means (the loop logs and moves
 * on; the handler logs and lets the reaper retry).
 */
class CinemaScrapeRunner(
  movieCache: MovieCache,
  bus:        EventBus
) extends Logging {

  def run(scraper: CinemaScraper): Seq[(CinemaMovie, CacheKey, Boolean)] = {
    val cinema: Cinema = scraper.cinema
    val t0     = System.currentTimeMillis()
    val movies = scraper.fetch()
    val touched     = movieCache.recordCinemaScrape(cinema, movies)
    val publishable = touched.count(_._3)
    val elapsed     = System.currentTimeMillis() - t0
    logger.info(s"Refreshed ${cinema.displayName}: ${movies.size} entries in ${elapsed}ms ($publishable new)")
    touched.foreach { case (cm, key, isNew) =>
      if (isNew)
        bus.publish(MovieRecordCreated(
          key.cleanTitle, key.year, cm.movie.originalTitle,
          if (cm.director.nonEmpty) Some(cm.director.mkString(", ")) else None))
    }
    touched
  }
}
