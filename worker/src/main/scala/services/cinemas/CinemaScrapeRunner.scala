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
 * Shared so the "what happens for one cinema" rule lives in one place: the
 * queue-driven `ScrapeCinemaHandler` calls this for each scrape task, and the
 * fixture recorder calls it directly. It deliberately does NOT catch scrape
 * failures — each caller decides what a failure means (the handler logs and
 * lets the reaper retry).
 */
class CinemaScrapeRunner(
  movieCache: MovieCache,
  bus:        EventBus
) extends Logging {

  def run(scraper: CinemaScraper): Seq[(CinemaMovie, CacheKey, Boolean)] = {
    val cinema: Cinema = scraper.cinema
    val t0     = System.currentTimeMillis()
    val movies = scraper.fetch()
    val touched = movieCache.recordCinemaScrape(cinema, movies)
    val events  = CinemaScrapeRunner.eventsFor(touched)
    val elapsed = System.currentTimeMillis() - t0
    logger.info(s"Refreshed ${cinema.displayName}: ${movies.size} entries in ${elapsed}ms (${events.size} new)")
    events.foreach(bus.publish)
    touched
  }
}

object CinemaScrapeRunner {
  /** The `MovieRecordCreated` events a scrape's `touched` rows imply — one per
   *  genuinely-new `(cinema, title, year)`. A pure function so the two publish
   *  sites can't drift in how they derive an event from a slot: the prod runner
   *  publishes these inline as each cinema lands; the fixture harness's
   *  `runOneScrapeTick` defers them until every cinema is recorded (a
   *  deterministic single-pass settle). Both build the event the same way. */
  def eventsFor(touched: Seq[(CinemaMovie, CacheKey, Boolean)]): Seq[MovieRecordCreated] =
    touched.collect { case (cm, key, true) =>
      MovieRecordCreated(
        key.cleanTitle, key.year, cm.movie.originalTitle,
        if (cm.director.nonEmpty) Some(cm.director.mkString(", ")) else None)
    }
}
