package services.cinemas

import models.{Cinema, CinemaMovie}
import play.api.Logging
import services.events.{EventBus, MovieRecordCreated}
import services.freshness.{FreshnessKind, FreshnessStore, InMemoryFreshnessStore}
import services.movies.{CacheKey, MovieCache}
import services.tasks.{EnrichDetailsTasks, InMemoryTaskQueue, TaskQueue, TaskType}

/**
 * The per-cinema scrape core: fetch a cinema's current listings, write them
 * through `MovieCache`, publish a `MovieRecordCreated` for each genuinely new
 * (cinema, title, year), and — for a cinema that defers its per-film detail —
 * enqueue an `EnrichDetails` task per scraped film.
 *
 * Shared by both schedulers so the "what happens for one cinema" rule lives in
 * one place: the legacy continuous-loop `ShowtimeCache` and the queue-driven
 * `ScrapeCinemaHandler` both call this. Putting the detail-enqueue here (rather
 * than in `ScrapeCinemaHandler`) means deferred detail works regardless of which
 * scrape scheduler is active — the queue is the single, server-spanning detail
 * mechanism. It deliberately does NOT catch scrape failures — each caller
 * decides what a failure means (the loop logs and moves on; the handler logs and
 * lets the reaper retry).
 *
 * `detailEnrichers` is keyed by cinema display name. It is non-empty only when
 * deferred detail is enabled (the composition root passes the catalogue's
 * `DetailEnricher`s); otherwise empty, so nothing is enqueued and clients enrich
 * inline as before.
 */
class CinemaScrapeRunner(
  movieCache:      MovieCache,
  bus:             EventBus,
  queue:           TaskQueue                   = new InMemoryTaskQueue,
  freshness:       FreshnessStore              = new InMemoryFreshnessStore,
  detailEnrichers: Map[String, DetailEnricher] = Map.empty
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
    enqueueDetailTasks(cinema, touched)
    touched
  }

  /** Enqueue one `EnrichDetails` task per scraped film that carries a detail
   *  reference and isn't already detail-fresh. The freshness pre-check just
   *  avoids queue churn; the queue's unique index is the real guarantee a
   *  `(group, film)` detail task can't be queued twice (across servers). */
  private def enqueueDetailTasks(cinema: Cinema, touched: Seq[(CinemaMovie, CacheKey, Boolean)]): Unit =
    detailEnrichers.get(cinema.displayName).foreach { enricher =>
      touched.foreach { case (cm, key, _) =>
        cm.filmUrl.foreach { ref =>
          val dk = EnrichDetailsTasks.dedupKey(enricher.detailGroup, key)
          if (!freshness.isFresh(dk, FreshnessKind.DetailEnrich))
            queue.enqueue(TaskType.EnrichDetails, dk, EnrichDetailsTasks.payload(enricher, key, ref))
        }
      }
    }
}
