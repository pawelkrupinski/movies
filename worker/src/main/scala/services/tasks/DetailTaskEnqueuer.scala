package services.tasks

import services.events.{CinemaMovieAdded, DomainEvent}
import services.freshness.FreshnessStore
import services.movies.MovieCacheReader
import services.cinemas.common.DetailEnricher

/**
 * One per deferred cinema ("one handler per movie client"): on the first
 * appearance of a film in THIS cinema's listing (`CinemaMovieAdded`), enqueue
 * its `EnrichDetails` task so the detail is fetched promptly rather than waiting
 * for the next [[DetailReaper]] tick.
 *
 * This is purely the immediacy path. Refresh and retry are the reaper's job:
 * `CinemaMovieAdded` fires only on a film's *first* observation (it can also be
 * lost across a restart between publish and consumption), so it can't be the
 * sole trigger. The enqueuer filters by `event.cinema == enricher.cinema`, so a
 * cinema's enqueuer only ever acts on its own movies.
 */
class DetailTaskEnqueuer(
  enricher:  DetailEnricher,
  cache:     MovieCacheReader,
  queue:     TaskQueue,
  freshness: FreshnessStore
) {

  val onCinemaMovieAdded: PartialFunction[DomainEvent, Unit] = {
    case CinemaMovieAdded(c, title, year, Some(ref)) if c == enricher.cinema =>
      EnrichDetailsTasks.enqueueIfStale(queue, freshness, enricher, cache.keyOf(title, year), ref)
      ()
  }
}
