package services.tasks

import services.events.{DomainEvent, ImdbIdMissing, ImdbIdResolved, TmdbResolved}
import services.freshness.FreshnessKind
import services.movies.MovieCacheReader

/**
 * Bus subscribers that ENQUEUE rating tasks instead of fetching inline — the
 * "keep the bus, subscribers enqueue" design. Wired onto the same TMDB/IMDb
 * resolution events the `*Ratings` classes used to listen to, so a newly
 * resolved film gets its ratings queued promptly (the reaper is the periodic
 * backstop). Enqueue is deduped per (source, film) by the queue's unique index,
 * so this is safe to fire on every event across every server.
 *
 * Lives in `services.tasks` (not the `modules` composition root) so it can build
 * the per-film `CacheKey` via `MovieCacheReader.keyOf` (package-private to `services`).
 */
class RatingEnqueuer(cache: MovieCacheReader, queue: TaskQueue) {

  // imdbId now known → fetch the IMDb rating.
  val onTmdbResolved: PartialFunction[DomainEvent, Unit] = {
    case TmdbResolved(title, year, _) =>
      enqueue(title, year, Seq(TaskType.ImdbRating -> FreshnessKind.ImdbRating) ++ tmdbDownstream)
  }

  // imdbId resolved out-of-band → fetch the IMDb rating.
  val onImdbIdResolved: PartialFunction[DomainEvent, Unit] = {
    case ImdbIdResolved(title, year, _) =>
      enqueue(title, year, Seq(TaskType.ImdbRating -> FreshnessKind.ImdbRating))
  }

  // TMDB hit but no IMDb cross-ref → the non-IMDb ratings can still go (IMDb
  // follows once ImdbIdResolver recovers the id and ImdbIdResolved fires).
  val onImdbIdMissing: PartialFunction[DomainEvent, Unit] = {
    case ImdbIdMissing(title, year, _) => enqueue(title, year, tmdbDownstream)
  }

  private val tmdbDownstream = Seq(
    TaskType.FilmwebRating -> FreshnessKind.FilmwebRating,
    TaskType.RtRating      -> FreshnessKind.RtRating,
    TaskType.McRating      -> FreshnessKind.McRating
  )

  private def enqueue(title: String, year: Option[Int], kinds: Seq[(TaskType, FreshnessKind)]): Unit = {
    val key = cache.keyOf(title, year)
    kinds.foreach { case (tt, kind) =>
      queue.enqueue(tt, RatingTasks.dedupKey(kind, key), RatingTasks.payload(key))
    }
  }
}
