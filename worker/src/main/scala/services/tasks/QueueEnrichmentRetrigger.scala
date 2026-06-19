package services.tasks

import models.MovieRecord
import play.api.Logging
import services.freshness.{FreshnessKind, FreshnessStore}
import services.movies.{CacheKey, EnrichmentRetrigger, MovieService, RetriggerKind}

/**
 * Turns the cache's per-case merge retriggers into worker TASKS. When a merge
 * changed an enrichment's input fields, [[services.movies.MovieCache]] calls
 * this with the affected [[RetriggerKind]]s; for each, we enqueue the matching
 * task — one per case, never a blanket re-fetch-all.
 *
 * Ratings dedup/freshness is keyed on the stable tmdbId, so the periodic reaper
 * would otherwise skip a re-enqueue as still-fresh (the stamp predates the
 * merge). We INVALIDATE the stamp first so the handler's freshness re-gate sees
 * the work as due and actually re-fetches with the merge's corrected inputs.
 *
 * TMDB resolve + IMDb-id resolve aren't freshness-gated (the queue's per-dedupKey
 * idempotency is their guard), so they're a plain enqueue.
 */
class QueueEnrichmentRetrigger(queue: TaskQueue, freshness: FreshnessStore) extends EnrichmentRetrigger with Logging {

  override def retrigger(key: CacheKey, record: MovieRecord, kinds: Set[RetriggerKind]): Unit = {
    kinds.foreach {
      case RetriggerKind.ResolveTmdb =>
        queue.enqueue(
          TaskType.ResolveTmdb,
          EnrichTaskKeys.resolveTmdbDedup(key.cleanTitle, key.year),
          EnrichTaskKeys.resolveTmdbPayload(key.cleanTitle, key.year, record.director.headOption, record.originalTitle))
      case RetriggerKind.ResolveImdbId =>
        queue.enqueue(
          TaskType.ResolveImdbId,
          EnrichTaskKeys.resolveImdbIdDedup(key.cleanTitle, key.year),
          EnrichTaskKeys.resolveImdbIdPayload(key.cleanTitle, key.year, searchTitleOf(key, record)))
      case RetriggerKind.ImdbRating    => refreshRating(key, record, TaskType.ImdbRating,    FreshnessKind.ImdbRating)
      case RetriggerKind.FilmwebRating => refreshRating(key, record, TaskType.FilmwebRating, FreshnessKind.FilmwebRating)
      case RetriggerKind.RtRating      => refreshRating(key, record, TaskType.RtRating,      FreshnessKind.RtRating)
      case RetriggerKind.McRating      => refreshRating(key, record, TaskType.McRating,      FreshnessKind.McRating)
    }
    logger.info(s"Merge retrigger for '${key.cleanTitle}' (${key.year.getOrElse("—")}): ${kinds.map(_.toString).toSeq.sorted.mkString(", ")}")
  }

  private def refreshRating(key: CacheKey, record: MovieRecord, taskType: TaskType, kind: FreshnessKind): Unit = {
    val dedup = RatingTasks.dedupKey(kind, key, record.tmdbId)
    freshness.invalidate(dedup)
    queue.enqueue(taskType, dedup, RatingTasks.payload(key))
  }

  /** The title the IMDb-id lookup queries with — same derivation as
   *  `MovieService.publishTmdbOutcome`: the row's stable searchTitle, else its
   *  TMDB original title, else the api-query form of the clean title. */
  private def searchTitleOf(key: CacheKey, record: MovieRecord): String =
    record.searchTitle.orElse(record.originalTitle).getOrElse(MovieService.searchQuery(key.cleanTitle))
}
