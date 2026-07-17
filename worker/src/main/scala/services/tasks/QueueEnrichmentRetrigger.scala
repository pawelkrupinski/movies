package services.tasks

import models.{Country, MovieRecord}
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
 *
 * The merge decision ([[services.movies.MergeRetrigger]]) lives in `common` and is
 * country-BLIND: it emits `FilmwebRating` for any resolved row whose rating inputs
 * changed, unaware whether this deployment's country wires a Filmweb handler. So
 * this sink applies the same country gate the reaper/census do — [[RatingSources.forCountry]]
 * — and DROPS a rating retrigger whose source doesn't apply here. Without it, a
 * non-Filmweb country (UK) enqueues a handler-less `FilmwebRating` task that
 * [[TaskWorker]] re-releases forever ("no handler for FilmwebRating"), hot-looping
 * in `waiting` — exactly the backlog [[RatingSources]] documents the gate exists to
 * prevent, reached through the one enqueue path that used to skip it.
 */
class QueueEnrichmentRetrigger(
  queue:     TaskQueue,
  freshness: FreshnessStore,
  // The country this sink serves — selects which rating sources apply, mirroring
  // [[RatingEnqueuer]]. Defaults to Poland so tests and single-country paths keep
  // the historical (Filmweb-on) behaviour; the worker wiring passes the actual
  // per-country value.
  country: Country = Country.default
) extends EnrichmentRetrigger with Logging {

  // The rating task types whose handler THIS country wires — the single source of
  // truth shared with the reaper + census (see RatingSources). A rating retrigger
  // for a task type outside this set has no handler here, so we never enqueue it.
  private val enabledRatingTaskTypes = RatingSources.forCountry(country).map(_.taskType).toSet

  // The task type a rating retrigger kind maps to, for the country gate above. The
  // non-rating kinds (resolve) apply in every country and aren't listed.
  private val ratingTaskTypeOf: Map[RetriggerKind, TaskType] = Map(
    RetriggerKind.ImdbRating    -> TaskType.ImdbRating,
    RetriggerKind.FilmwebRating -> TaskType.FilmwebRating,
    RetriggerKind.RtRating      -> TaskType.RtRating,
    RetriggerKind.McRating      -> TaskType.McRating)

  private def enabled(kind: RetriggerKind): Boolean =
    ratingTaskTypeOf.get(kind).forall(enabledRatingTaskTypes.contains)

  override def retrigger(key: CacheKey, record: MovieRecord, kinds: Set[RetriggerKind]): Unit = {
    val (applicable, dropped) = kinds.partition(enabled)
    if (dropped.nonEmpty)
      logger.info(s"Dropping ${dropped.map(_.toString).toSeq.sorted.mkString(", ")} retrigger for " +
        s"'${key.cleanTitle}' — source not enabled in ${country.code}")
    applicable.foreach {
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
    if (applicable.nonEmpty)
      logger.info(s"Merge retrigger for '${key.cleanTitle}' (${key.year.getOrElse("—")}): ${applicable.map(_.toString).toSeq.sorted.mkString(", ")}")
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
