package services.tasks

import models.MovieRecord
import play.api.Logging
import services.movies.CacheKey
import services.movies.{EnrichmentRetrigger, RetriggerKind}
import services.titles.TitleNormalizer

/**
 * Turns the cache's per-case merge retriggers into worker TASKS. When a merge
 * changed an enrichment's input fields, [[services.movies.MovieCache]] calls
 * this with the affected [[RetriggerKind]]s; for each, we enqueue the matching
 * task — one per case, never a blanket re-fetch-all ([[RetriggerTask]], put on the queue by [[RetriggerEnqueuer]]).
 *
 * The merge decision ([[services.movies.MergeRetrigger]]) lives in `common` and is
 * country-BLIND: it emits `FilmwebRating` for any resolved row whose rating inputs
 * changed, unaware whether this deployment's country wires a Filmweb handler. So
 * this sink applies the same country gate the reaper/census do — the country's [[RatingSources.forCountry]],
 * handed in by the root — and DROPS a rating retrigger whose source doesn't apply here. Without it, a
 * non-Filmweb country (UK) enqueues a handler-less `FilmwebRating` task that
 * [[TaskWorker]] re-releases forever ("no handler for FilmwebRating"), hot-looping
 * in `waiting` — exactly the backlog [[RatingSources]] documents the gate exists to
 * prevent, reached through the one enqueue path that used to skip it.
 */
class QueueEnrichmentRetrigger(
  enqueuer: RetriggerEnqueuer,
  // The rating sources THIS country wires (`RatingSources.forCountry`) — the single source of truth shared with the
  // reaper + census. A rating retrigger for a task type outside them has no handler here, so it is never enqueued.
  sources: Seq[RatingSources.RatingSource],
  // The country's title rules, for the IMDb-id lookup's search title.
  normalizer: TitleNormalizer
) extends EnrichmentRetrigger with Logging {

  override def retrigger(key: CacheKey, record: MovieRecord, kinds: Set[RetriggerKind]): Unit = {
    val (applicable, dropped) = kinds.partition(RetriggerTask.of(_).appliesWith(sources))
    if (dropped.nonEmpty)
      logger.info(s"Dropping ${dropped.map(_.toString).toSeq.sorted.mkString(", ")} retrigger for " +
        s"'${key.cleanTitle}' — source not enabled in this country")
    applicable.foreach(kind => enqueuer.enqueue(RetriggerTask.of(kind), key, record.tmdbId, searchTitleOf(key, record)))
    if (applicable.nonEmpty)
      logger.info(s"Merge retrigger for '${key.cleanTitle}' (${key.year.getOrElse("—")}): ${applicable.map(_.toString).toSeq.sorted.mkString(", ")}")
  }

  /** The title the IMDb-id lookup queries with — same derivation as
   *  `MovieService.publishTmdbOutcome`: the row's stable searchTitle, else its
   *  TMDB original title, else the api-query form of the clean title. */
  private def searchTitleOf(key: CacheKey, record: MovieRecord): String =
    record.searchTitle.orElse(record.originalTitle).getOrElse(normalizer.searchQuery(key.cleanTitle))
}
