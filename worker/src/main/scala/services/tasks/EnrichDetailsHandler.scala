package services.tasks

import models.SourceData
import play.api.Logging
import services.UptimeMonitor
import services.cinemas.DetailEnricher
import services.freshness.{FreshnessKind, FreshnessStore}
import services.movies.{CacheKey, MovieCache}

/** Builds the dedup key, freshness key, and payload for an `EnrichDetails` task.
 *  The dedup key encodes `(detailGroup, film)` so "enrich film F for group G"
 *  can exist at most once — the queue's unique index rejects a second. */
object EnrichDetailsTasks {
  val GroupKey  = "group"
  val RefKey    = "ref"
  val TitleKey  = "title"
  val YearKey   = "year"

  def dedupKey(group: String, key: CacheKey): String =
    s"detail|$group|${key.cleanTitle}|${key.year.map(_.toString).getOrElse("")}"

  def payload(enricher: DetailEnricher, key: CacheKey, ref: String): Map[String, String] =
    Map(
      GroupKey -> enricher.detailGroup,
      RefKey   -> ref,
      TitleKey -> key.cleanTitle,
      YearKey  -> key.year.map(_.toString).getOrElse("")
    )

  /** Enqueue a detail task for `(enricher's group, film)` unless it's already
   *  detail-fresh. The freshness pre-check just avoids queue churn; the queue's
   *  unique index is the real cross-server guarantee that a `(group, film)`
   *  detail task can't be queued twice. Returns true iff newly enqueued. Shared
   *  by every producer (the event enqueuer, the periodic reaper, the inline
   *  scrape path) so the enqueue rule lives in exactly one place. */
  def enqueueIfStale(queue: TaskQueue, freshness: FreshnessStore, enricher: DetailEnricher, key: CacheKey, ref: String): Boolean = {
    val dk = dedupKey(enricher.detailGroup, key)
    !freshness.isFresh(dk, FreshnessKind.DetailEnrich) &&
      queue.enqueue(TaskType.EnrichDetails, dk, payload(enricher, key, ref)) == EnqueueResult.Added
  }
}

/**
 * Handles an `EnrichDetails` task: fetch one film's detail (once per
 * `(detailGroup, film)` per 6h) and merge it into the enricher's `detailTarget`
 * `SourceData` slot, preserving anything already in that slot (e.g. a cinema
 * slot's showtimes). A 1:1 cinema targets its own slot; a chain targets a
 * shared network source, so one fetch serves every venue.
 *
 * Freshness-gated: skips if the detail was fetched inside the window. A failed
 * fetch (`fetchFilmDetail` → None) is reported `Done` without marking fresh, so
 * the cinema's next scrape re-enqueues it rather than the worker spinning.
 */
class EnrichDetailsHandler(
  enrichersByGroup: Map[String, DetailEnricher],
  cache:            MovieCache,
  freshness:        FreshnessStore,
  uptime:           UptimeMonitor
) extends TaskHandler with Logging {
  import HandlerOutcome._

  override val taskType: TaskType = TaskType.EnrichDetails

  override def handle(task: Task): HandlerOutcome = {
    val key = task.dedupKey
    if (freshness.isFresh(key, FreshnessKind.DetailEnrich)) return Skipped

    enrichersByGroup.get(task.payload.getOrElse(EnrichDetailsTasks.GroupKey, "")) match {
      case None =>
        logger.warn(s"No detail enricher for task $key; dropping.")
        Done
      case Some(enricher) =>
        // Record enrichment health on /uptime: a resolved detail is a success, an
        // absent/failed fetch a failure (red/yellow). A 1:1 cinema records under
        // its own "<cinema>|enrichment" sub-row; a chain overrides this to a
        // single network-level service (e.g. "Globalne: Cinema City").
        val service = enricher.enrichmentServiceOverride
          .getOrElse(UptimeMonitor.enrichmentService(enricher.cinema.displayName))
        enricher.fetchFilmDetail(task.payload.getOrElse(EnrichDetailsTasks.RefKey, "")) match {
          case None =>
            uptime.recordFailure(service, s"detail fetch returned nothing for ${task.payload.getOrElse(EnrichDetailsTasks.TitleKey, key)}")
            Done // failed/absent — not marked fresh, the next scrape re-enqueues
          case Some(detail) =>
            val title  = task.payload.getOrElse(EnrichDetailsTasks.TitleKey, "")
            val year   = task.payload.get(EnrichDetailsTasks.YearKey).filter(_.nonEmpty).flatMap(_.toIntOption)
            val target = enricher.detailTarget
            // Merge into the target slot, creating it if absent: a chain's network
            // source has no slot from a listing scrape, so it must be added here;
            // a 1:1 cinema's slot already exists, so this preserves its showtimes.
            cache.putIfPresent(cache.keyOf(title, year), current =>
              current.copy(data = current.data + (target -> detail.mergeInto(current.data.getOrElse(target, SourceData())))))
            freshness.markFresh(key, FreshnessKind.DetailEnrich)
            uptime.recordSuccess(service)
            Done
        }
    }
  }
}
