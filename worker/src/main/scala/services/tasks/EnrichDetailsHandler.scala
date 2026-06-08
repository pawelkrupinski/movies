package services.tasks

import models.Source
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
 * `(detailGroup, film)` per 6h) and merge it into that cinema's `SourceData`
 * slot, preserving the slot's showtimes.
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
        // Record this cinema's enrichment health, grouped under it on /uptime: a
        // resolved detail is a success, an absent/failed fetch a failure (red/
        // yellow), mirroring how the scrape records against the bare cinema name.
        val service = UptimeMonitor.enrichmentService(enricher.cinema.displayName)
        enricher.fetchFilmDetail(task.payload.getOrElse(EnrichDetailsTasks.RefKey, "")) match {
          case None =>
            uptime.recordFailure(service, s"detail fetch returned nothing for ${task.payload.getOrElse(EnrichDetailsTasks.TitleKey, key)}")
            Done // failed/absent — not marked fresh, the next scrape re-enqueues
          case Some(detail) =>
            val title = task.payload.getOrElse(EnrichDetailsTasks.TitleKey, "")
            val year  = task.payload.get(EnrichDetailsTasks.YearKey).filter(_.nonEmpty).flatMap(_.toIntOption)
            cache.putIfPresent(cache.keyOf(title, year), current =>
              current.data.get(enricher.cinema) match {
                case Some(slot) => current.copy(data = current.data + ((enricher.cinema: Source) -> detail.mergeInto(slot)))
                case None       => current // slot dropped between scrape and enrich — leave alone
              })
            freshness.markFresh(key, FreshnessKind.DetailEnrich)
            uptime.recordSuccess(service)
            Done
        }
    }
  }
}
