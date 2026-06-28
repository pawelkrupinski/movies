package services.tasks

import models.{CinemaShowing, SourceData}
import play.api.Logging
import services.UptimeMonitor
import services.cinemas.DetailEnricher
import services.events.{EventBus, MovieDetailsComplete}
import services.freshness.{FreshnessKind, FreshnessStore}
import services.movies.{CacheKey, MovieCache}

import java.time.{Clock, Instant}

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
   *  by the ONE-SHOT producers (the event enqueuer on first appearance, the inline
   *  scrape path) so the enqueue rule lives in one place. The PERIODIC reaper uses
   *  [[enqueueIfDue]] instead — it must gate on the same [[DueWindow]] the handler
   *  re-gates on, or it churns the queue. (`!isFresh ⟹ isDue`, so a task these
   *  one-shot paths enqueue is always still due at the handler — never churned.) */
  def enqueueIfStale(queue: TaskQueue, freshness: FreshnessStore, enricher: DetailEnricher, key: CacheKey, ref: String): Boolean = {
    val dk = dedupKey(enricher.detailGroup, key)
    !freshness.isFresh(dk, FreshnessKind.DetailEnrich) &&
      queue.enqueue(TaskType.EnrichDetails, dk, payload(enricher, key, ref)) == EnqueueResult.Added
  }

  /** Enqueue a detail task only when it's DUE under `dueWindow` — the phase-spread
   *  gate the periodic [[DetailReaper]] enqueues on and [[EnrichDetailsHandler]]
   *  re-gates on (shared instance). The phase offset, hashed from `dk`, scatters a
   *  synchronized cohort (a re-key wave that orphans a whole batch's stamps at
   *  once) across the period instead of dumping it in one tick. Returns true iff
   *  newly enqueued. */
  def enqueueIfDue(queue: TaskQueue, freshness: FreshnessStore, dueWindow: DueWindow,
                   enricher: DetailEnricher, key: CacheKey, ref: String, now: Instant): Boolean = {
    val dk = dedupKey(enricher.detailGroup, key)
    dueWindow.isDue(dk, freshness.lastFetchedAt(dk), now) &&
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
 * Re-gated at pickup by the SAME [[DueWindow]] the [[DetailReaper]] enqueues on,
 * not a separate rolling TTL: when it didn't, the reaper enqueued a row on its
 * phase boundary while the handler skipped it as still-fresh, so the same row
 * churned the queue every tick without ever refreshing (see [[DueWindow]]). A
 * failed fetch (`fetchFilmDetail` → None) is reported `Done` without marking
 * fresh, so the cinema's next scrape re-enqueues it rather than the worker
 * spinning.
 */
class EnrichDetailsHandler(
  enrichersByGroup: Map[String, DetailEnricher],
  cache:            MovieCache,
  freshness:        FreshnessStore,
  uptime:           UptimeMonitor,
  bus:              EventBus,
  // SAME instance the DetailReaper enqueues on — see the class doc / [[DueWindow]].
  dueWindow:        DueWindow,
  clock:            Clock = Clock.systemUTC()
) extends TaskHandler with Logging {
  import HandlerOutcome._

  override val taskType: TaskType = TaskType.EnrichDetails

  override def handle(task: Task): HandlerOutcome = {
    val key = task.dedupKey
    if (!dueWindow.isDue(key, freshness.lastFetchedAt(key), clock.instant())) return Skipped

    enrichersByGroup.get(task.payload.getOrElse(EnrichDetailsTasks.GroupKey, "")) match {
      case None =>
        logger.warn(s"No detail enricher for task $key; dropping.")
        Done
      case Some(enricher) =>
        // Record enrichment health on /uptime: a resolved detail is a success, an
        // absent/failed fetch a failure (red/yellow). A 1:1 cinema records under
        // its own "<cinema>|enrichment" sub-row; a chain overrides this to a
        // single network-level service (e.g. "Cinema City Enrichment").
        val service = enricher.enrichmentServiceOverride
          .getOrElse(UptimeMonitor.enrichmentService(enricher.cinema.displayName))
        enricher.fetchFilmDetail(task.payload.getOrElse(EnrichDetailsTasks.RefKey, "")) match {
          case None =>
            uptime.recordFailure(service, s"detail fetch returned nothing for ${task.payload.getOrElse(EnrichDetailsTasks.TitleKey, key)}")
            Done // failed/absent — not marked fresh, the next scrape re-enqueues
          case Some(detail) =>
            val title  = task.payload.getOrElse(EnrichDetailsTasks.TitleKey, "")
            val year   = task.payload.get(EnrichDetailsTasks.YearKey).filter(_.nonEmpty).flatMap(_.toIntOption)
            // For a 1:1 venue the listing slot is keyed per shown title
            // (`CinemaShowing`), so target THAT slot — a bare-cinema target would
            // create a separate empty slot and the detail would never merge into the
            // film's showtimes. A chain redirects detail to its shared network source
            // (not per-title), which is left as-is.
            val target =
              if (enricher.detailTarget == enricher.cinema) CinemaShowing.keyFor(enricher.cinema, title)
              else enricher.detailTarget
            val rowKey = cache.keyOf(title, year)
            // Was this the detail the row was held back for? Capture before the
            // merge clears the flag, so a periodic re-fetch of an already-done
            // row (DetailReaper refreshing showtimes) doesn't re-trigger TMDB.
            val wasPending = cache.get(rowKey).exists(_.detailPending)
            // Merge into the target slot, creating it if absent: a chain's network
            // source has no slot from a listing scrape, so it must be added here;
            // a 1:1 cinema's slot already exists, so this preserves its showtimes.
            // Clearing `detailPending` releases the row to the read model + the
            // TMDB stage now that its detail (director/originalTitle/year) is in.
            cache.putIfPresent(rowKey, current =>
              current.copy(
                data          = current.data + (target -> detail.mergeInto(current.data.getOrElse(target, SourceData()))),
                detailPending = false))
            freshness.markFresh(key, FreshnessKind.DetailEnrich)
            uptime.recordSuccess(service)
            // The detail just landed → enrich the film now, with the better hints.
            if (wasPending) {
              val row = cache.get(rowKey)
              bus.publish(MovieDetailsComplete(
                title, year,
                row.flatMap(_.cinemaOriginalTitle),
                row.map(_.director).filter(_.nonEmpty).map(_.mkString(", "))))
            }
            Done
        }
    }
  }
}
