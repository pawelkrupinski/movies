package services.venuepages

import services.freshness.{FreshnessKind, FreshnessStore}
import services.tasks.{DueWindow, Enqueued, TaskProducer}
import services.tasktypes.TaskType

import java.time.Instant

/**
 * When a venue detail page is due: the ONE schedule the periodic [[DetailReaper]] enqueues on and the
 * [[EnrichDetailsHandler]] re-gates its pickup on. Shared, so the two agree on "due" — when they did not, the reaper
 * enqueued a row on its phase boundary while the handler skipped it as still-fresh, so the same row churned the queue
 * every tick without ever refreshing (see [[DueWindow]]). A page's stamp says "we asked recently", never "we have data".
 */
final class DetailSchedule(queue: TaskProducer, freshness: FreshnessStore, dueWindow: DueWindow) {

  /** Whether the detail stamps have hydrated from Mongo: they load in the rest phase (after the scrape stamps), so a
   *  tick against the not-yet-hydrated mirror reads every detail as never-fresh and re-enqueues the whole
   *  deferred-detail corpus on EVERY deploy (the recurring post-deploy spike). An in-memory store is ready at once. */
  def ready: Boolean = freshness.isReady(FreshnessKind.DetailEnrich)

  /** Whether the page asked under `dedupKey` is due at `now`. */
  def isDue(dedupKey: String, now: Instant): Boolean = dueWindow.isDue(dedupKey, freshness.lastFetchedAt(dedupKey), now)

  /** Enqueue an `EnrichDetails` task under the dedup key `dedupKey` it is due, deduplicated and stamped by, carrying
   *  `payload` (built only then), only when it's DUE under the phase-spread window. The phase offset, hashed from
   *  `dedupKey`, scatters a synchronized cohort (a re-key wave that orphans a whole batch's stamps at once) across the
   *  period instead of dumping it in one tick. What the enqueue came to: [[Enqueued.Zero]] when not due. */
  def enqueueIfDue(dedupKey: String, payload: => Map[String, String], now: Instant): Enqueued =
    if (!isDue(dedupKey, now)) Enqueued.Zero
    else queue.enqueued(TaskType.EnrichDetails, dedupKey, payload, submittedAt = now)

  /** Stamp the page asked under `dedupKey`: asked at `now`. */
  def asked(dedupKey: String, now: Instant): Unit = freshness.markFresh(dedupKey, FreshnessKind.DetailEnrich, now)

  /** Stamp the PAGE's own answer, filed into venue_pages: `pageRead` or `pageGone`, keyed by the page so the answer
   *  moves with a listing when rows regroup. */
  def answered(page: VenuePage, now: Instant): Unit = asked(page.answerStamp, now)

  /** Stamp `page` of `group` tried by a `ReadVenuePage` task, whatever it said (`EnrichDetailsTasks.pageAttempted`). */
  def attempted(group: String, page: String, now: Instant): Unit = asked(EnrichDetailsTasks.pageAttempted(group, page), now)
}
