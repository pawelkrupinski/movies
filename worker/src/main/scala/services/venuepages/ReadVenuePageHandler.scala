package services.venuepages

import play.api.Logging
import services.UptimeMonitor
import services.cinemas.common.DetailEnricher
import services.tasks.{EnqueueResult, EnrichDetailsTasks, HandlerOutcome, Task, TaskHandler, TaskQueue, TaskType}

/** Asking for a venue page to be read into venue_pages, keyed by the PAGE — for a listing no film row
 *  holds yet, which `EnrichDetails` (keyed by the film row) cannot reach. */
object ReadVenuePageTasks {
  def payload(enricher: DetailEnricher, page: String): Map[String, String] =
    Map(EnrichDetailsTasks.GroupKey -> enricher.detailGroup, EnrichDetailsTasks.RefKey -> page)

  /** Queue the read; the queue's unique dedup key keeps one per page however often it is asked. */
  def enqueue(queue: TaskQueue, enricher: DetailEnricher, page: String, now: java.time.Instant): Boolean =
    queue.enqueue(TaskType.ReadVenuePage, EnrichDetailsTasks.pageDedupKey(enricher.detailGroup, page),
      payload(enricher, page), submittedAt = now) == EnqueueResult.Added
}

/** Reads one venue page into venue_pages (`VenuePageReader`) and records it on /uptime. A page that
 *  failed for now is left unread: the listing waiting for it is taken in at its wait's limit. One the
 *  store did not take is asked again. */
final class ReadVenuePageHandler(enrichersByGroup: Map[String, DetailEnricher], reader: VenuePageReader, uptime: UptimeMonitor,
                                 freshness: services.freshness.FreshnessStore, clock: java.time.Clock)
    extends TaskHandler with Logging {

  override val taskType: TaskType = TaskType.ReadVenuePage

  override def handle(task: Task): HandlerOutcome = {
    val page = task.payload.getOrElse(EnrichDetailsTasks.RefKey, "")
    enrichersByGroup.get(task.payload.getOrElse(EnrichDetailsTasks.GroupKey, "")) match {
      case Some(enricher) if page.nonEmpty =>
        val read = reader.read(enricher, page)
        DetailUptime.record(uptime, enricher, page, read.outcome)
        // Not tried until the store took it: no one asks for this page again, so the task does.
        if (read.unfiled) HandlerOutcome.Reschedule(Some(s"venue_pages did not take ${task.dedupKey}"))
        else {
          freshness.markFresh(EnrichDetailsTasks.pageAttempted(enricher.detailGroup, page), services.freshness.FreshnessKind.DetailEnrich, clock.instant())
          HandlerOutcome.Done
        }
      case _ =>
        logger.warn(s"No detail enricher or page for task ${task.dedupKey}; dropping.")
        HandlerOutcome.Done
    }
  }
}
