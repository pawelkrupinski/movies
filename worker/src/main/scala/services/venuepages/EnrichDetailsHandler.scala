package services.venuepages

import java.time.Clock

import models.CacheKey
import play.api.Logging
import services.cinemas.common.{DetailEnricher, DetailFetchOutcome}
import services.tasks.{HandlerOutcome, Task, TaskHandler}
import services.tasktypes.TaskType

/** Builds the dedup key, freshness key, and payload for an `EnrichDetails` task.
 *  The dedup key encodes `(detailGroup, film)` so "enrich film F for group G"
 *  can exist at most once — the queue's unique index rejects a second. */
object EnrichDetailsTasks {
  val GroupKey  = "group"
  val RefKey    = "ref"
  val TitleKey  = "title"
  val YearKey   = "year"
  /** The row's key AS STORED (`normalized|year`). A cut-over country holds several films of one
   *  title and year apart by key alone (`lalka|` and `lalka~1164|`); re-deriving the key from the
   *  title and year addresses whichever owns the bare one, and lands the page on the wrong film. */
  val RowKey    = "row"

  def dedupKey(group: String, key: CacheKey): String =
    s"detail|$group|${key.cleanTitle}|${key.year.map(_.toString).getOrElse("")}"

  def payload(enricher: DetailEnricher, key: CacheKey, ref: String): Map[String, String] =
    Map(
      GroupKey -> enricher.detailGroup,
      RefKey   -> ref,
      TitleKey -> key.cleanTitle,
      YearKey  -> key.year.map(_.toString).getOrElse(""),
      RowKey   -> models.StoredMovieRecord.keyFor(key)
    )

  /** The PAGE's own stamps: `page` of `group` was read into venue_pages, or found gone. Keyed by the
   *  page, never by the film row it was on: a page's answer is the page's, wherever its listing goes. */
  def pageRead(group: String, page: String): String = s"${pageDedupKey(group, page)}|read"
  /** The dedup (and due) key of a detail task asked per PAGE (`DetailPages.PerPage`). */
  def pageDedupKey(group: String, page: String): String = s"detail-page|$group|$page"
  def pageGone(group: String, page: String): String = s"detail-page|$group|$page|gone"
  /** `page` of `group` was tried by a `ReadVenuePage` task, whatever it said: all a display-only venue's
   *  waiting listing needs (`VenuePageWait`). */
  def pageAttempted(group: String, page: String): String = s"detail-page|$group|$page|attempted"

}

/**
 * Handles an `EnrichDetails` task: fetch one film's detail (once per
 * `(detailGroup, film)` per 6h) and merge it into the enricher's `detailTarget`
 * `SourceData` slot ([[DetailTargets]]), preserving anything already in that slot
 * (e.g. a cinema slot's showtimes) — the fields the page states differently from
 * what the rows last took authoritative ([[LandedPages]]). A 1:1 cinema targets its
 * own slot; a chain targets a shared network source, so one fetch serves every venue.
 *
 * Re-gated at pickup by the SAME [[DetailSchedule]] the [[DetailReaper]] enqueues on,
 * not a separate rolling TTL: when it didn't, the reaper enqueued a row on its
 * phase boundary while the handler skipped it as still-fresh, so the same row
 * churned the queue every tick without ever refreshing (see [[services.tasks.DueWindow]]). A
 * TRANSIENTLY failed fetch is reported `Done` without marking fresh, so the
 * reaper asks again on its next tick — but only [[EnrichDetailsHandler.ImmediateRetries]]
 * times in a row; past that it is stamped and waits its normal window, so a page that
 * keeps failing is asked a few times an outage, not once a minute for its length. A
 * DURABLY gone page (404/410) is stamped instead — there, leaving it stale is
 * what makes the worker spin, because an unstamped key is due on every reaper
 * tick forever. See [[services.cinemas.common.DetailFetchOutcome]].
 */
class EnrichDetailsHandler(
  enrichersByGroup: Map[String, DetailEnricher],
  // SAME instance the DetailReaper enqueues on — see the class doc / [[DetailSchedule]].
  schedule:         DetailSchedule,
  // Each page read into `venue_pages` (`VenuePageReader`) and recorded on /uptime.
  reader:           RecordedPageReader,
  // What a read page changes on the row it was asked for ([[DetailRowLanding]]).
  landing:          DetailRowLanding,
  clock:            Clock
) extends TaskHandler with Logging {

  // Consecutive transient failures per task key. In memory on purpose, like ScrapeFreshnessPolicy's streak: losing it
  // on a restart costs at most ImmediateRetries extra asks per page.
  private val consecutiveFailures = new java.util.concurrent.ConcurrentHashMap[String, Integer]()

  import HandlerOutcome._

  override val taskType: TaskType = TaskType.EnrichDetails

  override def handle(task: Task): HandlerOutcome = {
    val key = task.dedupKey
    if (!schedule.isDue(key, clock.instant())) Skipped
    else enrichersByGroup.get(task.payload.getOrElse(EnrichDetailsTasks.GroupKey, "")) match {
      case None =>
        logger.warn(s"No detail enricher for task $key; dropping.")
        Done
      case Some(enricher) => readPage(task, enricher)
    }
  }

  /** Read the task's page into venue_pages (stamped and announced there, and recorded on /uptime), then
   *  land what it said on the row. */
  private def readPage(task: Task, enricher: DetailEnricher): HandlerOutcome = {
    val label  = task.payload.getOrElse(EnrichDetailsTasks.TitleKey, task.dedupKey)
    val ref    = task.payload.getOrElse(EnrichDetailsTasks.RefKey, "")
    // What venue_pages held of the page before this read replaces it: the last read and what the rows took of it.
    val before = landing.before(enricher, ref)
    val read   = reader.read(enricher, ref, label)
    read.outcome match {
      case DetailFetchOutcome.Failed =>
        // Failed transiently: left stale, so the reaper asks again next tick (once a minute) — within a budget. With
        // no budget, a page that kept failing was asked every minute for as long as it failed: from the 2026-10-27
        // Decodo outage on, ~100 Cineworld film pages a minute went to the direct leg and came back 403, ~9k
        // refusals every 3h from one IP to a site that had blocked it. Past the budget the failure is not a blip,
        // so stamp it and let the page wait its normal (phase-spread) window like a gone one.
        val streak = consecutiveFailures.merge(task.dedupKey, 1, (a, b) => a + b)
        if (streak > EnrichDetailsHandler.ImmediateRetries) {
          consecutiveFailures.remove(task.dedupKey)
          markAsked(task)
        } else Done
      case _ if read.unfiled =>
        // Read and not filed: neither stamped nor announced, so asked again — never landed on the row as done
        // while the identity model, which reads only venue_pages, decides the film without it.
        Reschedule(Some(s"venue_pages did not take ${enricher.detailGroup}|$ref"))
      case DetailFetchOutcome.Gone(_) =>
        // The page is gone (404/410), not failing. Leaving it stale is a
        // livelock: with no stamp the due gate is unconditionally true, so
        // DetailReaper re-enqueues this film every tick — once a minute, forever
        // — and each pass records another /uptime failure. Two such films held
        // the Cinema City chain row at ~90% failures and, since a bucket keeps
        // only 10 error strings, hid every other Cinema City enrichment failure
        // behind them.
        //
        // So stamp it. This is NOT claiming a detail landed — nothing reads
        // DetailEnrich freshness as "we have data", only as "we asked recently"
        // (this handler's own due gate, and DetailReaper's).
        // It costs nothing in recovery either: both detail caches already pin a
        // durable failure for 12h, so a retry inside this 6h window was being
        // answered from cache anyway. If the film comes back under a NEW url the
        // listing scrape rewrites the slot's `filmUrl`, and the next window
        // fetches it.
        consecutiveFailures.remove(task.dedupKey)
        markAsked(task)
      case fetched: DetailFetchOutcome.Fetched =>
        consecutiveFailures.remove(task.dedupKey)
        landing.land(task.payload, enricher, ref, before, fetched.detail)
        markAsked(task)
    }
  }

  /** Stamp the task's key: "we asked recently", never "we have data". */
  private def markAsked(task: Task): HandlerOutcome = {
    schedule.asked(task.dedupKey, clock.instant())
    Done
  }
}

object EnrichDetailsHandler {

  /** Transient failures in a row a detail page is asked again at the reaper's tick before it waits its normal window —
   *  enough to ride out a blip, not enough to hammer a site that is refusing us. */
  val ImmediateRetries = 2
}
