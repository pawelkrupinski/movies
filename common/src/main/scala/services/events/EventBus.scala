package services.events

import play.api.Logging
import services.tasks.TaskType

import java.util.concurrent.CopyOnWriteArrayList

/**
 * Domain-level events published by core services. Listeners subscribe by
 * passing a `PartialFunction[DomainEvent, Unit]` to `EventBus.subscribe` — the
 * bus uses `applyOrElse`, so handlers only need to match the cases they care
 * about (no explicit `case _ => ()` fallback). Handlers run synchronously on
 * the publisher's thread; listeners that do real work (network, disk) should
 * hand off to their own executor.
 */
sealed trait DomainEvent

/** A venue detail page was read into venue_pages — its detail, or that it is gone — for the enricher
 *  group `detailGroup` (one page serves every venue of a chain) and `page`, the ref the enrichment
 *  fetched (`DetailEnricher.nativeDetailRef`). Published AFTER the page is stored, so the model's
 *  index of venue_pages (`services.identity.VenuePageIndex`) reads the answer it announces: the
 *  identity model re-asks the listings that read that page. */
case class VenueDetailRead(detailGroup: String, page: String) extends DomainEvent

/** TMDB resolved a `(title, year)` to a film but TMDB had no IMDb cross-
 *  reference for it (common for very recent films and festival items, e.g.
 *  "Mortal Kombat II" 2026). `searchTitle` is the title we want to search
 *  IMDb's suggestion endpoint with — typically TMDB's `originalTitle` or
 *  English release title, whichever is more likely to match IMDb's primary.
 *
 *  `ImdbIdResolver` subscribes to this event, calls `ImdbClient.findId(...)`
 *  to recover the id, and writes it back to the cache — from where the
 *  `EnrichmentReaper` picks up the now-eligible IMDb rating on its next pass. */
case class ImdbIdMissing(title: String, year: Option[Int], searchTitle: String) extends DomainEvent

/** A queue task ran to a successful conclusion (`Done`/`Skipped` — NOT a
 *  reschedule). Published by `TaskWorker` (via an injected hook) the moment a
 *  task completes, so a consumer can chain follow-up work off it without the
 *  handler having to know what comes next (a chunked scrape's reduce, a share
 *  card's follow-up); every other task type's `TaskFinished` is simply ignored. */
case class TaskFinished(taskType: TaskType, dedupKey: String, payload: Map[String, String]) extends DomainEvent

/**
 * Publish/subscribe bus carrying `DomainEvent`s. Per CLAUDE.md DIP guidance,
 * consumers depend on this trait; `InProcessEventBus` is the production
 * (and only) implementation.
 */
trait EventBus {
  /** Register a handler. The bus uses `applyOrElse`, so the handler only
   *  needs to match the cases it cares about. */
  def subscribe(handler: PartialFunction[DomainEvent, Unit]): Unit

  /** Invoke every subscriber on the caller's thread. Throwing handlers are
   *  logged and skipped — a buggy listener can't break the bus or other
   *  listeners. */
  def publish(event: DomainEvent): Unit
}

class InProcessEventBus extends EventBus with Logging {
  // CopyOnWriteArrayList: writes (subscribe) are rare and happen at startup;
  // reads (publish) are hot and want a stable snapshot without locking.
  private val listeners = new CopyOnWriteArrayList[PartialFunction[DomainEvent, Unit]]()

  def subscribe(handler: PartialFunction[DomainEvent, Unit]): Unit = listeners.add(handler)

  def publish(event: DomainEvent): Unit = {
    val it = listeners.iterator()
    while (it.hasNext) {
      val pf = it.next()
      try pf.applyOrElse(event, (_: DomainEvent) => ()) catch {
        case exception: Throwable =>
          logger.warn(s"Event listener failed for $event: ${exception.getMessage}")
      }
    }
  }
}
