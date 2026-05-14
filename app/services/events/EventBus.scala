package services.events

import play.api.Logging

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

/** A title was observed in a cinema's refreshed schedule. May fire repeatedly
 *  for the same film as long as it remains in any cinema's listing —
 *  subscribers are responsible for dedup. */
case class MovieAdded(title: String, year: Option[Int]) extends DomainEvent

class EventBus extends Logging {
  // CopyOnWriteArrayList: writes (subscribe) are rare and happen at startup;
  // reads (publish) are hot and want a stable snapshot without locking.
  private val listeners = new CopyOnWriteArrayList[PartialFunction[DomainEvent, Unit]]()

  def subscribe(handler: PartialFunction[DomainEvent, Unit]): Unit = listeners.add(handler)

  /** Invoke every subscriber on the caller's thread. A handler that throws is
   *  logged and skipped — a buggy listener can't break the bus or other
   *  listeners. */
  def publish(event: DomainEvent): Unit = {
    val it = listeners.iterator()
    while (it.hasNext) {
      val pf = it.next()
      try pf.applyOrElse(event, (_: DomainEvent) => ()) catch {
        case ex: Throwable =>
          logger.warn(s"Event listener failed for $event: ${ex.getMessage}")
      }
    }
  }
}
