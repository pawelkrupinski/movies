package services.movies

import java.time.{Clock, Duration, Instant}
import java.util.concurrent.{ConcurrentHashMap, ConcurrentSkipListMap}
import java.util.concurrent.atomic.AtomicLong
import scala.jdk.CollectionConverters._

/**
 * WHEN EACH CHANGE-STREAM CURSOR LAST DELIVERED AN EVENT — the liveness signal the
 * reopen driver cannot give.
 *
 * A TERMINAL cursor error is reopened on a backoff ([[ChangeStreamReopen]]). A cursor that
 * is OPEN and silent — a server-side stall, a stale `resumeAfter` the server accepted and
 * never advanced past, a migration that left the token pointing at an oplog the new
 * primary does not have — errors nothing and reopens nothing. Its event counters simply
 * stop, which is also exactly what a quiet night looks like, and the site serves stale
 * ratings and showtimes for as long as nobody notices. That has happened: after a Mongo
 * migration the worker's stream sat dead for hours behind green panels.
 *
 * So the instant of the last DELIVERED event is kept per cursor, and the AGE of it —
 * computed against `now` when asked, never stored — is what a gauge exports, and the
 * instant itself is the floor the read-model catch-up reads `movies.updatedAt` from (raised
 * past it by a catch-up that finished — see [[catchUpFloor]]). A cursor that never delivered
 * ages from the moment this was created (the process boot, in production): "never" is
 * not "zero", and a stream that fails to start is the loudest case of a silent one.
 *
 * Keyed by collection name ([[ChangeStreamLiveness.Movies]] / `Screenings` / `Slots`):
 * the three cursors are three subscriptions that fail independently, and a live
 * `movie_slots` cursor says nothing about the `movies` one.
 *
 * THE OTHER HALF IS THE APPLY. A cursor that delivers on time can still sit behind an apply
 * thread that has fallen behind (one thread, a blocking stitch read per event), and a
 * delivered-but-unapplied event is exactly what a resume token saved at delivery used to skip
 * on restart (fixed 2026-09-23 — nothing could have shown that it was happening). So each
 * event handed to the apply thread is also recorded here ([[queued]]) until its apply has
 * run ([[applied]]): how many are waiting per cursor, and how long the OLDEST has waited —
 * computed against `now` like the delivery age, so a stuck apply climbs rather than freezes.
 */
final class ChangeStreamLiveness(clock: Clock = Clock.systemUTC()) {
  /** When this instance was created — the floor every never-delivered cursor ages from. */
  val openedAt: Instant = clock.instant()

  private val last       = new ConcurrentHashMap[String, Instant]()
  private val subscribed = new ConcurrentHashMap[String, Instant]()

  /** A cursor on `collection` subscribed (the driver's `onSubscribe`, or a fake's register).
   *  Until then nothing was promised: a repository with no change stream at all — a test
   *  wiring, a Mongo-less boot — has nothing to catch up on, only nothing to deliver. */
  def watching(collection: String): Unit = { subscribed.put(collection, clock.instant()); () }

  /** Whether a cursor on `collection` has ever subscribed in this process. */
  def isWatching(collection: String): Boolean = subscribed.containsKey(collection)

  /** One event was DELIVERED by `collection`'s cursor (the driver's `onNext`), whatever
   *  it carried and whatever the apply does with it. */
  def delivered(collection: String): Unit = { last.put(collection, clock.instant()); () }

  /** The instant of the last delivered event, `None` when the cursor delivered nothing
   *  since [[openedAt]]. */
  def lastDelivered(collection: String): Option[Instant] = Option(last.get(collection))

  /** The floor for "what did this cursor miss": its last delivery, or the open when it
   *  never delivered — everything written since then is unproven. */
  def lastDeliveredOrOpened(collection: String): Instant = lastDelivered(collection).getOrElse(openedAt)

  // Per cursor: the instant a catch-up read started whose every row was re-projected — what
  // the catch-up itself has proven, which the delivery floor alone never learns. Kept apart
  // from `last` so that a catch-up never makes a dead cursor LOOK alive to the age gauge.
  private val caughtUpThrough = new ConcurrentHashMap[String, Instant]()

  /** The instant a catch-up read about to start should hand back to [[caughtUp]]. From this
   *  clock, because it is compared against `updatedAt`, which is stamped by the same one. */
  def now(): Instant = clock.instant()

  /** The floor for the next catch-up read: the later of the cursor's last delivery (or its
   *  open) and the start of the last catch-up that re-projected everything it read. Without
   *  the second, a cursor that stays silent has every sweep re-read the same rows. */
  def catchUpFloor(collection: String): Instant = {
    val delivered = lastDeliveredOrOpened(collection)
    Option(caughtUpThrough.get(collection)).filter(_.isAfter(delivered)).getOrElse(delivered)
  }

  /** A catch-up read started at `readFrom` re-projected every row it returned. */
  def caughtUp(collection: String, readFrom: Instant): Unit = {
    caughtUpThrough.merge(collection, readFrom, (held, next) => if (next.isAfter(held)) next else held); ()
  }

  /** Seconds between the last delivered event (or the open) and `now`. Computed on demand
   *  so that a stalled cursor's age keeps GROWING on every scrape. */
  def ageSeconds(collection: String, now: Instant): Double =
    math.max(0L, Duration.between(lastDeliveredOrOpened(collection), now).toMillis) / 1000.0

  // Per cursor: every event handed to the apply thread and not yet applied, by a ticket in
  // hand-off order, with the instant it was handed off. A map, not a FIFO, so an apply that
  // finishes out of order (a throw, a future second apply thread) still removes its own entry.
  private val waiting = new ConcurrentHashMap[String, ConcurrentSkipListMap[Long, Instant]]()
  private val tickets = new AtomicLong(0L)

  private def waitingOn(collection: String): ConcurrentSkipListMap[Long, Instant] =
    waiting.computeIfAbsent(collection, _ => new ConcurrentSkipListMap[Long, Instant]())

  /** An event from `collection`'s cursor was handed to the apply thread. Returns the ticket
   *  to give back to [[applied]] once that apply has run. */
  def queued(collection: String): Long = {
    val ticket = tickets.incrementAndGet()
    waitingOn(collection).put(ticket, clock.instant())
    ticket
  }

  /** The apply behind `ticket` has run (or failed — either way it is no longer waiting). */
  def applied(collection: String, ticket: Long): Unit = { waitingOn(collection).remove(ticket); () }

  /** The ticket of the last event handed to the apply thread, from any cursor — 0 before the first. */
  def lastTicket: Long = tickets.get()

  /** Whether every event handed to the apply thread up to `ticket`, from any cursor, has been applied. */
  def appliedThrough(ticket: Long): Boolean =
    waiting.values().iterator().asScala.forall(onCursor => Option(onCursor.firstEntry()).forall(_.getKey > ticket))

  /** Events from `collection`'s cursor handed to the apply thread and not yet applied. */
  def pendingApplies(collection: String): Int = waitingOn(collection).size

  /** Seconds the OLDEST unapplied event from `collection`'s cursor has waited since it was
   *  handed off; 0 when nothing is waiting. Computed on demand, so a stuck apply keeps growing. */
  def applyLagSeconds(collection: String, now: Instant): Double =
    Option(waitingOn(collection).firstEntry())
      .map(oldest => math.max(0L, Duration.between(oldest.getValue, now).toMillis) / 1000.0)
      .getOrElse(0.0)
}

object ChangeStreamLiveness {
  val Movies     = "movies"
  val Screenings = "screenings"
  val Slots      = "movie_slots"
  /** Every cursor a [[MovieChangeStream]] runs — the label set the gauge is seeded with. */
  val Collections: Seq[String] = Seq(Movies, Screenings, Slots)

  /** For a repository with no change stream at all: every cursor ages from creation and
   *  nothing ever stamps it. */
  def unwatched(): ChangeStreamLiveness = new ChangeStreamLiveness()
}
