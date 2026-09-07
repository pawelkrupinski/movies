package services.movies

import java.time.{Clock, Duration, Instant}
import java.util.concurrent.ConcurrentHashMap

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
 * computed against `now` when asked, never stored — is what a gauge exports and what the
 * read-model catch-up compares `movies.updatedAt` against. A cursor that never delivered
 * ages from the moment this was created (the process boot, in production): "never" is
 * not "zero", and a stream that fails to start is the loudest case of a silent one.
 *
 * Keyed by collection name ([[ChangeStreamLiveness.Movies]] / `Screenings` / `Slots`):
 * the three cursors are three subscriptions that fail independently, and a live
 * `movie_slots` cursor says nothing about the `movies` one.
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

  /** Seconds between the last delivered event (or the open) and `now`. Computed on demand
   *  so that a stalled cursor's age keeps GROWING on every scrape. */
  def ageSeconds(collection: String, now: Instant): Double =
    math.max(0L, Duration.between(lastDeliveredOrOpened(collection), now).toMillis) / 1000.0
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
