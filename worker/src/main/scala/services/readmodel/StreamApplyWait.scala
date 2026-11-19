package services.readmodel

import tools.ChangeStreamLiveness


import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{Duration, FiniteDuration}

/** How a prune sweep's heal verdict ([[SweepVerdict]]) waits for the change stream to apply what it
 *  had in flight when the sweep let go of the projection lock. */
object StreamApplyWait {
  /** How long a write that landed while a sweep held the lock may take to be DELIVERED by its
   *  cursor, before the wait below can see it queued. */
  private val InFlightDeliveryGrace = Duration(2000L, TimeUnit.MILLISECONDS)
  /** The longest a sweep's heal verdict waits for the stream's apply thread to catch up: past the
   *  longest a debounce holds a film's re-read (its cap), plus the re-read itself. At 30 s, under a
   *  2-minute cap, a film the debounce was holding was counted a miss whenever its burst outlasted
   *  the wait — `ReadModelHealsRecurring` in the US, UK and Germany from 2026-09-30. */
  private[services] val InFlightApplyTimeout: FiniteDuration =
    services.movies.MovieChangeStream.Debounce.Worker.cap + Duration(60L, TimeUnit.SECONDS)

  /** The wait on blocking sleeps — what a sweeper built bare (a spec) waits with. */
  val Blocking: ChangeStreamLiveness => Unit = await(_, tools.Sleep.Blocking)

  /** Let the change stream apply what it had in flight when a sweep let go of the lock: wait out
   *  the delivery of a write that landed during the sweep, then until every event handed to the
   *  apply thread by then has been applied, or the timeout — an apply thread that far behind is
   *  `ChangeStreamApplyLagging`'s business, and a heal it leaves unapplied counts. No cursor
   *  subscribed (a Mongo-less boot, a test wiring) means nothing is in flight. */
  def await(
    liveness: ChangeStreamLiveness,
    sleep:    FiniteDuration => Unit,
    grace:    FiniteDuration = InFlightDeliveryGrace,
    timeout:  FiniteDuration = InFlightApplyTimeout
  ): Unit =
    if (ChangeStreamLiveness.Collections.exists(liveness.isWatching)) {
      sleep(grace)
      val handedOff = liveness.lastTicket
      // Bounded by polls, not a deadline: the check is a field read, so `timeout / AppliedPoll` polls wait `timeout`,
      // and a spec's sleep that skips the wait still ends the loop.
      val polls = math.max(1L, timeout.toNanos / AppliedPoll.toNanos)
      Iterator.range(0L, polls).takeWhile(_ => !liveness.appliedThrough(handedOff)).foreach(_ => sleep(AppliedPoll))
    }

  /** How often [[await]] looks again at what the apply thread has applied. */
  private val AppliedPoll = Duration(50L, TimeUnit.MILLISECONDS)
}
