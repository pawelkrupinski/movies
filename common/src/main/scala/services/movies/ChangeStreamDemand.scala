package services.movies

import org.mongodb.scala.Subscription

import java.util.concurrent.atomic.AtomicReference

/**
 * Bounds how far a change stream may run AHEAD of the apply that consumes it.
 *
 * WHY THIS EXISTS. Both cursors used to open with `request(Long.MaxValue)` — "send
 * me everything, as fast as you can read it" — while the apply they feed is a SINGLE
 * thread ([[tools.DaemonExecutors.singleThreadExecutor]], an unbounded
 * `LinkedBlockingQueue`) that does a BLOCKING Mongo read per event (the stitch in
 * `MongoMovieRepository`, the `findById` re-read for a showtime change). Producer and
 * consumer were coupled by nothing at all: every event the driver could read became a
 * queued task holding a fully decoded `StoredMovieDto`, and the only ceiling was heap.
 *
 * That is fine while the consumer keeps up — measured at ~30 write ops/s against prod,
 * it does, which is why this never showed up as an incident. It stops being fine the
 * moment the consumer is slow relative to the producer, and the local dev server is
 * exactly that case: the same event rate, but every stitch read crosses an ssh tunnel
 * (~50ms/doc) instead of staying in-cluster. A burst a pod absorbs is a multi-GB
 * backlog on a laptop.
 *
 * WHAT IT DOES. Classic reactive-streams backpressure: ask for `window` events up
 * front, then ask for exactly one more each time an event has been APPLIED. Delivered-
 * but-not-yet-applied can never exceed `window`, so the backlog is bounded by
 * construction — no dropped events, no queue-overflow policy to get wrong, and the
 * driver stops pulling from the cursor until the apply catches up.
 *
 * The [[applied]] call has to happen where the work FINISHES, not where it is handed
 * off. For a stream whose `onNext` applies inline that is the end of `onNext`; for the
 * two that hand off to the shared apply thread it is the task's `finally`. Getting it
 * wrong doesn't deadlock — it restores the old unbounded behaviour ([[applied]] called
 * too early) or stalls the stream ([[applied]] never called), and the specs pin both.
 */
final class ChangeStreamDemand(val window: Int) {
  require(window > 0, s"change-stream demand window must be positive, got $window")

  private val subscription = new AtomicReference[Subscription](null)

  /** Wire a freshly opened cursor and prime it with a full window. Called from the
   *  subscriber's `onSubscribe`; a reopen calls it again with the new subscription. */
  def opened(s: Subscription): Unit = {
    subscription.set(s)
    s.request(window.toLong)
  }

  /** One event has finished applying — release demand for one more. Safe to call after
   *  the cursor has gone (a terminal error mid-apply): the reference is cleared, so the
   *  request is dropped rather than aimed at a dead subscription. */
  def applied(): Unit = Option(subscription.get()).foreach(_.request(1L))

  /** Forget the cursor — a terminal error or a close. Demand released after this point
   *  goes nowhere until the next [[opened]]. */
  def closed(): Unit = subscription.set(null)
}

object ChangeStreamDemand {
  /** How far either cursor may run ahead of the apply thread.
   *
   *  Sized to stay batch-shaped (the driver fetches in batches, so a window far below
   *  one batch just adds round-trips) while keeping the worst case small: 256 stitched
   *  movie records is tens of MB, not the unbounded gigabytes it replaced. Each cursor
   *  gets its own window, so the shared apply queue is capped at twice this. */
  val DefaultWindow: Int = 256

  /** Unbounded demand, for the call sites that genuinely have no backlog to bound —
   *  the in-memory repository (its `watch` rings listeners synchronously, so there is
   *  no queue) and specs that drive a stream by hand. Named so a reader can tell an
   *  INTENTIONAL `Long.MaxValue` from the accident this class exists to remove. */
  def unbounded: ChangeStreamDemand = new ChangeStreamDemand(Int.MaxValue)
}
