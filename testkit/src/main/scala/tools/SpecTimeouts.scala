package tools

import settings.ProcessConfiguration

import scala.concurrent.duration.*

/**
 * THE timing policy for every wait a test makes — an `Await`, a latch, a `Future.get`, a
 * thread `join`, an [[Eventually]] deadline, a ScalaTest patience.
 *
 * The class of failure it ends: specs bounded a wait by whatever literal looked comfortable
 * on an idle laptop (`Await.result(f, 10.seconds)`, `latch.await(5, SECONDS)`, `eventually`'s
 * old 2 s), and under a loaded machine or CI runner the wait ran out although nothing was
 * wrong — a Mongo TTL-index round trip, a staging write, an auth-code store, an in-flight
 * commit, a change-stream debounce, each failing a full `itAll` and passing alone.
 *
 * A bound on a POSITIVE wait costs nothing when the spec is green — the wait returns the
 * moment the thing happens — so the bounds here are generous; they only decide how long a
 * genuinely broken spec takes to say so. `KINOWO_SPEC_TIME_SCALE` (a whole number, default 1)
 * multiplies them for a runner slower still.
 *
 * Two bounds are deliberately NOT generous and NOT scaled, because they are not timeouts:
 *  - [[Pace]]: how long one pass of a re-triggering loop waits before it makes another change
 *    (`Eventually.awaitStreamLive`'s `fired`); the loop's own deadline is [[Settle]].
 *  - [[quiet]]: the window a spec watches for something NOT to happen. A longer window only
 *    makes the absence claim stronger and the spec slower, so each site names its own.
 *
 * `NoLiteralWaitBoundSpec` keeps every other test-side wait bound coming from here.
 */
object SpecTimeouts {

  /** The resolved multiplier, from the one configuration resolver. */
  val Scale: Int = ProcessConfiguration.resolve().specTimeScale.value

  /** One operation completing: a Mongo/driver round trip, a future, a latch a worker thread
   *  counts down, a thread finishing its run. */
  val Io: FiniteDuration = 60.seconds * Scale

  /** Eventual consistency: a change stream delivering, a projection catching up, a poller's
   *  condition coming true — [[Eventually]]'s default deadline. */
  val Settle: FiniteDuration = 60.seconds * Scale

  /** A whole run: a pipeline pass over a corpus, a full projection, a batch of renders. */
  val Run: FiniteDuration = 10.minutes * Scale

  /** One pass of a re-triggering loop — a cadence, not a deadline (see above). */
  val Pace: FiniteDuration = 1.second

  /** A window watched for something NOT to happen — the absence claim's strength, chosen per
   *  site. A spec asserting a wait times out should prefer a max-duration bound on it. */
  def quiet(window: FiniteDuration): FiniteDuration = window
}
