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
   *  counts down, a thread finishing its run. Past the suite's [[SuiteDeadline]], [[PastDeadline]]. */
  def Io: FiniteDuration = withinSuiteDeadline(IoBound)

  /** Eventual consistency: a change stream delivering, a projection catching up, a poller's
   *  condition coming true — [[Eventually]]'s default deadline. Past the suite's
   *  [[SuiteDeadline]], [[PastDeadline]]. */
  def Settle: FiniteDuration = withinSuiteDeadline(SettleBound)

  private val IoBound: FiniteDuration     = 60.seconds * Scale
  private val SettleBound: FiniteDuration = 60.seconds * Scale

  /**
   * THE HUNG-SUITE BUDGET. A generous bound per wait makes a regression that hangs every test
   * of a suite cost a minute PER TEST, which runs a CI job past its `timeout-minutes`: the job is
   * cancelled, with no JUnit report and no flake rerun to say what broke. So a suite gets a
   * deadline, counted from its first [[Io]] / [[Settle]] read: past it, those bounds shrink to
   * [[PastDeadline]], and the rest of a hung suite fails in seconds per test, inside the job.
   *
   * The suite is told by the thread asking — ScalaTest names a thread running a suite
   * `…-ScalaTest-running-<suite>`; a bound read anywhere else (a thread a spec started, a
   * patience built at construction) is the full one. [[Run]] is never shrunk: the whole-corpus
   * suites wait a pipeline pass long after their start. A suite that runs past ten minutes BY
   * DESIGN mixes in [[OutlivesSuiteDeadline]] and keeps its full bounds: the country convergence
   * legs run for hours (US order-independence 12 min on CI, full legs up to 73), reading the
   * oplog and dropping their database with an [[Io]] bound that ten seconds would turn into a
   * flake on a loaded runner — and each carries a runaway guard of its own.
   */
  val SuiteDeadline: FiniteDuration = 10.minutes * Scale
  val PastDeadline: FiniteDuration  = 10.seconds * Scale

  private val suiteStarts = new java.util.concurrent.ConcurrentHashMap[String, java.lang.Long]()
  private val outliving   = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()
  private val SuiteThread = "ScalaTest-running-(.+)$".r.unanchored

  private def withinSuiteDeadline(bound: FiniteDuration): FiniteDuration =
    boundFor(bound, Thread.currentThread.getName, System.nanoTime(), suiteStarts)

  /** Exempt the suite named `suiteName` (ScalaTest's name, as its thread carries it) from
   *  [[SuiteDeadline]] — through [[OutlivesSuiteDeadline]], not called directly. */
  private[tools] def outlivesSuiteDeadline(suiteName: String): Unit = { outliving.add(suiteName); () }

  /** `bound`, or [[PastDeadline]] once the suite `threadName` runs has been waiting on bounds for
   *  longer than [[SuiteDeadline]] — `starts` holding when each suite first asked, `exempt` the
   *  suites that run past it by design. */
  private[tools] def boundFor(bound: FiniteDuration, threadName: String, nowNanos: Long,
                              starts: java.util.concurrent.ConcurrentHashMap[String, java.lang.Long],
                              exempt: java.util.Set[String] = outliving): FiniteDuration =
    threadName match {
      case SuiteThread(suite) if exempt.contains(suite) => bound
      case SuiteThread(suite) =>
        val start = starts.computeIfAbsent(suite, _ => nowNanos)
        if (nowNanos - start > SuiteDeadline.toNanos) bound.min(PastDeadline) else bound
      case _ => bound
    }

  /** A whole run: a pipeline pass over a corpus, a full projection, a batch of renders. */
  val Run: FiniteDuration = 10.minutes * Scale

  /** One pass of a re-triggering loop — a cadence, not a deadline (see above). */
  val Pace: FiniteDuration = 1.second

  /** A window watched for something NOT to happen — the absence claim's strength, chosen per
   *  site. A spec asserting a wait times out should prefer a max-duration bound on it. */
  def quiet(window: FiniteDuration): FiniteDuration = window
}

/** A suite that runs past [[SpecTimeouts.SuiteDeadline]] by design, and bounds a runaway itself
 *  (a replay guard, its CI step's ceiling): its [[SpecTimeouts.Io]] / [[SpecTimeouts.Settle]]
 *  waits keep their full bound however long it has been running. */
trait OutlivesSuiteDeadline extends org.scalatest.Suite {
  SpecTimeouts.outlivesSuiteDeadline(suiteName)
}
