package services.tasks

import services.Stoppable
import play.api.Logging
import services.freshness.{Freshness, FreshnessKind, FreshnessStore}
import services.schedule.{AlwaysClaimScheduledRunStore, OccurrenceKey, ScheduledRunStore}
import tools.DaemonExecutors
import services.cinemas.common.CinemaScraper

import java.time.{Clock, Duration => JDuration, Instant}
import java.util.concurrent.{ScheduledExecutorService, TimeUnit}
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * Periodically enqueues a `ScrapeCinema` task for every cinema that is due under
 * the shared [[DueWindow]] — its phase-window boundary has passed since the last
 * successful scrape, or it has never been scraped. The window's period is the
 * freshness setting (default 60min, `KINOWO_SCRAPE_FRESHNESS_MINUTES`); each
 * cinema's boundary sits at a deterministic phase offset hashed from its key, so
 * the ~300 cinemas spread evenly across the period instead of all falling due
 * together and scraping in a lockstep wave. Enqueue is deduped by the queue, so a
 * cinema with a task already waiting/working isn't queued twice; the handler
 * re-checks the SAME `DueWindow` and skips only if a concurrent run already
 * refreshed it this window (never a still-due task — that churn is what [[DueWindow]] fixes).
 *
 * Instead of re-scraping every cinema back-to-back in a continuous loop, the
 * worker scrapes a cinema at most once per window, and a failed scrape
 * (which doesn't mark freshness) is naturally retried on the next reaper tick.
 *
 * When a backlog makes more cinemas due than `maxEnqueuePerTick`, the tick
 * enqueues the MOST-OVERDUE first (oldest last-scrape, never-scraped first), so a
 * credit-throttled worker draining slowly doesn't keep re-serving the head of the
 * fixed city list and starving the tail — see [[tick]].
 *
 * On a multi-machine worker each tick is gated by a cluster-wide occurrence
 * claim ([[ScheduledRunStore]]) keyed by the tick's minute, so a given minute's
 * stale-cinema enqueue runs on one machine, rotating — not on every machine.
 */
class ScrapeReaper(
  scrapers:  Seq[CinemaScraper],
  queue:     TaskQueue,
  freshness: FreshnessStore,
  // The shared due schedule (each cinema scraped once per its freshness window,
  // phase-spread across it so all ~300 don't fall due together). The SAME instance
  // must back `ScrapeCinemaHandler` so this enqueue gate and that pickup re-gate
  // agree on what's due — see [[DueWindow]].
  dueWindow: DueWindow = new DueWindow(Freshness.defaultScrapeTtl),
  interval:  FiniteDuration = 1.minute,
  // A small extra spacing before the (now post-hydrate) first tick, so it doesn't
  // land on the same instant as the cache hydrate finishing. Defaults to 0 so the
  // tests that drive `tick()` directly are unaffected.
  initialDelay: FiniteDuration = 0.seconds,
  // How long each readiness wait blocks before logging and waiting again. We never
  // tick against a not-ready mirror (that was the boot storm) — readiness itself
  // now completes only once a hydrate SUCCEEDS or its bounded retry budget is spent
  // (see MongoFreshnessStore.hydrateInPhases), so this just paces the holding log.
  readyTimeout: FiniteDuration = 30.seconds,
  // Cap on how many stale cinemas a single tick enqueues. After a restart every
  // cinema can be stale at once; enqueuing all ~300 lets the TaskWorker pool
  // drain flat-out for minutes with no idle gap, exhausting the shared-CPU
  // credit balance (the boot-storm throttle spike). Capping the per-tick batch
  // so it drains inside the tick interval leaves the pool idle between minutes,
  // letting credit recover — the backlog clears over a few ticks instead. The
  // queue dedups, so already-in-flight cinemas don't re-count against the cap.
  // Default unbounded so the tests that drive `tick()` directly are unaffected.
  maxEnqueuePerTick: Int = Int.MaxValue,
  // Post-boot enqueue RAMP: for this long after the FIRST tick, the (non-throttled)
  // per-tick cap ramps linearly from ~1/5 of `maxEnqueuePerTick` up to the full cap.
  // Even capped, enqueuing the FULL `maxEnqueuePerTick` every tick from the first
  // one drains a restart's whole-corpus backlog flat-out (~12 min with no idle gap),
  // re-draining the shared-CPU credit balance that the restart just reset — the
  // residual boot-storm spike. Ramping the cap up over the first few minutes lets
  // the pool idle between the early ticks so credit rebuilds while the backlog still
  // clears. Anchored at the first tick (post-hydrate), so it covers exactly the
  // cold-restart window. Default 0 disables it, leaving tests/harness that drive
  // `tick()` directly (and the deterministic snapshot) unaffected.
  bootRamp: FiniteDuration = 0.seconds,
  // While the worker is CPU-credit throttled (see [[CpuCreditPoller]]), this bounds
  // the OUTSTANDING waiting scrapes (not just the per-tick additions): each tick
  // tops the waiting ScrapeCinema backlog up to THIS many, so once it's at budget
  // the tick adds nothing and the pool drains to near-empty + idles, rebuilding
  // credit (breaking the metastable spiral) — then the full cap resumes. A flat
  // per-tick cap couldn't do this: the queue dedups, so it kept adding new cinemas
  // until the whole corpus was queued, pinning the pool busy. Most-overdue-first
  // (see [[tick]]) means the bounded trickle still serves the stalest cinemas.
  // Default = unbounded so callers/tests that don't wire `throttle` keep the old
  // behaviour (the not-throttled path is unaffected).
  throttledMaxEnqueuePerTick: Int = Int.MaxValue,
  throttle: ScrapeThrottleSignal = ScrapeThrottleSignal.AlwaysHealthy,
  // SPREAD the (non-throttled) per-tick batch across the tick interval instead of
  // dumping it all at the tick instant. The reaper enqueues a clump of due cinemas
  // each tick; they fetch in parallel and their HTML/JSON payloads PARSE together —
  // a CPU spike that floors the shared-CPU credit balance (the parse-wave burst).
  // Slicing the same batch into `enqueueSpread` groups enqueued at staggered offsets
  // within the interval keeps the parses from landing together: SAME total work and
  // freshness (a sub-minute stagger is negligible against the 60-min scrape window),
  // lower CPU peak. Applies only to the healthy path — the throttled path already
  // trickles a tiny, backlog-bounded amount whose accounting must stay synchronous.
  // Default 1 disables it (single group at offset 0), leaving tests that drive
  // `tick()` directly — and the deterministic snapshot — unaffected.
  enqueueSpread: Int = 1,
  runStore: ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:    Clock = Clock.systemUTC()
) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("scrape-reaper")
  // Instant of the first tick, anchoring the post-boot ramp; set once, then read-only.
  private val rampAnchor = new AtomicReference[Option[Instant]](None)

  /** The non-throttled per-tick cap, ramped up over `bootRamp` from the first tick.
   *  A no-op (returns `maxEnqueuePerTick`) when the ramp is disabled or the cap is
   *  unbounded — so the default configuration and direct-`tick()` tests are unchanged.
   *  Pure given `now` and the (once-set) anchor. */
  private[tasks] def rampedCap(now: Instant): Int =
    if (bootRamp.toMillis <= 0 || maxEnqueuePerTick == Int.MaxValue) maxEnqueuePerTick
    else {
      val anchor  = rampAnchor.updateAndGet(prev => if (prev.isDefined) prev else Some(now)).get
      val elapsed = math.max(0L, JDuration.between(anchor, now).toMillis)
      if (elapsed >= bootRamp.toMillis) maxEnqueuePerTick
      else {
        val floorCap = math.max(1, maxEnqueuePerTick / 5)
        val scaled   = math.ceil(maxEnqueuePerTick.toDouble * elapsed / bootRamp.toMillis).toInt
        math.min(maxEnqueuePerTick, math.max(floorCap, scaled))
      }
    }

  def start(): Unit = {
    if (scrapers.isEmpty) { logger.info("ScrapeReaper: no cinemas; not starting."); return }
    // Defer onto the scheduler thread so we can block it on the freshness hydrate
    // without holding up boot wiring; it then schedules the periodic ticks.
    scheduler.execute(() => Try(awaitReadyThenStart()))
    logger.info(s"ScrapeReaper started over ${scrapers.size} cinemas, first tick after freshness hydrate then ${initialDelay.toSeconds}s, every ${interval.toSeconds}s.")
  }

  // Wait until the scrape freshness stamps are actually loaded, THEN begin the
  // periodic ticks. Readiness completes only once a hydrate SUCCEEDS (or its
  // bounded retry budget is spent — see MongoFreshnessStore.hydrateInPhases), so a
  // transient Mongo timeout no longer green-lights an empty mirror. We keep waiting
  // rather than ticking against a not-yet-hydrated mirror: ticking then would read
  // every cinema as stale and enqueue all ~300 at once — the boot storm that
  // drained the shared-CPU credit balance and slowed the next restart's hydrate,
  // storming again.
  private def awaitReadyThenStart(): Unit = {
    while (!Try(Await.ready(freshness.whenReady(FreshnessKind.CinemaScrape), readyTimeout)).isSuccess)
      logger.info("ScrapeReaper: freshness mirror still hydrating; holding scrape ticks (no cold re-scrape).")
    scheduler.scheduleWithFixedDelay(() => Try(tickIfClaimed()), initialDelay.toMillis, interval.toMillis, TimeUnit.MILLISECONDS)
  }

  /** Tick only if this machine wins the current minute's occurrence claim —
   *  otherwise another machine is enqueuing this window's stale cinemas, so
   *  skip. Returns the number enqueued (0 when the claim was lost). */
  private[tasks] def tickIfClaimed(): Int = {
    val key = OccurrenceKey.at("scrape", clock.millis(), interval, 0.seconds)
    if (runStore.claim(key)) tick() else 0
  }

  /** Enqueue every stale cinema, most-overdue first under the per-tick cap.
   *  Package-private so tests can drive it directly — bypasses the occurrence claim.
   *
   *  When `maxEnqueuePerTick` bites (a backlog: more cinemas due than the cap), we
   *  must not always favour the same head-of-list cinemas — `scrapers` is a fixed
   *  city→catalogue order, so a plain `takeWhile` over it would re-enqueue the front
   *  cinemas every tick and STARVE the tail (the last cities) while the worker is
   *  credit-throttled and draining the backlog slowly. Instead we order the due
   *  cinemas by how long they've waited — oldest `lastFetchedAt` (never-fetched =
   *  oldest) first — so the longest-overdue cinema is always served next and the
   *  backlog drains fairly. Ties break on the dedup key, keeping the order
   *  deterministic (no clock/random in the ordering — see ScrapeOrderDeterminismSpec).
   *  In steady state far fewer than the cap are due, so the sort is a cheap no-op. */
  private[tasks] def tick(now: Instant = clock.instant()): Int = {
    val due = scrapers.iterator
      .map(s => (ScrapeCinemaHandler.dedupKey(s.cinema), s.cinema.displayName))
      .filter { case (key, _) => dueWindow.isDue(key, freshness.lastFetchedAt(key), now) }
      .toVector
      // Oldest-fetched first; never-fetched (None) sorts ahead of any timestamp.
      .sortBy { case (key, _) =>
        (freshness.lastFetchedAt(key).map(_.toEpochMilli).getOrElse(Long.MinValue), key)
      }

    // Throttle backoff, BACKLOG-AWARE: when the pool is credit-starved, bound the
    // OUTSTANDING waiting scrapes to `throttledMaxEnqueuePerTick` rather than adding
    // that many AFRESH every tick. The queue dedups, so a flat per-tick cap keeps
    // piling new cinemas on until the whole due corpus is queued — which pins the
    // credit-starved pool permanently busy with NO idle gap, so credit never
    // rebuilds (the self-sustaining throttle spiral of 2026-06-24). Capping the
    // backlog lets the pool drain to near-empty and idle between ticks → credit
    // recovers, then the full cap resumes. Most-overdue-first (above) means the
    // bounded trickle still serves the stalest cinemas.
    if (throttle.isThrottled) {
      val cap      = math.max(0, throttledMaxEnqueuePerTick - queue.waitingCount(TaskType.ScrapeCinema))
      val enqueued = enqueueUpTo(due, cap)
      if (enqueued > 0)
        logger.warn(s"ScrapeReaper: CPU-credit throttle (slowest recent scrape ${throttle.slowScrapeMillis}ms) — " +
          s"backlog-capped to $cap new; enqueued $enqueued most-overdue cinema(s) of ${due.size} due, " +
          s"letting the pool drain + rebuild credit.")
      enqueued
    } else if (enqueueSpread <= 1) {
      // Un-spread healthy path (the default): enqueue the whole capped batch now.
      val enqueued = enqueueUpTo(due, rampedCap(now))
      if (enqueued > 0) logger.info(s"ScrapeReaper enqueued $enqueued stale cinema(s).")
      enqueued
    } else {
      // Spread the capped batch across the interval so the parses don't clump — see
      // `enqueueSpread`. Enqueue the first slice now; defer the rest onto staggered
      // offsets. The queue dedups, so a slice landing near the next tick can't
      // double-enqueue. `tick` returns only what it enqueued SYNCHRONOUSLY (slice 0),
      // matching the un-spread contract for the first-of-batch.
      val plan = planSlices(due.take(rampedCap(now)), enqueueSpread)
      val enqueuedNow = plan.headOption.map { case (_, first) => enqueueUpTo(first, first.size) }.getOrElse(0)
      plan.drop(1).foreach { case (offset, group) =>
        scheduleSlice(offset, () => {
          val n = Try(enqueueUpTo(group, group.size)).getOrElse(0)
          if (n > 0) logger.info(s"ScrapeReaper enqueued $n stale cinema(s) (spread slice at +${offset.toSeconds}s).")
        })
      }
      val deferred = plan.drop(1).map(_._2.size).sum
      if (enqueuedNow > 0 || deferred > 0)
        logger.info(s"ScrapeReaper enqueued $enqueuedNow stale cinema(s) now, spreading $deferred more across the tick in ${plan.size - 1} slice(s).")
      enqueuedNow
    }
  }

  /** Enqueue due cinemas in order until `cap` NEW tasks have been added; already-
   *  waiting/working cinemas dedup for free (they don't count against the cap).
   *  Returns the number of tasks actually added. */
  private def enqueueUpTo(due: Vector[(String, String)], cap: Int): Int = {
    var enqueued = 0
    due.iterator.takeWhile(_ => enqueued < cap).foreach { case (key, displayName) =>
      if (queue.enqueue(TaskType.ScrapeCinema, key,
            Map(ScrapeCinemaHandler.CinemaKey -> displayName)) == EnqueueResult.Added)
        enqueued += 1
    }
    enqueued
  }

  /** Split the tick's due-and-capped batch into `slices` contiguous groups, each
   *  tagged with the within-interval offset at which it should be enqueued (group
   *  k at k/groupCount of the interval), so their scrape parses land staggered
   *  across the tick rather than clumping at the tick instant — same total work,
   *  lower CPU peak. Group sizes differ by at most one (earlier groups take the
   *  remainder). `slices <= 1` or a batch of one → a single group at offset 0 (the
   *  un-spread default). Pure given the batch, `slices`, and `interval`. */
  private[tasks] def planSlices(
    batch:  Vector[(String, String)],
    slices: Int
  ): Vector[(FiniteDuration, Vector[(String, String)])] = {
    val n = math.max(1, slices)
    if (n <= 1 || batch.size <= 1) Vector((Duration.Zero, batch))
    else {
      val groupCount = math.min(n, batch.size)
      val base       = batch.size / groupCount
      val remainder  = batch.size % groupCount
      var idx        = 0
      (0 until groupCount).iterator.map { k =>
        val size   = base + (if (k < remainder) 1 else 0)
        val group  = batch.slice(idx, idx + size)
        idx += size
        ((interval.toMillis * k / groupCount).millis, group)
      }.toVector
    }
  }

  /** Deferral seam for the spread slices (overridable in tests so the staggering is
   *  observed deterministically without wall-clock waits). Runs `task` after `delay`
   *  on the reaper's scheduler; the returned handle is intentionally discarded — a
   *  dropped slice on shutdown just re-enqueues on the next tick. */
  protected def scheduleSlice(delay: FiniteDuration, task: Runnable): Unit = {
    scheduler.schedule(task, delay.toMillis, TimeUnit.MILLISECONDS)
    ()
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}
