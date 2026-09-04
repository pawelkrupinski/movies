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
 * freshness setting (`KINOWO_SCRAPE_FRESHNESS_MINUTES`, default 60min) — set per
 * country in that worker's k3s overlay, so the rate is NOT uniform across the
 * fleet: each country's value is sized so its PACED sweep fits inside it, which
 * on a chunked country with a big roster is many times PL's hour. The overlays
 * hold the live values and `WorkerScrapeCadenceConfigSpec` asserts the sweep
 * still fits each one — read them there, not from a number in this comment, which
 * is how the figures quoted here went stale before. Each cinema's boundary sits at a
 * deterministic phase offset hashed from its key, so a country's cinemas spread
 * evenly across the period instead of all falling due together and scraping in a
 * lockstep wave. Enqueue is deduped by the queue, so a
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
  // Ceiling on outstanding scrape TASKS ([[ScrapeReaper.ScrapeWorkTypes]]) — the
  // smoothing bound, which is now the only one. It sat beside a throttled "emergency
  // brake" that trimmed enqueue while an external signal said the box was CPU-starved;
  // that signal was Fly's shared-CPU credit balance and went with the platform.
  //
  // `maxEnqueuePerTick` counts venues, and on a chunked country a venue is not a unit
  // of work: 40 UK venues is ~1,440 chunk fetches, roughly 20 minutes for a 4-worker
  // pool paced at 5 req/s, dumped at one instant. That burst is what floors the credit
  // balance — not the total, which is comfortable (UK needs ~72 tasks/min against a
  // ~300/min ceiling, a 24% duty cycle). A restart makes it worse: the deploy re-grants
  // credit to ~16k, above `exit>14000`, so the worker reads as healthy, takes this
  // path, and enters the throttle it is about to trip already carrying the backlog that
  // keeps it there.
  //
  // Bounding the outstanding TASKS converts that into the steady trickle the corpus
  // actually needs — same total work and freshness, no burst. Default unbounded so
  // callers/tests that don't wire it keep the old behaviour.
  maxOutstandingScrapeTasks: Int = Int.MaxValue,
  // What one venue COSTS, in scrape tasks, so `maxOutstandingScrapeTasks` can be spent
  // in the unit it is denominated in. A chunked venue's ScrapeCinema task is only the
  // planner; the fetches it fans out arrive a moment later, so a bound checked at
  // admission time sees ~1/36th of what it is about to authorise. That is the sawtooth:
  // on 2026-07-28 kinowo-worker-uk's ScrapeCinema draining 19 -> 0 put ScrapeChunk
  // 244 -> 791 in a single step, from a reaper that believed it was under budget.
  //
  // Per country, because the fan-out is a property of the scrapers a country wires:
  // ~36 for a UK Flicks venue (one chunk per advertised day), ~16 for a German
  // Filmstarts one, 1 for an unchunked Polish venue. Set in each worker app's toml via
  // `KINOWO_SCRAPE_TASKS_PER_VENUE`. It only has to be right to within a factor of
  // two — it sizes a burst bound, not a schedule. Default 1 = unchunked, which leaves
  // the bound behaving exactly as a venue count for callers that don't set it.
  tasksPerVenue: Int = 1,
  // Cinemas whose scrape is already running are left OUT of the due set. A chunked
  // venue is not stamped until its run terminates (deliberately — see
  // [[ScrapeInFlight]]), so without this it stays most-overdue for its own duration
  // and the reaper re-admits it every tick into a no-op against the run mutex.
  inFlight: ScrapeInFlight = ScrapeInFlight.Never,
  // How long a chunked venue's fan-out is staggered over
  // ([[ScrapeCadence.ChunkEnqueueSpread]]). The reaper needs it because that spread
  // sets how long a venue OCCUPIES the budget: its chunks only become eligible
  // gradually, so the venue takes the whole window to finish however idle the pool
  // is. Throughput is concurrent-venues / spread, which makes the spread a term in
  // how big the outstanding budget must be — see `spreadAwareOutstandingBudget`.
  // Default zero = no spread, leaving unchunked callers and tests unaffected.
  chunkSpread: FiniteDuration = Duration.Zero,
  // SPREAD the (non-throttled) per-tick batch across the tick interval instead of
  // dumping it all at the tick instant. The reaper enqueues a clump of due cinemas
  // each tick; they fetch in parallel and their HTML/JSON payloads PARSE together —
  // a CPU spike that floors the shared-CPU credit balance (the parse-wave burst).
  // Slicing the same batch into `enqueueSpread` groups enqueued at staggered offsets
  // within the interval keeps the parses from landing together: SAME total work and
  // freshness (a sub-minute stagger is negligible against the 60-min scrape window),
  // lower CPU peak.
  // Default 1 disables it (single group at offset 0), leaving tests that drive
  // `tick()` directly — and the deterministic snapshot — unaffected.
  enqueueSpread: Int = 1,
  runStore: ScheduledRunStore = AlwaysClaimScheduledRunStore,
  clock:    Clock = Clock.systemUTC()
) extends Stoppable with Logging {

  private val scheduler: ScheduledExecutorService = DaemonExecutors.scheduler("scrape-reaper")

  /** Venues this tick must admit for the roster to be swept once per freshness
   *  window — `corpus / ticksPerWindow`, rounded up. The floor under every bound
   *  below: capping enqueue is meant to smooth the pool's load, not to park the
   *  corpus, so no cap may sit under the rate the roster's own freshness window
   *  implies. Derived rather than configured, so it tracks the roster and the window
   *  instead of drifting from them. */
  private val cadenceVenuesPerTick: Int = {
    val ticksPerWindow = math.max(1L, dueWindow.period.toMillis / math.max(1L, interval.toMillis))
    math.max(1, math.ceil(scrapers.size.toDouble / ticksPerWindow).toInt)
  }

  /** The outstanding-task budget actually used: at least enough for the venues that
   *  must be IN FLIGHT AT ONCE to hold cadence.
   *
   *  A venue's chunks are released gradually across `chunkSpread`, so a venue occupies
   *  the budget for that whole window regardless of how fast the pool drains — making
   *  throughput `concurrentVenues / chunkSpread`, not `pool capacity`. A flat budget
   *  therefore silently caps the sweep rate: 150 tasks over UK's 36-per-venue fan-out
   *  is 4 concurrent venues over a 5min spread = 48 venues/h against the 120 its
   *  843-venue, 420-min roster needs; DE's 9 concurrent = 108/h against 511. Prod
   *  2026-07-29 showed it as an IDLE pool (`worked_on`=0) beside 116 waiting tasks —
   *  queued but not yet eligible — while the oldest cinema aged past 15h.
   *
   *  Derived from the roster, the window and the spread rather than configured, so it
   *  cannot drift from them. The configured value still wins when it is larger. */
  /** Outstanding TASKS needed for the roster to be swept once per freshness window,
   *  accounting for the spread: a venue occupies budget for the whole `chunkSpread`,
   *  so holding cadence needs `venuesPerTick x spreadTicks` of them in flight at once,
   *  not `venuesPerTick`. Both budgets floor at this — falling under it means the
   *  roster ages without bound, which is not "backing off", it is falling behind. */
  private val cadenceTaskFloor: Int = {
    val spreadTicks = math.max(1L, chunkSpread.toMillis / math.max(1L, interval.toMillis))
    cadenceVenuesPerTick * spreadTicks.toInt * math.max(1, tasksPerVenue)
  }

  private val spreadAwareOutstandingBudget: Int =
    if (maxOutstandingScrapeTasks == Int.MaxValue) Int.MaxValue
    else math.max(maxOutstandingScrapeTasks, cadenceTaskFloor)
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
      .filterNot { case (_, cinemaName) => inFlight.isRunning(cinemaName) }
      .toVector
      // Oldest-fetched first; never-fetched (None) sorts ahead of any timestamp.
      .sortBy { case (key, _) =>
        (freshness.lastFetchedAt(key).map(_.toEpochMilli).getOrElse(Long.MinValue), key)
      }

    // Outstanding work in TASKS. A waiting ScrapeCinema is counted as the fan-out it is
    // about to become, not the one task it currently is: on a chunked country the
    // planner is an INTENTION to enqueue ~36 more, and a budget that ignores that keeps
    // admitting against room it has already committed. Prod showed exactly that shape —
    // ScrapeCinema=13 waiting with ScrapeChunk=0, ~470 tasks of pending fan-out read as 13.
    val perVenue = math.max(1, tasksPerVenue)
    val outstanding =
      queue.waitingCount(TaskType.ScrapeChunk) +
      queue.waitingCount(TaskType.ScrapeChunkReduce) +
      queue.waitingCount(TaskType.ScrapeCinema) * perVenue

    /** Venues admissible under a budget expressed in TASKS. Integer division floors, so
     *  room smaller than one venue's fan-out admits nothing rather than overshooting. */
    def venuesWithin(taskBudget: Int): Int =
      if (taskBudget == Int.MaxValue) Int.MaxValue
      else math.max(0, taskBudget - outstanding) / perVenue

    if (enqueueSpread <= 1) {
      // Un-spread healthy path (the default): enqueue the whole capped batch now.
      val enqueued = enqueueUpTo(due, math.min(rampedCap(now), venuesWithin(spreadAwareOutstandingBudget)))
      if (enqueued > 0) logger.info(s"ScrapeReaper enqueued $enqueued stale cinema(s) ($outstanding scrape task(s) already waiting).")
      enqueued
    } else {
      // Spread the capped batch across the interval so the parses don't clump — see
      // `enqueueSpread`. Enqueue the first slice now; defer the rest onto staggered
      // offsets. The queue dedups, so a slice landing near the next tick can't
      // double-enqueue. `tick` returns only what it enqueued SYNCHRONOUSLY (slice 0),
      // matching the un-spread contract for the first-of-batch.
      val plan = planSlices(due.take(math.min(rampedCap(now), venuesWithin(spreadAwareOutstandingBudget))), enqueueSpread)
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

object ScrapeReaper {

  /** Every task type a cinema scrape can be sitting in, and therefore everything the
   *  outstanding-task bound has to count.
   *
   *  `ScrapeCinema` alone is NOT the scrape's cost. A chunked venue's ScrapeCinema is
   *  just the PLANNER — it finishes almost immediately, having fanned out one
   *  `ScrapeChunk` per advertised day (~36 for a UK Flicks venue, ~16 for a German
   *  one) plus the `ScrapeChunkReduce` that stitches them. Counting only the planner
   *  makes the bound blind to two orders of magnitude of the work it has just created:
   *  the reaper sees a near-empty ScrapeCinema queue, judges itself under budget, and
   *  keeps enqueueing — so the pool never reaches the idle gap the bound exists to
   *  buy.
   *
   *  Measured on kinowo-worker-uk 2026-07-28, when a credit-throttle back-off used the
   *  same count: 381-1142 tasks waiting against a cap of 26, with the pool pinned busy
   *  all day. PL, whose venues mostly don't chunk, held 5-55 and recovered between
   *  ticks exactly as designed. */
  val ScrapeWorkTypes: Seq[TaskType] =
    Seq(TaskType.ScrapeCinema, TaskType.ScrapeChunk, TaskType.ScrapeChunkReduce)
}
