package services.tasks

import settings.{ScrapeBootRamp, ScrapeChunkSpread, ScrapeMaxEnqueuePerTick, ScrapeMaxOutstandingTasks, ScrapeTasksPerVenue}

import java.time.{Duration => JDuration, Instant}
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration._

/**
 * How much one [[ScrapeReaper]] tick may admit: the per-tick venue cap (ramped up after boot) and the
 * outstanding-task budget, each venue priced at what it costs the queue ([[ScrapeCostEstimates]]).
 *
 * @param rosterSize the venues the reaper sweeps
 * @param window     the freshness window the roster is swept once in ([[DueWindow.period]])
 * @param interval   how often the reaper ticks
 */
final class ScrapeAdmission(
  rosterSize: Int,
  window:     FiniteDuration,
  interval:   FiniteDuration,
  // Cap on how many stale cinemas a single tick enqueues. After a restart every
  // cinema can be stale at once; enqueuing all ~300 lets the TaskWorker pool
  // drain flat-out for minutes with no idle gap, exhausting the shared-CPU
  // credit balance (the boot-storm throttle spike). Capping the per-tick batch
  // so it drains inside the tick interval leaves the pool idle between minutes,
  // letting credit recover — the backlog clears over a few ticks instead. The
  // queue dedups, so already-in-flight cinemas don't re-count against the cap.
  // Default unbounded so the tests that drive `tick()` directly are unaffected.
  maxEnqueuePerTick: ScrapeMaxEnqueuePerTick = ScrapeMaxEnqueuePerTick(Int.MaxValue),
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
  bootRamp: ScrapeBootRamp = ScrapeBootRamp(0.seconds),
  // Ceiling on outstanding scrape TASKS ([[ScrapeEnqueuer.outstanding]]) — the
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
  maxOutstandingScrapeTasks: ScrapeMaxOutstandingTasks = ScrapeMaxOutstandingTasks(Int.MaxValue),
  // What one venue COSTS, in scrape tasks, so `maxOutstandingScrapeTasks` can be spent
  // in the unit it is denominated in. A chunked venue's ScrapeCinema task is only the
  // planner; the fetches it fans out arrive a moment later, so a bound checked at
  // admission time sees ~1/36th of what it is about to authorise. That is the sawtooth:
  // on 2026-07-28 kinowo-worker-uk's ScrapeCinema draining 19 -> 0 put ScrapeChunk
  // 244 -> 791 in a single step, from a reaper that believed it was under budget.
  //
  // Per VENUE, measured: each scrape records what it fanned out to (the planner, one
  // chunk per advertised day, the reduce — ~36 for a UK Flicks venue, ~16 for a German
  // Filmstarts one, 1 for an unchunked Polish venue), and a venue is admitted against
  // the mean of its own recent runs. Until a venue has one, the country's median; until
  // the country has any, its configured prior (`KINOWO_SCRAPE_TASKS_PER_VENUE`). Default:
  // a flat prior of 1, which leaves the bound behaving exactly as a venue count for
  // callers that don't wire costs.
  costs: ScrapeCostEstimates = new ScrapeCostEstimates(ScrapeTasksPerVenue(1)),
  // How long a chunked venue's fan-out is staggered over
  // ([[ScrapeCadence.ChunkEnqueueSpread]]). The budget needs it because that spread
  // sets how long a venue OCCUPIES the budget: its chunks only become eligible
  // gradually, so the venue takes the whole window to finish however idle the pool
  // is. Throughput is concurrent-venues / spread, which makes the spread a term in
  // how big the outstanding budget must be — see `spreadAwareOutstandingBudget`.
  // Default zero = no spread, leaving unchunked callers and tests unaffected.
  val chunkSpread: ScrapeChunkSpread = ScrapeChunkSpread(Duration.Zero)
) {
  import ScrapeAdmission.Room

  /** Venues a tick must admit for the roster to be swept once per freshness
   *  window — `corpus / ticksPerWindow`, rounded up. The floor under every bound
   *  below: capping enqueue is meant to smooth the pool's load, not to park the
   *  corpus, so no cap may sit under the rate the roster's own freshness window
   *  implies. Derived rather than configured, so it tracks the roster and the window
   *  instead of drifting from them. */
  private val cadenceVenuesPerTick: Int = {
    val ticksPerWindow = math.max(1L, window.toMillis / math.max(1L, interval.toMillis))
    math.max(1, math.ceil(rosterSize.toDouble / ticksPerWindow).toInt)
  }

  /** Outstanding TASKS needed for the roster to be swept once per freshness window,
   *  accounting for the spread: a venue occupies budget for the whole `chunkSpread`,
   *  so holding cadence needs `venuesPerTick x spreadTicks` of them in flight at once,
   *  not `venuesPerTick`. Both budgets floor at this — falling under it means the
   *  roster ages without bound, which is not "backing off", it is falling behind. */
  //
  // Re-read each tick: it is priced at the MEAN venue's measured cost, which moves as costs
  // are re-measured. Not the median ([[typicalVenueCost]]): a window's work is the SUM of
  // the roster's costs, and on a chunked country the median sits far under the mean. US
  // 2026-10-20..26 ran on a median-priced floor of 6 x 5 x 17 = 510 tasks against the ~930
  // its mean of 30.2 needs, with the pool half idle: ~210 chunked venues finished an hour
  // against the 297 a 14h sweep of 4,161 needs, so the sweep took ~16h. Each pass fell ~2h
  // further behind the boundaries, and once a venue's scrape landed more than half a period
  // late, nearest-boundary counting ([[tools.DueBoundary.NearestBoundary]]) took it
  // for an EARLY scrape for the next boundary, so the venue skipped a whole period. Every ~3
  // days that turned a whole cohort of chunked venues un-due at once: their chunks drained,
  // the freed room let the backlog of cheap unchunked venues through 40 a tick (~600 in
  // 20 minutes), and then ~3h with almost nothing due (10-20, 10-23, 10-26 near midday UTC).
  private def cadenceTaskFloor: Int = {
    val spreadTicks = math.max(1L, chunkSpread.value.toMillis / math.max(1L, interval.toMillis))
    cadenceVenuesPerTick * spreadTicks.toInt * math.max(1, math.ceil(costs.mean).toInt)
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
  private def spreadAwareOutstandingBudget: Int =
    if (maxOutstandingScrapeTasks.value == Int.MaxValue) Int.MaxValue
    else math.max(maxOutstandingScrapeTasks.value, cadenceTaskFloor)

  /** A typical venue's cost in whole tasks, never under one. */
  def typicalVenueCost: Int = math.max(1, math.ceil(costs.typical).toInt)

  /** What admitting the venue keyed `key` spends. */
  def costOf(key: String): Double = costs.costOf(key)

  // Instant of the first tick, anchoring the post-boot ramp; set once, then read-only.
  private val rampAnchor = new AtomicReference[Option[Instant]](None)

  /** The non-throttled per-tick cap, ramped up over `bootRamp` from the first tick.
   *  A no-op (returns `maxEnqueuePerTick`) when the ramp is disabled or the cap is
   *  unbounded — so the default configuration and direct-`tick()` tests are unchanged.
   *  Pure given `now` and the (once-set) anchor. */
  def rampedCap(now: Instant): Int =
    if (bootRamp.value.toMillis <= 0 || maxEnqueuePerTick.value == Int.MaxValue) maxEnqueuePerTick.value
    else {
      val anchor  = rampAnchor.updateAndGet(prev => if (prev.isDefined) prev else Some(now)).get
      val elapsed = math.max(0L, JDuration.between(anchor, now).toMillis)
      if (elapsed >= bootRamp.value.toMillis) maxEnqueuePerTick.value
      else {
        val floorCap = math.max(1, maxEnqueuePerTick.value / 5)
        val scaled   = math.ceil(maxEnqueuePerTick.value.toDouble * elapsed / bootRamp.value.toMillis).toInt
        math.min(maxEnqueuePerTick.value, math.max(floorCap, scaled))
      }
    }

  /** The room left under the outstanding-task budget with `outstanding` tasks already waiting. */
  def room(outstanding: Int): Room = {
    val budget = spreadAwareOutstandingBudget
    new Room(if (budget == Int.MaxValue) Double.MaxValue else math.max(0, budget - outstanding).toDouble, costOf, typicalVenueCost)
  }

  /** No budget at all: what a spread tick's slices are enqueued under, the room already spent at the tick. */
  val unbounded: Room = new Room(Double.MaxValue, costOf, typicalVenueCost)

  /** The oldest-first prefix of `due` that fits `room`. */
  def admissible(due: Vector[(String, String)], room: Room): Vector[(String, String)] = {
    var spent = 0.0
    due.takeWhile { case (key, _) =>
      val ok = room.fits(spent, key, first = spent == 0.0)
      if (ok) spent += costOf(key)
      ok
    }
  }
}

object ScrapeAdmission {

  /** The room left under a budget in TASKS, and the one rule for what fits in it: each
   *  venue priced at its own expected cost, so one heavy venue takes the room of many
   *  light ones. Admission stops at the first venue that doesn't fit rather than
   *  skipping it for lighter ones behind, or it would never get in; and a venue heavier
   *  than the whole budget still goes, first and alone, once the room could hold a
   *  typical venue — without that it would stay overdue forever. */
  final class Room private[ScrapeAdmission] (left: Double, costOf: String => Double, typicalVenueCost: Int) {
    def fits(spent: Double, key: String, first: Boolean): Boolean =
      spent + costOf(key) <= left || (first && left >= typicalVenueCost)
  }
}
