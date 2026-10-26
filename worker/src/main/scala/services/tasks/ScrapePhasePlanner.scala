package services.tasks

import play.api.Logging
import services.Stoppable
import tools.{DaemonExecutors, TickRecorder, TickRun, ScheduledTick}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import scala.util.Try

/**
 * Refreshes the scrape schedule's [[ScrapeCostEstimates]] from each cinema's recorded
 * costs (the mean of its last [[ScrapeCostStore.RecentRuns]] scrapes), and rebuilds the
 * [[CostSpacedPhaseOffset]] from them: once at start,
 * then every [[ScrapePhasePlanner.ReplanInterval]]. An unreadable store keeps the plan
 * already in place — an unknown cost is not a zero one.
 *
 * Each replan logs how many phases it moved, and the largest move
 * ([[CostSpacedPhaseOffset.moves]]). A replan that moves a block of phases by hours would
 * show as a burst or a hole in the sweep; when US had both on 2026-10-26 right after the
 * 11:32 replan, rebuilding the plans from `scrape_costs` showed the replan had moved
 * nothing: the costs barely move (a 5-run mean, run-to-run spread 1-3 tasks, no venue over
 * 0.1% of the load), and the cause was a sweep running slower than its window (see
 * `ScrapeReaper.cadenceTaskFloor`).
 */
final class ScrapePhasePlanner(
  roster:    Seq[String],
  costs:     ScrapeCostStore,
  periodFor: String => FiniteDuration,
  phases:    CostSpacedPhaseOffset,
  estimates: ScrapeCostEstimates,
  // Where each replan is reported (`ScheduledTickStale`).
  ticks:     TickRecorder = TickRecorder.Unrecorded
) extends Stoppable with Logging {

  private val scheduler = DaemonExecutors.scheduler("scrape-phase-plan")

  /** Plan the phases from the recorded costs: [[TickRun.Incomplete]] when they could not be read (the last plan kept). */
  def replan(): TickRun =
    Try(costs.recent()).fold(
      e => { logger.warn(s"Scrape phase plan kept: costs unreadable (${e.getMessage})"); TickRun.Incomplete },
      recent => {
        estimates.update(recent.collect { case (key, runs) if runs.nonEmpty => key -> runs.map(_.tasks.toDouble).sum / runs.size })
        val before = phases.current
        val after  = CostSpacedPhaseOffset.fractions(roster, estimates.costOf, key => periodFor(key).toMillis)
        phases.plan(after)
        val moved = CostSpacedPhaseOffset.moves(before, after, key => periodFor(key).toMillis)
        logger.info(s"Scrape phase plan: ${roster.size} cinema(s), ${estimates.measuredCount(roster)} with a measured cost, " +
          s"typical ${estimates.typical} task(s); ${moved.earlier} phase(s) moved earlier and ${moved.later} later " +
          s"by over a minute, the most by ${moved.maxMinutes} min.")
        TickRun.Completed
      })

  def start(): Unit = {
    ticks.started("scrape-phase-plan", ScrapePhasePlanner.ReplanInterval, scala.concurrent.duration.Duration.Zero)
    val _ = scheduler.scheduleWithFixedDelay(
      () => ScheduledTick.recorded("scrape-phase-plan", ScrapePhasePlanner.ReplanInterval, logger, ticks)(replan()),
      0L, ScrapePhasePlanner.ReplanInterval.toMillis, TimeUnit.MILLISECONDS)
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}

object ScrapePhasePlanner {
  /** Costs drift over days, not minutes, and every rebuild nudges phases, so hourly
   *  is plenty. */
  val ReplanInterval: FiniteDuration = 1.hour
}
