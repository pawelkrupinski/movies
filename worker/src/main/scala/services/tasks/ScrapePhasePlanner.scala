package services.tasks

import play.api.Logging
import services.Stoppable
import tools.DaemonExecutors

import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import scala.util.Try

/**
 * Refreshes the scrape schedule's [[ScrapeCostEstimates]] from each cinema's recorded
 * costs (the mean of its last [[ScrapeCostStore.RecentRuns]] scrapes), and rebuilds the
 * [[CostSpacedPhaseOffset]] from them: once at start,
 * then every [[ScrapePhasePlanner.ReplanInterval]]. An unreadable store keeps the plan
 * already in place — an unknown cost is not a zero one.
 */
final class ScrapePhasePlanner(
  roster:    Seq[String],
  costs:     ScrapeCostStore,
  periodFor: String => FiniteDuration,
  phases:    CostSpacedPhaseOffset,
  estimates: ScrapeCostEstimates
) extends Stoppable with Logging {

  private val scheduler = DaemonExecutors.scheduler("scrape-phase-plan")

  def replan(): Unit =
    Try(costs.recent()).fold(
      e => logger.warn(s"Scrape phase plan kept: costs unreadable (${e.getMessage})"),
      recent => {
        estimates.update(recent.collect { case (key, runs) if runs.nonEmpty => key -> runs.map(_.tasks.toDouble).sum / runs.size })
        phases.plan(CostSpacedPhaseOffset.fractions(roster, estimates.costOf, key => periodFor(key).toMillis))
        logger.info(s"Scrape phase plan: ${roster.size} cinema(s), ${estimates.measuredCount(roster)} with a measured cost, typical ${estimates.typical} task(s).")
      })

  def start(): Unit = {
    val _ = scheduler.scheduleWithFixedDelay(() => replan(), 0L, ScrapePhasePlanner.ReplanInterval.toMillis, TimeUnit.MILLISECONDS)
  }

  override def stop(): Unit = { scheduler.shutdown(); () }
}

object ScrapePhasePlanner {
  /** Costs drift over days, not minutes, and every rebuild nudges phases, so hourly
   *  is plenty. */
  val ReplanInterval: FiniteDuration = 1.hour
}
