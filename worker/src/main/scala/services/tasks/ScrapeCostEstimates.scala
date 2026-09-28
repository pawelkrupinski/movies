package services.tasks

import settings.ScrapeTasksPerVenue

import java.util.concurrent.atomic.AtomicReference

/**
 * What each cinema's scrape is expected to cost the queue, in tasks — read by the
 * [[ScrapeReaper]] to admit venues against its outstanding-task budget, and by the
 * cost-spaced phase plan. A cinema's own measured cost (the mean of its recent scrapes,
 * installed by [[ScrapePhasePlanner]]) wins; before one is measured, the median of the
 * cinemas that have one; before ANY is, the country's configured prior
 * (`KINOWO_SCRAPE_TASKS_PER_VENUE`, which the deploy specs tie to the sweep arithmetic).
 */
final class ScrapeCostEstimates(prior: ScrapeTasksPerVenue) {

  private final case class Measured(byKey: Map[String, Double], median: Double)

  private val measured = new AtomicReference(Measured(Map.empty, prior.value.max(1).toDouble))

  /** Install each cinema's mean measured cost. */
  def update(meanByKey: Map[String, Double]): Unit = {
    val sorted = meanByKey.values.toVector.sorted
    measured.set(Measured(meanByKey, if (sorted.isEmpty) prior.value.max(1).toDouble else sorted(sorted.size / 2)))
  }

  /** The expected cost of this cinema's next scrape. */
  def costOf(dedupKey: String): Double = {
    val m = measured.get()
    m.byKey.getOrElse(dedupKey, m.median)
  }

  /** What a typical venue costs — for work known only by count (a waiting planner). */
  def typical: Double = measured.get().median

  /** How many of this key's cinemas have a measured cost. */
  def measuredCount(keys: Seq[String]): Int = { val m = measured.get(); keys.count(m.byKey.contains) }
}
