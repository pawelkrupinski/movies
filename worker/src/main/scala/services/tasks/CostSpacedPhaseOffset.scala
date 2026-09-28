package services.tasks

import services.cadence.DueBoundary

import java.util.concurrent.atomic.AtomicReference

/**
 * The scrape schedule's phases, spaced by what each cinema COSTS the queue rather
 * than hashed so that every cinema gets an equal slice.
 *
 * A hashed phase spreads cinemas evenly by count, but one chunked venue can fan out
 * 100 tasks while its neighbour costs one, so equal slices of cinemas are wildly
 * unequal slices of work — and wherever the hash happens to land two heavy venues
 * close together, the queue spikes. Here the roster keeps its hashed ORDER, but each
 * cinema's phase is the running share of load before it, so every cinema's slot is
 * as wide as its own cost. Replayed against 2026-09-21..28's chunk plans, spacing by
 * the average of each venue's first five runs cut the p99 of held chunks by 18-34%
 * per country against the hash (UK 323→213, US 892→708, DE 152→111, ES 70→57,
 * PL 95→78), and the peak by more.
 *
 * Load is cost / period, so a venue on a shortened period (VenueScrapeCadence) weighs
 * in by how often it actually runs. Costs come from [[ScrapeCostEstimates]].
 *
 * This holds the current plan; [[ScrapePhasePlanner]] rebuilds it from the persisted
 * costs. A moved phase is safe only because the scrape [[DueWindow]] counts each
 * scrape toward its NEAREST boundary ([[services.cadence.DueBoundary.NearestBoundary]]).
 * Until the first plan lands, and for any key it doesn't know, the hashed phase
 * stands in — so this needs nothing to construct, and the wiring can hand it to the
 * eagerly-built scrape `DueWindow`.
 */
final class CostSpacedPhaseOffset extends PhaseOffset {

  private val fractions = new AtomicReference(Map.empty[String, Double])

  def millis(dedupKey: String, periodMillis: Long): Long =
    fractions.get().get(dedupKey) match {
      case Some(fraction) => (fraction * periodMillis).toLong
      case None           => DueBoundary.hashedPhaseMillis(dedupKey, periodMillis)
    }

  /** Install a new plan: each key's phase as a fraction of its period. */
  def plan(fractionsByKey: Map[String, Double]): Unit = fractions.set(fractionsByKey)
}

object CostSpacedPhaseOffset {

  /** Each key's phase as a fraction of its period: the roster in hashed order, each
   *  key starting where the running share of load (cost / period) before it ends. */
  def fractions(keys: Seq[String], costOf: String => Double, periodMillisOf: String => Long): Map[String, Double] = {
    val ordered = keys.distinct.sortBy(key => (DueBoundary.hashedPhaseMillis(key, Int.MaxValue.toLong), key))
    val loads   = ordered.map(key => costOf(key).max(0.0) / periodMillisOf(key).max(1L).toDouble)
    val total   = loads.sum
    if (total <= 0) Map.empty
    else ordered.zip(loads.scanLeft(0.0)(_ + _)).map { case (key, before) => key -> before / total }.toMap
  }
}
