package tools.costs

import tools.costs.PerformanceBudget.{bytes, ceiling, operations}

/**
 * Every hot path's cost budget, in one place: what it measured when its budget was set, and the
 * ceiling its spec holds it to. Each names the spec that reads it, and `PerformanceBudgetsSpec`
 * fails on one no spec reads.
 *
 * HOW TO UPDATE ONE — deliberately, never to make a red build green:
 *  1. Find out WHY the path costs more. A regression is fixed, not budgeted: a cache that stopped
 *     hitting, a copy per element, a read of rows nobody keeps.
 *  2. When the cost is a deliberate trade (a page that now shows more), re-measure on the parent
 *     commit and on yours with the spec's own `info` lines (each prints `name: actual of limit`),
 *     on an idle machine, and set `measured` to yours.
 *  3. Say in the commit message what moved and both numbers. A path made cheaper lowers its budget
 *     in the same commit, so the gain is held from then on.
 *
 * Allocation is per thread (`AllocationMeter`), warmed and the median of several runs, so it holds on
 * a loaded machine; operation counts are exact.
 */
object PerformanceBudgets {

  private def kb(n: Double): Long = (n * 1024).toLong
  private def mb(n: Double): Long = (n * 1024 * 1024).toLong

  // ── Web ───────────────────────────────────────────────────────────────────────────────────────

  /** `FilterDescriptionSpec`: an unfiltered listing's description over 600 films, which must not build the
   *  filters' universes (~1 MB a New York render when it did). */
  val UnfilteredListingDescription = bytes("unfiltered listing description, 600 films", measured = kb(4.0))

  // ── Fixed ceilings: a bound the path must stay under, not a measured cost ────────────────────

  /** `ShareCardPostersSpec`: refusing an 8000×12000 progressive JPEG before decoding it, which would
   *  take hundreds of megabytes of raster. */
  val GiantProgressivePosterRefusal = ceiling("refusing a giant progressive JPEG", limit = mb(4))
}
