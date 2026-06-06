package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Concurrency-budget behaviour for the worker's scrape/enrich fan-out. The
 *  overlapping-task probe is shared with the common [[DaemonExecutorsSpec]] via
 *  [[ExecutorProbes]] (re-exported through `common % "test->test"`). */
class SharedExecutionBudgetSpec extends AnyFlatSpec with Matchers {

  "SharedExecutionBudget" should "cap total concurrency across every EC it hands out" in {
    val budget = new SharedExecutionBudget(2)
    // Two distinct ECs from the same budget — work submitted to either still
    // shares the one permit pool, so the combined peak can't exceed 2.
    val peak = ExecutorProbes.peakConcurrency(12, IndexedSeq(budget.ec("a"), budget.ec("b")))
    peak should be <= 2
  }

  it should "run unbounded when maxConcurrent <= 0" in {
    val budget = new SharedExecutionBudget(0)
    // No permit gate: all 8 overlapping tasks run at once.
    val peak = ExecutorProbes.peakConcurrency(8, IndexedSeq(budget.ec("unbounded")))
    peak should be > 2
  }

  "a sub-limited EC from a budget" should "cap its own tasks at the sub-limit even with budget to spare" in {
    val budget = new SharedExecutionBudget(8)
    // subLimit=2: this EC alone never runs more than 2 at once, though the
    // budget would allow up to 8.
    val peak = ExecutorProbes.peakConcurrency(10, IndexedSeq(budget.ec("scrape", subLimit = 2)))
    peak should be <= 2
  }

  it should "still draw from — and stay within — the shared budget" in {
    val budget = new SharedExecutionBudget(3)
    // scrape capped at 2, plus a normal sibling EC; combined they can't exceed
    // the budget of 3 (and scrape's own share never exceeds 2).
    val peak = ExecutorProbes.peakConcurrency(12, IndexedSeq(budget.ec("scrape", subLimit = 2), budget.ec("other")))
    peak should be <= 3
  }
}
