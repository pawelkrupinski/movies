package tools.costs

import org.scalatest.Assertion
import org.scalatest.Assertions.{fail, succeed}

/**
 * A ceiling on what one hot path may cost: heap allocated, or operations done (cache rebuilds,
 * rows fetched, Mongo commands sent, DOM queries made). Every budget lives in [[PerformanceBudgets]],
 * the one place to read what each path costs today and to change it deliberately.
 *
 * An allocation budget is the cost MEASURED when it was set, times [[PerformanceBudget.Margin]]: a
 * regression worth catching (a cache that stopped hitting, a copy per element) costs a multiple,
 * while JIT and collection-sizing noise stays well inside half again. An operation budget is exact:
 * a count is deterministic, so its ceiling is the number the path must not exceed.
 */
final case class PerformanceBudget(name: String, limit: Long, unit: PerformanceBudget.Measure, measured: Long) {

  /** Passes when `actual` is within the limit; fails naming the budget, what it measured when set,
   *  and how to change it. `detail` says what was measured (a breakdown, the sizes involved). */
  def check(actual: Long, detail: => String = ""): Assertion =
    if (actual <= limit) succeed
    else fail(s"$name: ${unit.render(actual)} exceeds its budget of ${unit.render(limit)} " +
      s"(${unit.render(measured)} when the budget was set)${if (detail.isEmpty) "" else s"; $detail"}.\n" +
      "A regression: find what started costing more. A deliberate change: re-measure and update " +
      "tools.costs.PerformanceBudgets as its header says, with the before/after in the commit message.")

  /** The budget as one line — for a report beside the measured value. */
  def render(actual: Long): String = s"$name: ${unit.render(actual)} of ${unit.render(limit)}"
}

object PerformanceBudget {

  /** How far an allocation may grow past what was measured before its spec fails. */
  val Margin: Double = 1.5

  enum Measure {
    case Bytes, Operations
    def render(value: Long): String = this match {
      case Bytes      => if (value >= (1L << 20)) f"${value / 1048576.0}%.2f MB" else f"${value / 1024.0}%.1f KB"
      case Operations => value.toString
    }
  }

  /** At most [[Margin]] times the `measured` bytes. */
  def bytes(name: String, measured: Long): PerformanceBudget =
    PerformanceBudget(name, math.ceil(measured * Margin).toLong, Measure.Bytes, measured)

  /** At most `limit` bytes — a bound the path must stay under whatever it costs today, not a measure of it. */
  def ceiling(name: String, limit: Long): PerformanceBudget = PerformanceBudget(name, limit, Measure.Bytes, limit)

  /** At most `limit` operations — exactly, a count does not drift. */
  def operations(name: String, limit: Long): PerformanceBudget = PerformanceBudget(name, limit, Measure.Operations, limit)
}
