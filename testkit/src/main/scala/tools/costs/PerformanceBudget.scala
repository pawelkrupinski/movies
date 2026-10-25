package tools.costs

import org.scalatest.Assertion
import org.scalatest.Assertions.{fail, succeed}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import scala.util.Try

/**
 * What one hot path may cost: heap allocated, or operations done (cache rebuilds, rows fetched, Mongo
 * commands sent, DOM queries made). Every budget lives in [[PerformanceBudgets]], the one place to read
 * what each path costs today and to change it deliberately.
 *
 * An allocation budget is a RATCHET around the cost MEASURED when it was set: it fails above `measured`
 * times its band's upper factor (a regression) AND below its lower factor (a stale figure: the path got
 * cheaper, and a budget left at the old cost would let it grow back unnoticed). A ceiling only fails
 * above, never below — it is a bound the path must stay under, not a measure of it. An operation budget
 * is exact: a count is deterministic, so its ceiling is the number the path must not exceed.
 *
 * Why a ratchet: on 2026-10-24 an identity change added 4 MB to every quiet projection tick (5.1 -> 9.4
 * MB, noise under 0.06 MB) and every check stayed green, because the ceiling was 1.5x a `measured` that
 * was itself wrong on both sides of the jump.
 */
final case class PerformanceBudget(
  name: String,
  limit: Long,
  unit: PerformanceBudget.Measure,
  measured: Long,
  floor: Long = 0L
) {

  /** Passes when `actual` is within `floor` to `limit`; fails naming the budget, what it measured when
   *  set, and what to do. `detail` says what was measured (a breakdown, the sizes involved). Every
   *  check is also recorded ([[PerformanceBudget.Report]]), so one run lists every figure. */
  def check(actual: Long, detail: => String = ""): Assertion = {
    PerformanceBudget.Report.record(this, actual)
    def said = if (detail.isEmpty) "" else s"; $detail"
    if (actual > limit)
      fail(s"$name: ${unit.render(actual)} exceeds its budget of ${unit.render(limit)} " +
        s"(${unit.render(measured)} when the budget was set)$said.\n" +
        "A REGRESSION: find what started costing more and fix it. Only a deliberate trade (a page that now " +
        "shows more) raises `measured` in tools.costs.PerformanceBudgets, re-measured as its header says, " +
        "with the before/after in the commit message.")
    else if (actual < floor)
      fail(s"$name: ${unit.render(actual)} is below its budget's floor of ${unit.render(floor)} " +
        s"(${unit.render(measured)} when the budget was set)$said.\n" +
        s"A STALE FIGURE: the path got cheaper. Lower `measured` to ${unit.render(actual)} in " +
        "tools.costs.PerformanceBudgets in this same commit (re-measured as its header says, before/after in " +
        "the commit message), so the gain is held from now on.")
    else succeed
  }

  /** The budget as one line — for a report beside the measured value. */
  def render(actual: Long): String =
    if (floor > 0)
      s"$name: ${unit.render(actual)}, held to ${unit.render(floor)}..${unit.render(limit)} " +
        s"(measured ${unit.render(measured)})"
    else s"$name: ${unit.render(actual)} of ${unit.render(limit)}"
}

object PerformanceBudget {

  /** How far an allocation may move from what was measured before its spec fails: above `measured ×
   *  upper` is a regression, below `measured × lower` a stale figure. */
  final case class Band(upper: Double, lower: Double) {
    require(upper > 1.0 && lower > 0.0 && lower < 1.0, s"a band brackets the measured cost: $this")
  }

  /** The band every allocation budget gets unless it names a wider one. Allocation is per thread,
   *  warmed and the median of several runs ([[AllocationMeter]]): repeated runs, in one JVM or fresh
   *  ones, land within a few percent of each other. 15% either way keeps that noise green, while a cache
   *  that stopped hitting, a copy per element or a read model rebuilt — tens of percent, usually
   *  multiples — goes red, and so does a gain nobody wrote down. */
  val DefaultBand: Band = Band(upper = 1.15, lower = 0.85)

  enum Measure {
    case Bytes, Operations
    def render(value: Long): String = this match {
      case Bytes      => if (value >= (1L << 20)) f"${value / 1048576.0}%.2f MB" else f"${value / 1024.0}%.1f KB"
      case Operations => value.toString
    }
  }

  /** `measured` bytes, held within `band`: a regression above it, a stale figure below. A path whose
   *  measured noise exceeds the default band names a wider one, with a comment saying why. */
  def bytes(name: String, measured: Long, band: Band = DefaultBand): PerformanceBudget =
    PerformanceBudget(
      name,
      limit = math.ceil(measured * band.upper).toLong,
      unit = Measure.Bytes,
      measured = measured,
      floor = math.floor(measured * band.lower).toLong
    )

  /** At most `limit` bytes — a bound the path must stay under whatever it costs today, not a measure of it. */
  def ceiling(name: String, limit: Long): PerformanceBudget = PerformanceBudget(name, limit, Measure.Bytes, limit)

  /** At most `limit` operations — exactly, a count does not drift. */
  def operations(name: String, limit: Long): PerformanceBudget = PerformanceBudget(name, limit, Measure.Operations, limit)

  /** Every check appends `name<TAB>actual<TAB>measured<TAB>floor<TAB>limit` (raw numbers) to
   *  `target/performance-budgets.tsv` under the test JVM's working directory, so one run lists every
   *  figure beside its budget — what re-measuring them reads. Best effort: it never fails a check.
   *  A spec of the budget machinery itself checks made-up figures inside [[isolated]], so they never reach it. */
  object Report {
    val File = Paths.get("target", "performance-budgets.tsv")

    private val redirect = new ThreadLocal[Path]

    /** Runs `body` with this thread's checks recorded to `to` instead of [[File]]. */
    def isolated[A](to: Path)(body: => A): A = {
      redirect.set(to)
      try body
      finally redirect.remove()
    }

    def record(budget: PerformanceBudget, actual: Long): Unit = {
      val line = Seq(budget.name, actual, budget.measured, budget.floor, budget.limit).mkString("", "\t", "\n")
      val file = Option(redirect.get).getOrElse(File)
      Try {
        Files.createDirectories(file.toAbsolutePath.getParent)
        Files.write(file, line.getBytes(UTF_8), StandardOpenOption.CREATE, StandardOpenOption.APPEND)
      }
      ()
    }
  }
}
