package tools

import org.scalatest.exceptions.TestFailedException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.costs.PerformanceBudget.{Band, bytes, ceiling, operations}
import tools.costs.{PerformanceBudget, PerformanceBudgets}

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/**
 * `tools.costs.PerformanceBudgets` holds no budget nothing reads: a spec deleted or rewritten without its
 * budget would leave a number that looks guarded and is not — the stale-entry half of keeping every
 * hot path's cost in one place. And each budget is a real ceiling: named, positive where it is bytes.
 * An allocation budget is a two-sided ratchet — red above its band (a regression) and below it (a stale
 * figure) — while a ceiling bounds only from above and an operation count is exact.
 */
class PerformanceBudgetsSpec extends AnyFlatSpec with Matchers {

  import ScalaSourceScan.{TestRoots, codeOf, scalaFiles}

  private val Home       = Paths.get("testkit/src/main/scala/tools/costs/PerformanceBudgets.scala")
  private val Definition = """(?m)^\s*val\s+([A-Z]\w*)\s*=\s*(?:bytes|ceiling|operations)\(""".r

  private val declared: Seq[String] = Definition.findAllMatchIn(codeOf(Home)).map(_.group(1)).toSeq

  /** Every budget `PerformanceBudgets` defines, read off the object itself. */
  private lazy val budgets: Seq[PerformanceBudget] = classOf[PerformanceBudgets.type].getMethods.toSeq
    .filter(m => m.getParameterCount == 0 && m.getReturnType == classOf[PerformanceBudget])
    .map(_.invoke(PerformanceBudgets).asInstanceOf[PerformanceBudget])

  "every performance budget" should "be read by a spec" in {
    declared should not be empty
    val readers = scalaFiles(TestRoots).filterNot(_.endsWith(Home)).map(codeOf)
    val unread  = declared.filterNot(name => readers.exists(_.contains(s"PerformanceBudgets.$name")))
    withClue("a budget no spec reads guards nothing — delete it with its spec, or point the spec at it: ")(unread shouldBe empty)
  }

  it should "be a named ceiling, an allocation one above zero" in {
    budgets.map(_.name).sorted shouldBe budgets.map(_.name).distinct.sorted
    budgets.size shouldBe declared.size
    budgets.filter(b => b.unit == PerformanceBudget.Measure.Bytes && b.limit <= 0) shouldBe empty
  }

  private val Fixtures     = Files.createTempFile("performance-budgets-fixture", ".tsv")
  private val FixtureNames = Set("path", "noisy path", "bound", "count", "none")
  private def fixtureRows(f: Path): Seq[String] =
    if (Files.exists(f)) Files.readAllLines(f).asScala.toSeq.map(_.split('\t').head).filter(FixtureNames) else Nil
  /** What an older local run left in the real report, before this spec checked anything: not this run's doing. */
  private val leftBefore = fixtureRows(PerformanceBudget.Report.File).size

  /** The failure `check` raises on `actual`, or None when it passes. The made-up figures of this spec are
   *  recorded to a temp file: the real report is what CI uploads and `adopt-ci-budgets` reads. */
  private def verdict(budget: PerformanceBudget, actual: Long): Option[String] =
    PerformanceBudget.Report.isolated(Fixtures) {
      try { budget.check(actual); None }
      catch { case e: TestFailedException => Some(e.getMessage) }
    }

  "an allocation budget" should "fail above measured times its upper factor, as a regression" in {
    val b = bytes("path", measured = 1000)
    verdict(b, 1150) shouldBe None
    verdict(b, 1151).get should (include("exceeds its budget") and include("REGRESSION"))
  }

  it should "fail below measured times its lower factor, as a stale figure to lower" in {
    val b = bytes("path", measured = 1000)
    verdict(b, 850) shouldBe None
    verdict(b, 849).get should (include("below its budget's floor") and include("STALE FIGURE") and include("Lower `measured`"))
  }

  it should "pass anywhere inside its band, and take a wider band where one is named" in {
    val b = bytes("path", measured = 1000)
    Seq(850L, 1000L, 1150L).flatMap(verdict(b, _)) shouldBe empty
    val noisy = bytes("noisy path", measured = 1000, band = Band(upper = 1.3, lower = 0.7))
    Seq(700L, 1300L).flatMap(verdict(noisy, _)) shouldBe empty
    verdict(noisy, 699) should not be empty
    verdict(noisy, 1301) should not be empty
    an[IllegalArgumentException] should be thrownBy Band(upper = 0.9, lower = 0.8)
  }

  "a ceiling" should "bound from above only: any cost under it passes, however far under" in {
    val c = ceiling("bound", limit = 1000)
    Seq(0L, 1L, 1000L).flatMap(verdict(c, _)) shouldBe empty
    verdict(c, 1001) should not be empty
  }

  "an operation budget" should "be exact: up to its limit passes, one more fails, none never does" in {
    val o = operations("count", limit = 4)
    Seq(0L, 4L).flatMap(verdict(o, _)) shouldBe empty
    verdict(o, 5).get should include("exceeds its budget")
    verdict(operations("none", limit = 0), 1) should not be empty
  }

  "every allocation budget in PerformanceBudgets" should "be a ratchet that brackets its measured cost" in {
    val ratchets = budgets.filter(b => b.unit == PerformanceBudget.Measure.Bytes && b.floor > 0)
    ratchets should not be empty
    ratchets.filterNot(b => b.floor < b.measured && b.measured < b.limit) shouldBe empty
  }

  "this spec's fixture figures" should "land in its own file, never in the real performance-budgets.tsv" in {
    fixtureRows(Fixtures) should contain("noisy path")
    fixtureRows(PerformanceBudget.Report.File).size shouldBe leftBefore
  }
}
