package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.costs.{PerformanceBudget, PerformanceBudgets}

import java.nio.file.Paths

/**
 * `tools.costs.PerformanceBudgets` holds no budget nothing reads: a spec deleted or rewritten without its
 * budget would leave a number that looks guarded and is not — the stale-entry half of keeping every
 * hot path's cost in one place. And each budget is a real ceiling: named, positive where it is bytes.
 */
class PerformanceBudgetsSpec extends AnyFlatSpec with Matchers {

  import ScalaSourceScan.{TestRoots, codeOf, scalaFiles}

  private val Home       = Paths.get("testkit/src/main/scala/tools/costs/PerformanceBudgets.scala")
  private val Definition = """(?m)^\s*val\s+([A-Z]\w*)\s*=\s*(?:bytes|ceiling|operations)\(""".r

  private val declared: Seq[String] = Definition.findAllMatchIn(codeOf(Home)).map(_.group(1)).toSeq

  "every performance budget" should "be read by a spec" in {
    declared should not be empty
    val readers = scalaFiles(TestRoots).filterNot(_.endsWith(Home)).map(codeOf)
    val unread  = declared.filterNot(name => readers.exists(_.contains(s"PerformanceBudgets.$name")))
    withClue("a budget no spec reads guards nothing — delete it with its spec, or point the spec at it: ")(unread shouldBe empty)
  }

  it should "be a named ceiling, an allocation one above zero" in {
    val budgets = classOf[PerformanceBudgets.type].getMethods.toSeq
      .filter(m => m.getParameterCount == 0 && m.getReturnType == classOf[PerformanceBudget])
      .map(_.invoke(PerformanceBudgets).asInstanceOf[PerformanceBudget])
    budgets.map(_.name).sorted shouldBe budgets.map(_.name).distinct.sorted
    budgets.size shouldBe declared.size
    budgets.filter(b => b.unit == PerformanceBudget.Measure.Bytes && b.limit <= 0) shouldBe empty
  }
}
