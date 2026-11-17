package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Paths
import testsupport.SourceTrees

class ScalaSourceScanSpec extends AnyFlatSpec with Matchers {

  "definesMain" should "find a real main definition, not a mention of one in a string or a comment" in {
    ScalaSourceScan.definesMain("object Tool {\n  def main(args: Array[String]): Unit = ()\n}") shouldBe true
    ScalaSourceScan.definesMain("object Lint {\n  val marker = \"def main(\"\n}") shouldBe false
    ScalaSourceScan.definesMain("object Lint {\n  // a program has def main(args)\n  val x = 1\n}") shouldBe false
    ScalaSourceScan.definesMain("/** a program: a file with\n *  def main(args) in it */\nobject Lint") shouldBe false
  }

  // 2026-10-19: the geodata module's ~20k lines of generated tables sat outside MainRoots, so every source lint (the
  // zone-literal rule among them) silently stopped reading them when they moved out of common.
  "MainRoots" should "name every module's main sources (testkit's, test-side code, being among TestRoots)" in {
    val mains = SourceTrees("*/src/main").directories.map(_.toString)
    mains should not be empty
    mains.filterNot(root => ScalaSourceScan.MainRoots.declared.contains(root) || ScalaSourceScan.TestRoots.declared.exists(test => root.startsWith(s"$test/")))
      .shouldBe(empty)
  }

  "isProgram" should "not take the scan itself, which names def main( only in a string, for a program" in {
    ScalaSourceScan.isProgram(Paths.get("worker/src/test/scala/tools/ScalaSourceScan.scala")) shouldBe false
  }
}
