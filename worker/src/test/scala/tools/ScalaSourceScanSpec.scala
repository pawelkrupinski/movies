package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Paths

class ScalaSourceScanSpec extends AnyFlatSpec with Matchers {

  "definesMain" should "find a real main definition, not a mention of one in a string or a comment" in {
    ScalaSourceScan.definesMain("object Tool {\n  def main(args: Array[String]): Unit = ()\n}") shouldBe true
    ScalaSourceScan.definesMain("object Lint {\n  val marker = \"def main(\"\n}") shouldBe false
    ScalaSourceScan.definesMain("object Lint {\n  // a program has def main(args)\n  val x = 1\n}") shouldBe false
    ScalaSourceScan.definesMain("/** a program: a file with\n *  def main(args) in it */\nobject Lint") shouldBe false
  }

  "isProgram" should "not take the scan itself, which names def main( only in a string, for a program" in {
    ScalaSourceScan.isProgram(Paths.get("worker/src/test/scala/tools/ScalaSourceScan.scala")) shouldBe false
  }
}
