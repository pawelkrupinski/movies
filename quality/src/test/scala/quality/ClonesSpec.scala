package quality

import org.scalatest.LoneElement
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** CPD over sources held in memory: a block of 50+ tokens copied is a clone group, a shorter one is not, imports alike
 *  are none, and the rows key a group by the files it spans. */
class ClonesSpec extends AnyFlatSpec with Matchers with LoneElement {

  /** A method of about 20 tokens a line over `n` lines: `n` = 3 is past 50 tokens, `n` = 1 under it. */
  private def block(name: String, n: Int): String =
    s"  def $name(xs: Seq[Int]): Int = {\n" +
      (1 to n).map(i => s"    val v$i = xs.map(x => x * $i + 1).filter(_ > $i).sum\n").mkString + "    0\n  }\n"

  private def source(obj: String, body: String): String = s"package a\n\nobject $obj {\n$body}\n"

  "a block of 50 tokens or more in two files" should "be one clone group with a copy in each" in {
    val groups = Clones.of(Seq(
      "a/One.scala" -> source("One", block("f", 10)),
      "a/Two.scala" -> source("Two", block("g", 10)),
    ))
    groups.size shouldBe 1
    groups.head.tokens should be >= Clones.MinimumTokens
    groups.head.files shouldBe Seq("a/One.scala", "a/Two.scala")
    Clones.rows(groups) shouldBe Seq("a/One.scala|a/Two.scala" -> 2)
    QualityReport.cloneRows(groups) shouldBe
      Seq("limit\tcopies\t0", "limit\tclone-tokens\t50", "copies\ta/One.scala|a/Two.scala\t2")
  }

  "a block under 50 tokens" should "be no clone" in {
    Clones.of(Seq("a/One.scala" -> source("One", block("f", 1)), "a/Two.scala" -> source("Two", block("g", 1)))) shouldBe Nil
  }

  "a block of 50 to 100 tokens" should "be a clone (the limit was 100 tokens until 2026-11-20)" in {
    val groups = Clones.of(Seq("a/One.scala" -> source("One", block("f", 3)), "a/Two.scala" -> source("Two", block("g", 3))))
    groups.map(_.tokens).loneElement should (be >= 50 and be < 100)
    QualityReport.cloneRows(groups) shouldBe
      Seq("limit\tcopies\t0", "limit\tclone-tokens\t50", "copies\ta/One.scala|a/Two.scala\t2")
  }

  "two sources importing alike" should "be no clone, and keep their lines" in {
    val imports = (1 to 12).map(i => s"import a.b$i.{C$i, D$i}\n").mkString +
      "import scala.concurrent.{\n  ExecutionContext,\n  Future\n}\n"
    def importing(obj: String) = s"package a.b.c\n\n$imports\nobject $obj {\n  val x = 1\n}\n"
    Clones.of(Seq("a/One.scala" -> importing("One"), "a/Two.scala" -> importing("Two"))) shouldBe Nil
    val blanked = Clones.withoutImports(importing("One"))
    blanked.linesIterator.size shouldBe importing("One").linesIterator.size
    blanked should not include "import"
    blanked should not include "Future"
    blanked should include("object One {")
  }

  "a third copy" should "be counted in its group's row, and a copy within one file keyed by that file alone" in {
    val three = Clones.of(Seq(
      "a/One.scala" -> source("One", block("f", 10)),
      "a/Two.scala" -> source("Two", block("g", 10)),
      "a/Three.scala" -> source("Three", block("h", 10)),
    ))
    Clones.rows(three) shouldBe Seq("a/One.scala|a/Three.scala|a/Two.scala" -> 3)
    Clones.rows(Clones.of(Seq("a/One.scala" -> source("One", block("f", 10) + "  val sep = 1\n" + block("g", 10))))) shouldBe
      Seq("a/One.scala" -> 2)
  }
}
