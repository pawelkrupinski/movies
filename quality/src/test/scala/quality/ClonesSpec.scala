package quality

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** CPD over sources held in memory: a block of 100+ tokens copied is a clone group, a shorter one is not, and the rows
 *  key a group by the files it spans. */
class ClonesSpec extends AnyFlatSpec with Matchers {

  /** A method of about 12 tokens a line over `n` lines: `n` = 10 is past 100 tokens. */
  private def block(name: String, n: Int): String =
    s"  def $name(xs: Seq[Int]): Int = {\n" +
      (1 to n).map(i => s"    val v$i = xs.map(x => x * $i + 1).filter(_ > $i).sum\n").mkString + "    0\n  }\n"

  private def source(obj: String, body: String): String = s"package a\n\nobject $obj {\n$body}\n"

  "a block of 100 tokens or more in two files" should "be one clone group with a copy in each" in {
    val groups = Clones.of(Seq(
      "a/One.scala" -> source("One", block("f", 10)),
      "a/Two.scala" -> source("Two", block("g", 10)),
    ))
    groups.size shouldBe 1
    groups.head.tokens should be >= Clones.MinimumTokens
    groups.head.files shouldBe Seq("a/One.scala", "a/Two.scala")
    Clones.rows(groups) shouldBe Seq("a/One.scala|a/Two.scala" -> 2)
    QualityReport.cloneRows(groups) shouldBe Seq("limit\tcopies\t0", "copies\ta/One.scala|a/Two.scala\t2")
  }

  "a block under 100 tokens" should "be no clone" in {
    Clones.of(Seq("a/One.scala" -> source("One", block("f", 2)), "a/Two.scala" -> source("Two", block("g", 2)))) shouldBe Nil
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
