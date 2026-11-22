package quality

import org.scalatest.LoneElement
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A declared data table is read from the code: a member val of an object built only of literals, names, constructors,
 *  collections and the operators that build them is data; a def, a branch or a lambda anywhere in it is not, so logic
 *  can't hide in one. */
class DataTablesSpec extends AnyFlatSpec with Matchers with LoneElement {

  private def verdict(member: String): DataTables.Verdict = {
    val text = s"package a\nobject Rows {\n  val Prefix = \"p\"\n$member\n  def lookup(k: String) = table.get(k)\n}\n"
    DataTables.of("A.scala", text, Set("A.scala:Rows.table")).loneElement
  }

  "a table of constructors, collections, strings and pairs" should "be data, its lines its whole val" in {
    val table = verdict(
      """  val table: Seq[(String, Row)] = Seq(
        |    "a" -> Row("one", s"$Prefix-x", 1, Some(Duration.ofSeconds(5))).mirror("why " + "more"),
        |    "b" -> Row(name = "two", tags = Set("x", "y") ++ Set("z"), 2L, None),
        |    "c" -> Row("three",
        |      "stripped".stripMargin, 3, None),
        |  )""".stripMargin)
    table.isData shouldBe true
    (table.startLine, table.endLine) shouldBe (4, 9)
  }

  "a table holding a def" should "not be data, naming the line" in {
    verdict("  val table = {\n    def row(s: String) = s\n    Seq(row(\"a\"))\n  }").offence shouldBe Some(4)
  }

  "a table holding a branch" should "not be data" in {
    verdict("  val table = Seq(\n    if (Prefix.isEmpty) \"a\" else \"b\"\n  )").offence shouldBe Some(5)
    verdict("  val table = Prefix match { case \"p\" => Seq(1); case _ => Nil }").offence shouldBe Some(4)
  }

  "a table holding a lambda" should "not be data" in {
    verdict("  val table = Seq(\"a\", \"b\")\n    .map(_.toUpperCase)").isData shouldBe false
    verdict("  val table = Seq(\"a\").map(s => s -> s.length)").isData shouldBe false
  }

  "a key naming a def, or no member" should "have no verdict" in {
    DataTables.of("A.scala", "object Rows { def table = Seq(1) }", Set("A.scala:Rows.table", "A.scala:Rows.gone")) shouldBe Nil
  }
}
