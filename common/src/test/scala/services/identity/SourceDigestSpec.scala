package services.identity

import kinowo.build.SourceDigest
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What the rules and venue slot versions digest of a Scala source (`project/SourceDigest.scala`): its code, without
 *  the comments and spare whitespace no compiler reads — and never a byte of a literal, an indentation, or a line end
 *  or blank line the parser could read differently. */
class SourceDigestSpec extends AnyFlatSpec with Matchers {

  private def code(source: String) = SourceDigest.code(source)
  private val q3 = "\"\"\""

  "a Scala source's code" should "drop line comments and trailing whitespace" in {
    code("val x = 1 // one\nval y = 2   \n") shouldBe "val x = 1\nval y = 2\n"
  }

  it should "drop a line only a comment held, without leaving a blank line there" in {
    code("val x = 1\n  // about y\nval y = 2\n") shouldBe "val x = 1\nval y = 2\n"
    code("val x = 1\n  /* about\n     y */\nval y = 2\n") shouldBe "val x = 1\nval y = 2\n"
  }

  it should "drop Scaladoc" in {
    code("  /** The answer.\n   *  @return 42 */\n  def f: Int = 42\n") shouldBe "  def f: Int = 42\n"
  }

  it should "drop nested block comments whole" in {
    code("val x = /* a /* b */ still comment */ 1\n") shouldBe "val x =   1\n"
    code("/* a /* b */ c */ val x = 1\n") shouldBe "                  val x = 1\n"
  }

  it should "keep a run of blank lines as one blank line, and add none where there was none" in {
    code("def f = 1\n\n\n\ndef g = 2\n") shouldBe "def f = 1\n\ndef g = 2\n"
    code("\n\nfoo\n\n  \n{ x }\n\n") shouldBe "foo\n\n{ x }\n\n"
    code("foo\n// c\n{ x }\n") shouldBe "foo\n{ x }\n"
  }

  it should "keep the indentation, and a token's column after a comment before it" in {
    code("  if a then\n\tb\n") shouldBe "  if a then\n\tb\n"
    code("  /* c */ x\n") shouldBe "          x\n"
    code("f /* a\n  b */ x\n") shouldBe "f\n       x\n"
  }

  it should "keep comment markers inside string literals" in {
    code("val u = \"http://x/*y*/\" // c\n") shouldBe "val u = \"http://x/*y*/\"\n"
    code("val e = \"a\\\"//b\"\n") shouldBe "val e = \"a\\\"//b\"\n"
  }

  it should "keep a triple-quoted string exactly, its comment markers, trailing whitespace and blank lines too" in {
    val literal = s"${q3}a // b  \n\n  /* c */ \n\"d\"\"$q3"
    code(s"val t = $literal // e\n") shouldBe s"val t = $literal\n"
  }

  it should "keep interpolations exactly, their splices and escaped dollars and quotes" in {
    val s1 = "s\"//${x /* y */ + \"//\"}$$/*\""
    code(s"val i = $s1 // z\n") shouldBe s"val i = $s1\n"
    val s2 = "s\"$\"//\""
    code(s"val j = $s2\n") shouldBe s"val j = $s2\n"
    val s3 = s"f${q3}// ${"$"}{ s\"}\" + 1 } /*$q3"
    code(s"val k = $s3 // w\n") shouldBe s"val k = $s3\n"
  }

  it should "keep character literals and backquoted names that look like comment markers" in {
    code("val c = '/' // d\nval q = '\"'\nval a = '\\''\n") shouldBe "val c = '/'\nval q = '\"'\nval a = '\\''\n"
    code("val `a//b` = 1 // c\n") shouldBe "val `a//b` = 1\n"
    code("val x = '{ y } // c\n") shouldBe "val x = '{ y }\n"
  }

  it should "leave a source it cannot lex as it is" in {
    code("val s = \"open // not closed\n") shouldBe "val s = \"open // not closed\n"
    code("/* never closed\nval x = 1\n") shouldBe "/* never closed\nval x = 1\n"
  }

  "the digest" should "not move on a comment-only or whitespace-only edit to a Scala source" in {
    val before = Map("scala/A.scala" -> "object A {\n  val x = 1\n\n  val y = 2\n}\n")
    val after  = Map("scala/A.scala" -> "/** A. */\nobject A {   \n  val x = 1 // one\n\n  /* two */\n\n  val y = 2\n}\n")
    digest(after) shouldBe digest(before)
  }

  it should "move on a code edit, and on any edit to a file that is not Scala" in {
    val before = Map("scala/A.scala" -> "object A { val x = 1 }\n", "resources/a.json" -> "{\"a\": 1}")
    digest(before + ("scala/A.scala" -> "object A { val x = 2 }\n")) should not be digest(before)
    digest(before + ("scala/A.scala" -> "object A { val x = \"1 // 2\" }\n")) should not be digest(before + ("scala/A.scala" -> "object A { val x = \"1\" }\n"))
    digest(before + ("resources/a.json" -> "{\"a\": 1} ")) should not be digest(before)
  }

  private def digest(files: Map[String, String]): String =
    SourceDigest.of(files.keys.toSeq.sorted, path => files(path).getBytes("UTF-8"))
}
