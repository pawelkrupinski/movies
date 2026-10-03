package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Every Scaladoc block in production code sits directly on the member it documents.
 *
 * Scaladoc attaches a `/** … */` to the NEXT definition, so a doc that is followed by a
 * blank line, by a second doc, or by a statement documents something else or nothing: an
 * edit that inserts a member between a doc and its definition silently moves the
 * description onto the newcomer, and the old member reads as undocumented. That drift
 * was fixed by hand in nearly every review round; this makes it fail the build instead.
 *
 * Allowed between a doc and its definition: annotations, modifiers and `//` comments. A
 * "definition" is a `def`/`val`/`var`/`class`/`object`/`trait`/`enum`/`case`/`given`/
 * `type`/`extension`/`package` — or a constructor parameter (`name: Type`), which is how
 * the codebase documents individual parameters. A file-level note that describes no one
 * member is a plain `/* … */` comment, not a Scaladoc.
 */
class DocCommentPlacementSpec extends AnyFlatSpec with Matchers {

  import DocCommentPlacementSpec._
  import ScalaSourceScan._

  "the detector" should "pass a doc directly on a definition, through annotations, modifiers and line comments" in {
    misplacedDocs("/** A. */\ndef a = 1") shouldBe empty
    misplacedDocs("/** A. */ def a = 1") shouldBe empty
    misplacedDocs("/** A.\n *  More. */\n@deprecated(\"x\", \"1\")\nprivate[movies] final case class A(x: Int)") shouldBe empty
    misplacedDocs("/** A. */\n// why it is lazy\nlazy val a = 1") shouldBe empty
    misplacedDocs("/** A. */\noverride protected def a = 1") shouldBe empty
    misplacedDocs("class A(\n  /** the x */\n  x: Int,\n  /** the y */\n  y: => Int)") shouldBe empty
    misplacedDocs("/** Pkg. */\npackage object p") shouldBe empty
    misplacedDocs("val glob = \"/** not a doc */\"\nval b = 2") shouldBe empty
    misplacedDocs("// see /** this */\nval b = 2") shouldBe empty
  }

  it should "flag a doc followed by a blank line, a second doc, a block comment or a statement" in {
    misplacedDocs("/** A. */\n\ndef a = 1").map(_.reason) shouldBe Seq(BlankLine)
    misplacedDocs("/** A. */\n/** B. */\ndef b = 1").map(_.reason) shouldBe Seq(AnotherDoc)
    misplacedDocs("/** A. */\n/* note */\ndef a = 1").map(_.reason) shouldBe Seq(NotADefinition)
    misplacedDocs("/** A. */\nimport x.y").map(_.reason) shouldBe Seq(NotADefinition)
    misplacedDocs("/** A. */\nfoo(1)").map(_.reason) shouldBe Seq(NotADefinition)
    misplacedDocs("object O {\n  /** A. */\n}").map(_.reason) shouldBe Seq(NotADefinition)
    misplacedDocs("/** A. */").map(_.reason) shouldBe Seq(NotADefinition)
  }

  it should "name the line the misplaced doc opens on" in {
    misplacedDocs("val a = 1\n\n/** A.\n *  B. */\n\ndef b = 2").map(_.line) shouldBe Seq(3)
  }

  "Production sources" should "put every Scaladoc directly on the member it documents" in {
    val offenders = scalaFiles(MainRoots).flatMap(path => misplacedDocs(read(path)).map(m => s"$path:${m.line} ${m.reason}"))
    withClue("Move each of these docs onto its member, merge it with the member's own doc, or delete it " +
      "(a note about no one member is a plain /* */ comment):\n" + offenders.mkString("\n") + "\n") {
      offenders shouldBe empty
    }
  }
}

object DocCommentPlacementSpec {

  sealed trait Reason
  case object BlankLine      extends Reason { override def toString = "is followed by a blank line" }
  case object AnotherDoc     extends Reason { override def toString = "is followed by another Scaladoc" }
  case object NotADefinition extends Reason { override def toString = "is not followed by a definition" }

  final case class MisplacedDoc(line: Int, reason: Reason)

  private val Modifier   = """(?:@[\w.]+(?:\([^)]*\))?\s*|(?:private|protected)(?:\[[\w.]+\])?\s+|(?:final|sealed|abstract|override|implicit|lazy|inline|open|transparent|opaque|infix)\s+)"""
  private val Definition = ("""^""" + Modifier + """*(?:(?:def|val|var|class|object|trait|enum|case|given|type|extension|package)\b|[\w$]+\s*:)""").r
  private val Annotation = """^@[\w.]+(?:\(.*\))?\s*$""".r

  /** Every `/** … */` in `source` that does not sit directly on a definition. */
  def misplacedDocs(source: String): Seq[MisplacedDoc] = {
    val found = Seq.newBuilder[MisplacedDoc]
    var open  = source.indexOf("/**")
    while (open >= 0) open = source.indexOf("/**", scan(source, open, found))
    found.result()
  }

  /** Judges the doc opening at `open` (if it is one) and answers where scanning resumes. */
  private def scan(source: String, open: Int, found: scala.collection.mutable.Growable[MisplacedDoc]): Int = {
    val lineStart = source.lastIndexOf('\n', open) + 1
    val before    = source.substring(lineStart, open)
    // `/**/` is an empty block comment; a `/**` after `//` or inside a string is no doc.
    if (source.startsWith("/**/", open) || before.contains("//") || before.count(_ == '"') % 2 == 1) open + 3
    else {
      val close = source.indexOf("*/", open + 3)
      if (close < 0) source.length
      else {
        val after     = source.substring(close + 2)
        val sameLine  = after.takeWhile(_ != '\n').trim
        val following =
          if (sameLine.nonEmpty) Iterator(sameLine)
          else after.linesIterator.drop(1).map(_.trim).filterNot(line => line.startsWith("//") || Annotation.matches(line))
        val next   = following.nextOption()
        val reason = next match {
          case Some("")                                             => Some(BlankLine)
          case Some(line) if line.startsWith("/**")                 => Some(AnotherDoc)
          case Some(line) if Definition.findFirstIn(line).isDefined => None
          case _                                                    => Some(NotADefinition)
        }
        reason.foreach(r => found += MisplacedDoc(source.substring(0, open).count(_ == '\n') + 1, r))
        close + 2
      }
    }
  }
}
