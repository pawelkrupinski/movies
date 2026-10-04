package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

/**
 * Pins the compiler rule that makes a read outcome impossible to drop unread: `build.sbt` turns
 * on `-Wnonunit-statement` and `-Wvalue-discard` (an outcome returned where Unit is expected) and silences them for every type except the outcome types it names, so
 * (under `-Werror`) a statement that computes a [[ScanOutcome]] or a [[ReadOutcome]] and throws
 * it away does not compile. The rule found, the day it went in, four readers that dropped an
 * incomplete scan and answered with part of a collection as the whole.
 *
 * The filter is a regex in a build file, so this spec reads it back and runs it against the
 * compiler's own message shapes: a mistyped name or an escape gone wrong would otherwise switch
 * the rule off without a word.
 */
class OutcomesCannotBeDroppedSpec extends AnyFlatSpec with Matchers {

  private lazy val build = new String(Files.readAllBytes(Paths.get("build.sbt")), StandardCharsets.UTF_8)

  /** The `-Wconf` filter that silences every unused value but the guarded types, unescaped. */
  private lazy val silencer: scala.util.matching.Regex = {
    val Line = """"-Wconf:msg=(.*unused value.*):silent"""".r
    val escaped = build.linesIterator.map(_.trim.stripSuffix(",")).collectFirst { case Line(pattern) => pattern }
      .getOrElse(fail("build.sbt has no -Wconf filter silencing unused values"))
    escaped.replace("\\\\", "\\").r
  }

  private def silenced(message: String): Boolean = silencer.findFirstIn(message).isDefined

  /** The types the build guards: each must still exist, or its entry is stale. */
  private val Guarded = Seq("tools.ScanOutcome", "tools.ReadOutcome", "tools.GuardedWrite")

  "build.sbt" should "turn the unused-value warning on" in {
    build should include("\"-Wnonunit-statement\"")
  }

  it should "turn the discarded-value warning on — an outcome landing in a Unit-typed position" in {
    build should include("\"-Wvalue-discard\"")
  }

  it should "keep a discarded outcome an error, in both of the compiler's spellings" in {
    Guarded.foreach { name =>
      silenced(s"discarded non-Unit value of type $name. Add `: Unit` to discard silently.") shouldBe false
      silenced(s"Discarded non-Unit value of type Option[$name]. Add `: Unit` to discard silently.") shouldBe false
    }
    silenced("discarded non-Unit value of type Int. Add `: Unit` to discard silently.") shouldBe true
    silenced("Discarded non-Unit value of type Int. Add `: Unit` to discard silently.") shouldBe true
  }

  it should "keep an unused outcome an error, bare or wrapped" in {
    Guarded.foreach { name =>
      silenced(s"unused value of type $name") shouldBe false
      silenced(s"unused value of type scala.util.Try[$name | Unit]") shouldBe false
      silenced(s"unused value of type Option[$name[Seq[String]]]") shouldBe false
    }
  }

  it should "leave every other unused value alone" in {
    silenced("unused value of type Long") shouldBe true
    silenced("unused value of type scala.util.Try[Unit]") shouldBe true
    silenced("unused value of type services.MongoIndex.Outcome") shouldBe true
    // A name that merely contains a guarded one is not it.
    silenced("unused value of type services.WriteOutcomeOfScanOutcomes") shouldBe true
  }

  "Every guarded type" should "exist" in {
    Guarded.foreach(name => noException should be thrownBy Class.forName(name))
    Guarded.foreach(name => build should include(name.stripPrefix("tools.")))
  }
}
