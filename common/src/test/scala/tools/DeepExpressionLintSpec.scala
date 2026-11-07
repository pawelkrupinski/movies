package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import testsupport.ModuleSources

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * The compiler walks a chain of binary operators recursively, a few frames per link, so a long
 * enough chain overflows the stack of whichever thread compiles it. `LegacySlugOwners` once held
 * its 3,152 slugs as `"…" +` lines, 369 of them in a row: it compiled on a Mac and failed every
 * CI job with `java.lang.StackOverflowError` in `PostTyper` (Main run 38382160891). A Seq's
 * elements or a multi-line string are flat; a chain is not.
 *
 * Every compiled source root counts — test, it, page and fixtures code is compiled by the same
 * compiler on the same CI runners — and every shape of chain a line break can hide: `+` and `++`,
 * trailing a literal, an identifier or a closing bracket, or leading the next line.
 */
class DeepExpressionLintSpec extends AnyFlatSpec with Matchers {

  /** Well past any hand-written chain (13 is the longest today), well short of the 369 that
   *  overflowed. */
  private val MaxChain = 30

  /** A line continued by a trailing `+`/`++` after a literal, identifier or closing bracket —
   *  not a comment line, whose `+` is prose. */
  private val Trailing = """^(?!\s*(//|\*|/\*)).*["\w)\]}`]\s*\+\+?\s*$""".r
  /** A line continuing the one above with a leading `+`/`++`. */
  private val Leading = """^\s*\+\+?\s+\S.*$""".r

  /** A line is a link when an operator joins it to the next one — trailing it, or leading that
   *  next line — so `"a" +` / `"b"` and `"a"` / `+ "b"` are each one link, and a chain mixing
   *  the two styles is counted whole. */
  private def longestChain(lines: Seq[String]): Int =
    val indexed = lines.toIndexedSeq
    indexed.indices.foldLeft((0, 0)) { case ((best, run), i) =>
      val linked = Trailing.matches(indexed(i)) || indexed.lift(i + 1).exists(Leading.matches)
      val next = if (linked) run + 1 else 0
      (best max next, next)
    }._1

  /** Every `<module>/src/<config>/scala` directory under the repository root. */
  private def sourceRoots: Seq[Path] = ModuleSources.configDirectories().map(_.resolve("scala"))

  "Every compiled source root" should "chain no more concatenations in a row than the compiler can walk" in {
    val roots = sourceRoots
    roots.map(ModuleSources.named) should contain allOf (
      "common/src/main/scala", "common/src/test/scala", "web/src/main/scala", "web/src/test/scala", "web/src/it/scala",
      "web/src/page/scala", "identity/src/main/scala", "identity/src/test/scala", "scrapers/src/main/scala", "scrapers/src/test/scala", "scrapers/src/fixtures/scala", "worker/src/main/scala", "worker/src/test/scala", "worker/src/it/scala",
      "worker/src/fixtures/scala")
    val sources = roots.flatMap(root => Files.walk(root).iterator.asScala.filter(_.toString.endsWith(".scala")).toSeq)
    val deep = sources.map(p => p -> longestChain(Files.readAllLines(p).asScala.toSeq)).filter(_._2 > MaxChain)
    deep.map { case (p: Path, n) => s"$p: $n" } shouldBe empty
  }

  "the lint" should "catch the chain that overflowed" in {
    longestChain(Seq.fill(MaxChain + 1)("  \"a b \" +") :+ "  \"c\"") shouldBe MaxChain + 1
    longestChain(Seq("  \"a\" +", "  \"b\"", "  \"c\" +", "  \"d\"")) shouldBe 1
  }

  it should "catch a chain of trailing ++" in {
    longestChain(Seq("  Seq(a) ++", "  others ++", "  more.toSeq ++", "  Nil")) shouldBe 3
  }

  it should "catch a chain of identifiers and calls joined by a trailing +" in {
    longestChain(Seq("  prefix +", "  name +", "  suffix(x) +", "  rest")) shouldBe 3
  }

  it should "catch a chain of leading + and ++" in {
    longestChain(Seq("  \"a\"", "    + \"b\"", "    + name", "    ++ more", "  )")) shouldBe 3
  }

  it should "count a chain that mixes trailing and leading operators as one" in {
    longestChain(Seq("  \"a\" +", "  \"b\"", "    + \"c\" +", "  \"d\"", "  done")) shouldBe 3
  }

  it should "not count a comment's +, an increment, or a line that merely mentions an operator" in {
    longestChain(Seq("  // one +", "   * two +", "  count += 1", "  val plus = \"+\"", "  x + y")) shouldBe 0
  }
}
