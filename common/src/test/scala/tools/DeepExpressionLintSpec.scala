package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import testsupport.RepoRoot

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/**
 * The compiler walks a chain of binary operators recursively, a few frames per link, so a long
 * enough chain overflows the stack of whichever thread compiles it. `LegacySlugOwners` once held
 * its 3,152 slugs as `"…" +` lines, 369 of them in a row: it compiled on a Mac and failed every
 * CI job with `java.lang.StackOverflowError` in `PostTyper` (Main run 38382160891). A Seq's
 * elements or a multi-line string are flat; a chain is not.
 */
class DeepExpressionLintSpec extends AnyFlatSpec with Matchers {

  /** Well past any hand-written chain, well short of the 369 that overflowed. */
  private val MaxChain = 40

  private val Continued = """^.*"\s*\+\s*$""".r

  private def longestChain(lines: Seq[String]): Int =
    lines.foldLeft((0, 0)) { case ((best, run), line) =>
      val next = if (Continued.matches(line)) run + 1 else 0
      (best max next, next)
    }._1

  "Main sources" should "chain no more string concatenations in a row than the compiler can walk" in {
    val sources = Seq("common", "web", "worker").map(m => RepoRoot.file(s"$m/src/main/scala").toPath)
      .flatMap(root => Files.walk(root).iterator.asScala.filter(_.toString.endsWith(".scala")).toSeq)
    sources should not be empty
    val deep = sources.map(p => p -> longestChain(Files.readAllLines(p).asScala.toSeq)).filter(_._2 > MaxChain)
    deep.map { case (p: Path, n) => s"$p: $n" } shouldBe empty
  }

  "the lint" should "catch the chain that overflowed" in {
    longestChain(Seq.fill(MaxChain + 1)("  \"a b \" +") :+ "  \"c\"") shouldBe MaxChain + 1
    longestChain(Seq("  \"a\" +", "  \"b\"", "  \"c\" +", "  \"d\"")) shouldBe 1
  }
}
