package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import testsupport.ModuleSources

import java.nio.charset.StandardCharsets
import java.nio.file.Files

/**
 * Handlers that catch `Throwable` may only shrink in number.
 *
 * Scala 3.10.1's E235 ("Catching Throwable can lead to unexpected behavior") is silenced build-wide
 * (build.sbt, e07a06174): the ~100 handlers that existed then catch Throwable on purpose -- a worker loop, a
 * scrape attempt or a boot step that must outlive any failure -- and moving each to `NonFatal` is a per-site
 * decision. Silenced globally, the compiler no longer says anything about a NEW one, so this counts them
 * instead: `case _: Throwable` / `case e: Throwable` (with or without a guard) in every compiled source root,
 * test code included, since -Werror compiled those too.
 *
 * More than [[Ceiling]] fails: catch `NonFatal(e)` (or the narrower exception you mean) instead. Fewer fails
 * too, asking for the ceiling to come down with it, so the ratchet only ever tightens.
 */
class CatchThrowableLintSpec extends AnyFlatSpec with Matchers {

  /** The count on 2026-11-15, when E235 was silenced. Lower it when a site goes; never raise it. */
  private val Ceiling = 97

  private val CatchThrowable = """\bcase\s+(?:_|[a-z]\w*)\s*:\s*(?:java\.lang\.)?Throwable\b""".r
  private val CommentLine    = """^\s*(?://|\*|/\*)""".r

  private def sites(lines: Iterator[String]): Iterator[String] =
    lines.filterNot(line => CommentLine.findPrefixOf(line).isDefined)
      .map(line => line.indexOf("//") match {
        case -1 => line
        case at => if (line.lastIndexOf('"', at) >= 0) line else line.take(at)
      })
      .flatMap(line => CatchThrowable.findAllIn(line).map(_ => line.trim))

  "the Throwable-catch site matcher" should "count a handler, bound or not, guarded or not, and nothing else" in {
    sites(Iterator(
      "  catch { case _: Throwable => () }",
      "    case e: Throwable => logger.warn(e)",
      "    case t: java.lang.Throwable if retrying => retry()",
      "  final case class Failed(cause: Throwable)",
      "    case NonFatal(e) => ()",
      "    // case _: Throwable => an example in a comment",
      "   * case e: Throwable => prose in a doc comment",
      "  val f: Throwable => Unit = _ => ()"
    )).size shouldBe 3
  }

  "the build's Scala sources" should s"catch Throwable in at most $Ceiling handlers" in {
    // This file's own matcher cases are string literals the scan would count.
    val found = ModuleSources.scalaFiles().filterNot(_.endsWith("tools/CatchThrowableLintSpec.scala")).flatMap { file =>
      sites(new String(Files.readAllBytes(file), StandardCharsets.UTF_8).linesIterator)
        .map(site => s"${ModuleSources.named(file)}: $site")
    }
    withClue(s"${found.size} handlers catch Throwable (ceiling $Ceiling). Catch NonFatal(e) or the exception you " +
      "mean -- E235 is silenced in build.sbt, so only this counts them:\n" + found.mkString("\n") + "\n") {
      found.size should be <= Ceiling
    }
    withClue(s"only ${found.size} handlers catch Throwable now: lower CatchThrowableLintSpec.Ceiling to ${found.size}\n") {
      found.size shouldBe Ceiling
    }
  }
}
