package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

/**
 * Production code times itself with [[tools.Stopwatch]], not by subtracting two clock reads.
 *
 * `val t0 = System.nanoTime(); … (System.nanoTime() - t0) / 1e9` was repeated at ~30 call sites,
 * each converting units its own way, and the `System.currentTimeMillis()` ones measured on the
 * WALL clock, which an NTP step can turn negative. A clock read on the left of a subtraction is
 * that shape; a deadline (`deadline - System.nanoTime()`, `System.nanoTime() < deadline`) is not,
 * and stays allowed.
 */
class NoHandRolledTimingSpec extends AnyFlatSpec with Matchers {

  import ScalaSourceScan.{MainRoots, scalaFiles}

  private val ElapsedByHand = """(System\.nanoTime|System\.currentTimeMillis)\(\)\s*-\s*\w""".r

  "production code" should "time code with Stopwatch, not by subtracting clock reads" in {
    MainRoots.map(Paths.get(_)).foreach(root => withClue(s"$root must exist (run from the repo root)")(Files.isDirectory(root) shouldBe true))
    val offenders = for {
      file          <- scalaFiles(MainRoots)
      if !file.endsWith("tools/Stopwatch.scala")
      (line, index) <- Files.readAllLines(file).asScala.zipWithIndex
      if ElapsedByHand.findFirstIn(line).isDefined
    } yield s"$file:${index + 1}: ${line.trim}"
    withClue("time with tools.Stopwatch (start()/timed/total()) instead:\n" + offenders.mkString("\n") + "\n")(offenders shouldBe empty)
  }
}
