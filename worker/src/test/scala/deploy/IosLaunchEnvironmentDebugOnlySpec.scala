package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * THE iOS APP READS ITS LAUNCH ENVIRONMENT IN DEBUG BUILDS ONLY. Every `KINOWO_*` hook a UI test sets
 * (`KINOWO_UITEST_SERVER`, `_FIXTURE`, `_PIN_ROOM_TOOLTIPS`, `KINOWO_FORCE_DETECTED_CITY`, ...) swaps
 * production behaviour for a fixture, so a release build must not even carry the names: each read of
 * `ProcessInfo.processInfo.environment` sits inside an `#if DEBUG` branch. Until 5d648ce92 the
 * RepertoireStore fixture hooks and ShowingsView's pinned tooltips were read in release builds too,
 * and no harness runs a Release build, so this source rule is the test that reaches it.
 */
class IosLaunchEnvironmentDebugOnlySpec extends AnyFlatSpec with Matchers {
  private val Read = "ProcessInfo.processInfo.environment"

  /** The lines of `source` that read the environment outside an `#if DEBUG` branch (1-based). */
  private def releaseReads(source: String): Seq[Int] = {
    // One entry per open `#if`: whether the branch being read is a DEBUG-only one.
    var stack = List.empty[Boolean]
    source.linesIterator.zipWithIndex.flatMap { case (raw, index) =>
      val line = raw.trim
      if (line.startsWith("#if ")) { stack = (line == "#if DEBUG") :: stack; None }
      else if (line.startsWith("#elseif") || line == "#else") { stack = false :: stack.drop(1); None }
      else if (line.startsWith("#endif")) { stack = stack.drop(1); None }
      else if (line.startsWith("//")) None
      else if (line.contains(Read) && !stack.contains(true)) Some(index + 1)
      else None
    }.toSeq
  }

  private def swiftSources(dir: java.io.File): Seq[java.io.File] =
    Option(dir.listFiles()).toSeq.flatten.sortBy(_.getName).flatMap { f =>
      if (f.isDirectory) swiftSources(f) else if (f.getName.endsWith(".swift")) Seq(f) else Nil
    }

  "the rule" should "tell a DEBUG branch from a release one" in {
    releaseReads(
      s"""let a = $Read["X"]
         |#if DEBUG
         |let b = $Read["Y"]
         |#else
         |let c = $Read["Z"]
         |#endif
         |// $Read in a comment
         |""".stripMargin) shouldBe Seq(1, 5)
  }

  "ios/Kinowo" should "read the launch environment only inside #if DEBUG" in {
    val sources = swiftSources(new java.io.File("ios/Kinowo"))
    sources should not be empty
    val offenders = sources.flatMap(f => releaseReads(RepoFile.read(f.getPath)).map(n => s"${f.getPath}:$n"))
    withClue("a launch-environment hook compiled into release builds; wrap it in #if DEBUG with a fixed-off #else: ") {
      offenders shouldBe empty
    }
  }
}
