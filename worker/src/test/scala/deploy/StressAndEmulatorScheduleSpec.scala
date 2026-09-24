package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

/**
 * WHERE the luck-dependent suites run: the race harnesses and the WebKit phones nightly, many
 * times over (nightly-stress.yml); the Android emulator lane only for app changes, with its
 * nightly coverage in order-independence.yml.
 *
 * The harness LIST lives in scripts/ci/stress-races.sh, which refuses to run a class that no
 * longer exists — but only at 01:30. This checks the same thing on every push, so a rename that
 * drops a harness from the nightly run fails the push that made it.
 */
class StressAndEmulatorScheduleSpec extends AnyFlatSpec with Matchers {
  private lazy val stress   = RepoFile.read(".github/workflows/nightly-stress.yml")
  private lazy val android  = RepoFile.read(".github/workflows/android.yml")
  private lazy val nightlyOrder = RepoFile.read(".github/workflows/order-independence.yml")
  private lazy val script   = RepoFile.read("scripts/ci/stress-races.sh")

  /** ("worker/Test", "tools.BoundedParallelSpec") for every harness line of the script. */
  private lazy val harnesses: Seq[(String, String)] =
    """(?m)^\s+"(\w+/(?:Test|IntegrationTest)) ([\w.]+)"\s*$""".r.findAllMatchIn(script)
      .map(m => m.group(1) -> m.group(2)).toSeq

  "stress-races.sh" should "list harnesses, or the nightly run stresses nothing" in {
    harnesses.map(_._1).toSet should contain allOf ("worker/Test", "worker/IntegrationTest")
  }

  it should "list only classes that exist where it says" in {
    val missing = harnesses.filterNot { case (target, cls) =>
      val module = target.takeWhile(_ != '/')
      val dir    = Paths.get(module, "src", if (target.endsWith("IntegrationTest")) "it" else "test")
      val simple = cls.split('.').last
      Files.walk(dir).iterator().asScala.filter(_.toString.endsWith(".scala"))
        .exists(p => Files.readString(p).linesIterator.exists(_.matches(s""".*\\bclass $simple\\b.*""")))
    }
    withClue("harnesses stress-races.sh names that are gone: ")(missing shouldBe empty)
  }

  "nightly-stress.yml" should "run the race harnesses 50 times on its schedule" in {
    stress should include("cron:")
    stress should include("scripts/ci/stress-races.sh \"$ITERATIONS\" all")
    stress should include("inputs.iterations || '50'")
  }

  it should "run the WebKit phone projects with --repeat-each 5" in {
    stress should include("extra-args: --repeat-each 5")
    stress should include("webkit-iphone-se*,webkit-iphone-13*,webkit-iphone-17-pro-max*")
  }

  // The workflow's lane cancels a run in progress for a newer push: a push touching only
  // scripts/ cancelling one that touched android/** would, diffed from its own `before`,
  // never run the emulator for that app change. The base is the last run that FINISHED.
  "android.yml" should "gate the emulator lane on what changed since the last successful run, not this push's before" in {
    val preflight = RepoFile.block(RepoFile.block(android, "jobs"), "preflight")
    val gate      = RepoFile.step(preflight, "Did this push touch the app?")
    gate should include("base: ${{ steps.base.outputs.sha }}")
    val base = RepoFile.stepScript(preflight, "The last successful run's commit, for the emulator filter")
    base should include("--status success")
    base should include("--branch main")
    RepoFile.block(preflight, "permissions") should include("actions: read")
  }

  it should "run the emulator lane only when the app or its own workflow changed" in {
    val instrumented = RepoFile.block(RepoFile.block(android, "jobs"), "instrumented")
    instrumented should include("if: needs.preflight.outputs.emulator == 'true'")
    val preflight = RepoFile.block(RepoFile.block(android, "jobs"), "preflight")
    preflight should include("emulator: ${{ steps.android.outputs.changed }}")
    val lines    = preflight.linesIterator.toVector
    val at       = lines.indexWhere(_.trim == "patterns: |")
    val indent   = lines(at).takeWhile(_ == ' ').length
    val patterns = lines.drop(at + 1).takeWhile(l => l.takeWhile(_ == ' ').length > indent).map(_.trim)
    patterns shouldBe Seq("android/**", ".github/workflows/android.yml")
  }

  "order-independence.yml" should "still run every androidTest class on the emulator nightly" in {
    nightlyOrder should include("android-instrumented:")
    nightlyOrder should include("scripts/devtest-shuffled.sh")
  }
}
