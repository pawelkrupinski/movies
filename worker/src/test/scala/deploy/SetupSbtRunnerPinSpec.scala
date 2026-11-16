package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Every `sbt/setup-sbt` call names the sbt runner the job image already carries.
 *
 * The image (`SBT_RUNNER_URL`) bakes the runner into the tool cache,
 * where setup-sbt looks first: found there, the action restores and downloads nothing. Unpinned, the action asks for
 * its own default, and when `@v1` moved that default to 2.0.11 every sbt job of every workflow missed the baked 2.0.10
 * and fetched the tarball again: 4.5 s of a `test` row's setup and 11.5 s of a convergence leg's (jobs 25777 and 25817,
 * 2026-10-26), across ~6,000 sbt jobs a week. The runner only launches the build's own sbt (project/build.properties),
 * so which runner it is changes nothing else. Bump this with the image's ARG, in the same pair of commits.
 *
 * Where the runner is NOT in the tool cache (GitHub's hosted runners), setup-sbt's own download is one `curl -sL` with no
 * retry: a DNS blip on a hosted runner failed a convergence leg on it (curl exit 6, Identity model convergence #1195,
 * 2026-11-16). So every setup-sbt step comes right after `scripts/ci/seed-sbt-runner.sh`, which puts the pinned runner
 * into the tool cache through fetch-verified.sh (retrying for minutes, checksum-verified) and names the curl exit if it
 * still fails; setup-sbt then finds it there and downloads nothing.
 *
 * And setup-sbt's own disk cache stays off: it is sbt 2's, and this build is on sbt 1, so it only ever restored and
 * saved an empty directory.
 */
class SetupSbtRunnerPinSpec extends AnyFlatSpec with Matchers {

  /** The runner the job image bakes into `$RUNNER_TOOL_CACHE/sbt/<version>`. */
  private val ImageRunner = "2.0.10"

  private val SetupSbt = """^(\s*)- uses: sbt/setup-sbt@""".r

  /** Each setup-sbt step in a file: its `with:` keys and values, comments dropped. */
  private def setupSbtSteps(text: String): Seq[Map[String, String]] = {
    val lines = text.linesIterator.toVector
    lines.indices.collect { i =>
      SetupSbt.findFirstMatchIn(lines(i)).map { m =>
        val stepIndent = m.group(1).length
        val body       = lines.drop(i + 1).takeWhile(l => l.trim.isEmpty || l.takeWhile(_ == ' ').length > stepIndent)
        body
          .map(_.takeWhile(_ != '#').trim)
          .collect { case s"$key: $value" if key != "with" && key != "if" => key -> value.trim.stripPrefix("'").stripSuffix("'") }
          .toMap
      }
    }.flatten
  }

  private val SeedStep = """run: '"$GITHUB_WORKSPACE/scripts/ci/seed-sbt-runner.sh"'"""

  /** Each setup-sbt step's own `if:` and the code lines of the step before it, comments dropped. */
  private def seedsBefore(text: String): Seq[(Option[String], Seq[String])] = {
    val lines = RepoFile.withoutComments(text).linesIterator.map(_.trim).filter(_.nonEmpty).toVector
    lines.indices.collect {
      case at if lines(at).stripPrefix("- ").startsWith("uses: sbt/setup-sbt@") =>
        val ownIf = lines.drop(at + 1).takeWhile(!_.startsWith("- ")).find(_.startsWith("if:"))
        val start = lines.lastIndexWhere(_.startsWith("- "), at - 1)
        (ownIf, lines.slice(start, at).map(_.stripPrefix("- ")))
    }
  }

  private lazy val steps: Seq[(String, Map[String, String])] =
    RepoFile.ciFiles().flatMap(path => setupSbtSteps(RepoFile.read(path)).map(path -> _))

  "every setup-sbt step" should "name the sbt runner the job image already holds" in {
    steps should not be empty
    val unpinned = steps.collect { case (path, w) if !w.get("sbt-runner-version").contains(ImageRunner) => path }
    withClue(s"pass `sbt-runner-version: $ImageRunner`, or it downloads the runner again: ") {
      unpinned shouldBe empty
    }
  }

  it should "leave setup-sbt's sbt-2 disk cache off" in {
    val cached = steps.collect { case (path, w) if !w.get("disk-cache").contains("false") => path }
    withClue("pass `disk-cache: 'false'`; this build is on sbt 1: ") {
      cached shouldBe empty
    }
  }

  it should "come right after the retrying seed step, under the same condition" in {
    val found = RepoFile.ciFiles().flatMap(path => seedsBefore(RepoFile.read(path)).map(path -> _))
    found.map(_._1).distinct.size should be >= 9
    val unseeded = found.collect {
      case (path, (ownIf, before)) if !before.contains(SeedStep) || before.find(_.startsWith("if:")) != ownIf => path
    }
    withClue(s"put a step `$SeedStep` (with setup-sbt's own `if:`) right before setup-sbt: ") {
      unseeded shouldBe empty
    }
  }

  "the seed script" should "pin the same runner, with its release's sha256" in {
    val script = RepoFile.read("scripts/ci/seed-sbt-runner.sh")
    script should include(s"version=\"$${1:-$ImageRunner}\"")
    script should include("sha256=\"${2:-6ff28e1dea38d1de48ea3d4e903f0380b85b19f945a103e30ec4298717c90cd7}\"")
  }

  "the build" should "still be on sbt 1, for which setup-sbt's disk cache holds nothing" in {
    RepoFile.read("project/build.properties") should include("sbt.version=1.")
  }
}
