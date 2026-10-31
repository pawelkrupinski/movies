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

  "the build" should "still be on sbt 1, for which setup-sbt's disk cache holds nothing" in {
    RepoFile.read("project/build.properties") should include("sbt.version=1.")
  }
}
