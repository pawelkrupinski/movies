package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/**
 * The repository's script self-tests — `test_*.py`, `*_test.py`, `*-test.sh` beside the CI
 * helpers, the roster generators and the convergence tooling — guard logic no JVM layer reaches:
 * the generated rosters' builders, the parser that turns a red leg's log into hard clusters. A
 * self-test no workflow runs guards nothing: the roster generators' and the convergence findings
 * parser's sat unrun until this spec. Each must be named by some workflow or composite action.
 *
 * (`infra/` is the fleet's own repository-in-a-directory, checked by `infra/bin/check`, and
 * `android/scripts/firebase-test.sh` submits a device test rather than testing a script.)
 */
class ScriptSelfTestsRunInCiSpec extends AnyFlatSpec with Matchers {

  private val Roots = Seq("scripts", ".github", "data")
  private val SelfTest = """(test_[\w-]+\.py|[\w-]+_test\.py|[\w-]+-test\.sh)""".r

  private def files(root: String): Seq[Path] = {
    val stream = Files.walk(Paths.get(root))
    try stream.iterator.asScala.filter(Files.isRegularFile(_)).toSeq finally stream.close()
  }

  private lazy val selfTests: Seq[String] =
    Roots.flatMap(files).filter(path => SelfTest.matches(path.getFileName.toString)).map(_.toString).sorted

  private lazy val runs: Seq[RepoFile.RunStep] =
    (files(".github/workflows") ++ files(".github/actions")).filter(_.toString.matches(""".*\.ya?ml"""))
      .flatMap(path => RepoFile.runSteps(RepoFile.read(path.toString)))

  "every script self-test" should "be run by some workflow's `run:` step" in {
    selfTests should not be empty
    // Run by a `run:` step — a comment, a `paths:` trigger or a step name runs nothing — that names
    // its path, or its file name from a `working-directory` of the test's own directory. Never by
    // file name alone: Spain's and the US's roster generators both have a `test_generate_roster.py`.
    def run(path: String): Boolean = {
      val file = Paths.get(path)
      runs.exists(r => r.script.contains(path) ||
        (r.workingDirectory.contains(file.getParent.toString) && r.script.contains(file.getFileName.toString)))
    }
    selfTests.filterNot(run) shouldBe empty
  }
}
