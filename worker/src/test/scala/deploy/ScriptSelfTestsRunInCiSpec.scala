package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/**
 * The repository's script self-tests guard logic no JVM layer reaches: the pre-push hook's selection,
 * the CI helpers, the identity and release tooling, the roster generators. A self-test no workflow runs
 * guards nothing: the roster generators' and the convergence findings parser's sat unrun until this spec.
 *
 * Under the script roots a test is found by its NAME — `<tool>-test.sh` or `<tool>_test.py` — by
 * scripts/run-script-tests, which ci.yml runs whole: so each such file must be one the runner lists, and
 * no test may hide under another scheme (`test_*.py`, `*-spec.sh`) it would miss. A test tagged
 * `# script-test: <tag> <why>` leaves the default run and says why; unless `manual` (it needs what no CI
 * job has, or tests nothing), some workflow runs it by name where its toolchain is. The `data/` roster
 * generators' `test_*.py` are named one by one by the workflows that build the rosters.
 */
class ScriptSelfTestsRunInCiSpec extends AnyFlatSpec with Matchers {

  private val RunnerRoots = Seq("scripts", ".github/scripts", ".github/actions", "android/scripts", "ios/scripts", "infra/bin")
  private val SelfTest = """([\w-]+_test\.py|[\w-]+-test\.sh)""".r
  private val OffScheme = """(test_[\w-]+\.py|[\w-]+_spec\.py|[\w-]+-spec\.sh)""".r
  /** The assertion harness the shell specs source: named like a spec, it tests nothing itself. */
  private val Harness = Set("scripts/shell-spec.sh")
  private val Pruned = Set("node_modules", "build", ".build", "target")

  private def files(root: String): Seq[Path] = {
    val stream = Files.walk(Paths.get(root))
    try stream.iterator.asScala.filter(Files.isRegularFile(_))
      .filterNot(path => path.iterator.asScala.exists(part => Pruned(part.toString))).toSeq
    finally stream.close()
  }

  private def named(scheme: scala.util.matching.Regex, roots: Seq[String]): Seq[String] =
    roots.filter(root => Files.isDirectory(Paths.get(root))).flatMap(files)
      .filter(path => scheme.matches(path.getFileName.toString)).map(_.toString).sorted

  private lazy val runs: Seq[RepoFile.RunStep] =
    (files(".github/workflows") ++ files(".github/actions")).filter(_.toString.matches(""".*\.ya?ml"""))
      .flatMap(path => RepoFile.runSteps(RepoFile.read(path.toString)))

  // Run by a `run:` step — a comment, a `paths:` trigger or a step name runs nothing — that names its path,
  // or its file name from a `working-directory` of the test's own directory. Never by file name alone:
  // Spain's and the US's roster generators both have a `test_generate_roster.py`.
  private def runByName(path: String): Boolean = {
    val file = Paths.get(path)
    runs.exists(r => r.script.contains(path) ||
      (r.workingDirectory.contains(file.getParent.toString) && r.script.contains(file.getFileName.toString)))
  }

  "the script-test runner" should "list every self-test under its roots" in {
    val onDisk = named(SelfTest, RunnerRoots)
    onDisk should not be empty
    ScriptTests.all.map(_.path) should contain theSameElementsAs onDisk
  }

  "a script self-test" should "be named <tool>-test.sh or <tool>_test.py, so the runner finds it" in {
    named(OffScheme, RunnerRoots).filterNot(Harness) shouldBe empty
  }

  "ci.yml" should "run every untagged self-test through the runner" in {
    ScriptTests.ciRunsAll shouldBe true
  }

  "a tagged self-test" should "say why it leaves the default run" in {
    ScriptTests.all.filter(test => test.tag.nonEmpty && test.reason.trim.isEmpty).map(_.path) shouldBe empty
  }

  it should "be run by name by some workflow, unless it is manual" in {
    ScriptTests.all.filter(_.tag.exists(_ != "manual")).map(_.path).filterNot(runByName) shouldBe empty
  }

  "every data self-test" should "be run by some workflow's `run:` step" in {
    val dataTests = named("""(test_[\w-]+\.py|[\w-]+_test\.py|[\w-]+-test\.sh)""".r, Seq("data"))
    dataTests should not be empty
    dataTests.filterNot(runByName) shouldBe empty
  }
}
