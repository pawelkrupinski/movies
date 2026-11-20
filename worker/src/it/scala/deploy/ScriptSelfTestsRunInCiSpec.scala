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
 * scripts/run-script-tests, which tool-self-tests.yml runs whole (off the gate; ci.yml did until 2026-10-30): so each such file must be one the runner lists, and
 * no test may hide under another scheme (`test_*.py`, `*-spec.sh`) it would miss. A test tagged
 * `# script-test: <tag> <why>` leaves the default run and says why; unless `manual` (it needs what no CI
 * job has, or tests nothing), some workflow runs it by name where its toolchain is. The `data/` roster
 * generators' tests are under the same scheme since 2026-10-15: they were `test_*.py`, named one by one by a
 * ci.yml step that a new one could miss.
 */
class ScriptSelfTestsRunInCiSpec extends AnyFlatSpec with Matchers {

  private val RunnerRoots = Seq("scripts", ".github/scripts", ".github/actions", "android/scripts", "ios/scripts", "infra/bin", "infra/nix/files", "data")
  private val SelfTest = """([\w-]+_test\.py|[\w-]+-test\.sh)""".r
  private val OffScheme = """(test_[\w-]+\.py|[\w-]+_spec\.py|[\w-]+-spec\.sh)""".r
  /** The assertion harness the shell specs source: named like a spec, it tests nothing itself. */
  private val Harness = Set("scripts/shell-spec.sh")
  private val Pruned = Set("node_modules", "build", ".build", "target")
  /** The nightly macOS step's floor run: the bash scripts/floor-bash finds, macOS's /bin/bash 3.2 there. */
  private val FloorRun = """scripts/run-script-tests --bash "$(scripts/floor-bash)""""

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
  // Spain's and the US's roster generators both have a `generate_roster_test.py`.
  private def runByName(path: String): Boolean = {
    val file = Paths.get(path)
    runs.exists(r => r.script.contains(path) ||
      (r.workingDirectory.contains(file.getParent.toString) && r.script.contains(file.getFileName.toString)))
  }

  private val Provide = "GNU timeout, shellcheck and rsync 3, as Linux CI and the laptop have them"

  "the script-test runner" should "list every self-test under its roots" in {
    val onDisk = named(SelfTest, RunnerRoots)
    onDisk should not be empty
    ScriptTests.all.map(_.path) should contain theSameElementsAs onDisk
  }

  "a script self-test" should "be named <tool>-test.sh or <tool>_test.py, so the runner finds it" in {
    named(OffScheme, RunnerRoots).filterNot(Harness) shouldBe empty
  }

  "tool-self-tests.yml" should "run every untagged self-test through the runner" in {
    ScriptTests.ciRunsAll shouldBe true
  }

  "the nightly macOS job" should "run every self-test under the floor bash 3.2 (macOS's /bin/bash), the floor the tools promise" in {
    // ci.yml's Linux bash is 5: a bash-4-only construct passes there and breaks every Mac
    RepoFile.runSteps(RepoFile.read(".github/workflows/order-independence.yml"))
      .exists(_.script.linesIterator.exists(_.trim == FloorRun)) shouldBe true
  }

  it should "first give them GNU timeout, shellcheck and rsync 3, which the tools run with on Linux CI and the laptop" in {
    // The macOS image has none of them: ten self-tests failed there "timeout: command not found" and "shellcheck is
    // not installed" (Order independence #28, run 41294275183), and backtest_alerts_local_test.py five times "rsync on
    // PATH is not rsync 3" (#89, run 44664123050: the image's rsync is openrsync), though all passed on ubuntu and
    // the laptop. rsync is linked beside timeout, ahead of /usr/bin on PATH.
    val swift = RepoFile.block(RepoFile.block(RepoFile.read(".github/workflows/order-independence.yml"), "jobs"), "swift")
    val provide = RepoFile.stepScript(swift, Provide)
    provide should include("brew install coreutils shellcheck rsync")
    provide should include("gtimeout")
    provide should include("""ln -sf "$(brew --prefix rsync)/bin/rsync" "$RUNNER_TEMP/gnu-timeout/rsync"""")
    provide should include("$GITHUB_PATH")
    RepoFile.positionOf(swift, Provide) should be < RepoFile.positionOf(swift, FloorRun)
  }

  "a tagged self-test" should "say why it leaves the default run" in {
    ScriptTests.all.filter(test => test.tag.nonEmpty && test.reason.trim.isEmpty).map(_.path) shouldBe empty
  }

  it should "be run by name by some workflow, unless it is manual" in {
    ScriptTests.all.filter(_.tag.exists(_ != "manual")).map(_.path).filterNot(runByName) shouldBe empty
  }
}
