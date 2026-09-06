package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.sys.process.{Process, ProcessLogger}

/**
 * Coverage is measured, and it is measured OFF the deploy path.
 *
 * sbt-scoverage sat configured in build.sbt with nothing running it, so the
 * ~5,700 unit cases had a coverage number nobody had ever seen. The obvious fix
 * — one more job in ci.yml — is the wrong one twice over: ci.yml is at the
 * account's 20-runner budget (CiRunnerBudgetSpec), and main.yml's deploy
 * `needs` the WHOLE of ci.yml, so an instrumented run there holds every deploy
 * for the ~2× it takes even with `continue-on-error`. coverage.yml therefore
 * runs when `Main` has COMPLETED, deploy included, and the runners are idle.
 *
 * This spec locks that shape — the trigger, the cache isolation, the module
 * set — and RUNS the summary step's shell against a fabricated report, since
 * that shell is the only place the percentages are read and a sed that parsed
 * nothing would print a blank cell forever.
 */
class CoverageWorkflowSpec extends AnyFlatSpec with Matchers {

  private lazy val coverageYml = RepoFile.read(".github/workflows/coverage.yml")
  private lazy val ciYml       = RepoFile.read(".github/workflows/ci.yml")
  private lazy val mainYml     = RepoFile.read(".github/workflows/main.yml")

  private lazy val triggers = RepoFile.block(coverageYml, "on")
  private lazy val job      = RepoFile.block(coverageYml, "coverage")

  private def mainWorkflowName: String =
    mainYml.linesIterator.map(_.trim).collectFirst { case s"name: $name" => name.trim }
      .getOrElse(fail("main.yml has no `name:`"))

  "the coverage workflow" should "start only after Main has completed, or by hand — never at t=0 with the gating jobs" in {
    triggers should include("workflow_run:")
    triggers should include(s"workflows: [$mainWorkflowName]")
    triggers should include("types: [completed]")
    triggers should include("workflow_dispatch:")
    withClue("a push/PR trigger is a 21st concurrent job — see CiRunnerBudgetSpec: ") {
      triggers should not include "push:"
      triggers should not include "pull_request:"
    }
  }

  it should "not be a job in either deploy-gating workflow" in {
    Seq(ciYml, mainYml).foreach { yml =>
      RepoFile.block(yml, "jobs").linesIterator.map(_.trim).toVector should not contain "coverage:"
    }
  }

  it should "supersede an in-flight run rather than queue behind it" in {
    val concurrency = RepoFile.block(coverageYml, "concurrency")
    concurrency should include("cancel-in-progress: true")
  }

  /**
   * Every instrumented JVM appends its own `scoverage.measurements.<run-id>`
   * under `scoverage-data`, and the report sums them all. The directory lives
   * inside the cached `*\/target/scala-*`, so a warm runner that did not wipe it
   * would report the previous run's hits on top of this one's.
   *
   * But ONLY the measurements may go. The same directory holds the compile-time
   * `scoverage.coverage` (the statement table the report is keyed on), and the
   * instrumented classes restored from the cache write straight into it — the
   * runtime never creates it. Whenever zinc recompiles a module the compiler
   * recreates the directory and drops the stale measurements itself; on an
   * exact cache hit it compiles NOTHING, so a step that `rm -rf`'d the whole
   * directory left every restored module writing into a path that no longer
   * existed: 134 specs red on `FileNotFoundException: …/common/target/
   * scala-3.9.0/scoverage-data/scoverage.measurements…` (run 34058606835, an
   * infra-only commit), then `coverageAggregate` "succeeded" with no report.
   */
  it should "wipe the previous run's measurements before it measures — and nothing else in scoverage-data" in {
    val steps = job.linesIterator.map(_.trim).filter(_.startsWith("- name: ")).toVector
    steps.indexOf("- name: Drop measurements from any earlier run") should be < steps.indexOf("- name: Run the unit suites instrumented")

    val root       = Files.createTempDirectory("coverage-wipe")
    val moduleData = root.resolve("common/target/scala-3.9.0/scoverage-data")
    val rootData   = root.resolve("target/scala-3.9.0/scoverage-data")
    write(moduleData.resolve("scoverage.coverage"), "# Coverage data, format version: 3.0\n")
    write(moduleData.resolve("scoverage.measurements.a1b2.17"), "1\n2\n")
    write(moduleData.resolve("scoverage.measurements.a1b2.18"), "3\n")
    write(rootData.resolve("scoverage.measurements.c3d4.1"), "9\n")
    // A module the run never instruments (testkit, e2e) has no scoverage-data at
    // all; the step must not trip over the unmatched glob.
    Files.createDirectories(root.resolve("testkit/target/scala-3.9.0/classes"))
    val script = root.resolve("wipe.sh")
    Files.writeString(script, RepoFile.stepScript(coverageYml, "Drop measurements from any earlier run"))

    Process(Seq("bash", "-eo", "pipefail", script.toString), root.toFile).! shouldBe 0

    def names(dir: Path): Set[String] =
      Option(dir.toFile.list()).map(_.toSet).getOrElse(fail(s"$dir was deleted, not emptied of measurements"))
    names(moduleData) shouldBe Set("scoverage.coverage")
    names(rootData) shouldBe Set.empty
  }

  it should "instrument the same suites ci.yml's test job runs, in a step that cannot fail the report" in {
    val ciTest = RepoFile.block(ciYml, "test")
    ciTest should include("- run: sbt testUnitNoE2e")
    val instrumented = RepoFile.step(job, "Run the unit suites instrumented")
    instrumented should include("run: sbt coverage testUnitNoE2e")
    instrumented should include("continue-on-error: true")
  }

  it should "aggregate in a separate sbt invocation, so a red spec still yields a report" in {
    val aggregate = RepoFile.step(job, "Aggregate the per-module reports")
    aggregate should include("run: sbt coverageAggregate")
    aggregate should not include "continue-on-error"
  }

  it should "publish the aggregate and per-module HTML as one artifact" in {
    job should include("name: coverage-report")
    job should include("target/scala-*/scoverage-report")
    job should include("*/target/scala-*/scoverage-report")
    job should include("if-no-files-found: error")
  }

  it should "have a ceiling on a hung instrumented run" in {
    job should include("timeout-minutes:")
  }

  /**
   * Instrumented classes restored into the gating jobs' zinc cache would cost
   * every one of them a full recompile — and the reverse would cost this job the
   * same. The keys must not resolve into each other's caches in either direction.
   */
  it should "keep its zinc cache apart from the deploy-gating jobs' cache" in {
    def cacheKeys(yml: String): (String, String) = {
      val lines   = yml.linesIterator.map(_.trim).toVector
      val key     = lines.collectFirst { case s"key: $k" => k }.getOrElse(fail("no cache key"))
      val restore = lines.dropWhile(_ != "restore-keys: |").drop(1).headOption.getOrElse(fail("no restore-keys"))
      (key, restore)
    }
    val (coverageKey, coverageRestore) = cacheKeys(job)
    val (ciKey, ciRestore)             = cacheKeys(RepoFile.block(ciYml, "test"))
    withClue(s"coverage key $coverageKey would restore from ci.yml's $ciRestore: ") {
      coverageKey should not startWith ciRestore
    }
    withClue(s"ci.yml key $ciKey would restore from coverage's $coverageRestore: ") {
      ciKey should not startWith coverageRestore
    }
  }

  /** scoverage.xml's root element as sbt-scoverage 2.4 actually writes it — the
   *  attributes on the line AFTER the tag, which is what a one-line-at-a-time
   *  sed has to cope with. Copied from a real report, not guessed. */
  private def scoverageXml(statementRate: String, branchRate: String): String =
    s"""<?xml version="1.0" ?>
       |<scoverage 
       |statement-count="200" statements-invoked="100" statement-rate="$statementRate" branch-rate="$branchRate" version="1.0" timestamp="1757178000000">
       |    <packages>
       |    </packages>
       |</scoverage>
       |""".stripMargin

  private def write(path: Path, content: String): Unit = {
    Files.createDirectories(path.getParent)
    Files.writeString(path, content)
  }

  it should "read the statement and branch rates out of the reports it just wrote" in {
    val root = Files.createTempDirectory("coverage-summary")
    write(root.resolve("target/scala-3.9.0/scoverage-report/scoverage.xml"), scoverageXml("41.20", "30.50"))
    write(root.resolve("common/target/scala-3.9.0/scoverage-report/scoverage.xml"), scoverageXml("55.00", "40.00"))
    write(root.resolve("worker/target/scala-3.9.0/scoverage-report/scoverage.xml"), scoverageXml("38.12", "27.90"))
    val summary = root.resolve("summary.md")
    val script  = root.resolve("summary.sh")
    Files.writeString(script, RepoFile.stepScript(coverageYml, "Coverage summary"))

    val rc = Process(Seq("bash", script.toString), root.toFile, "GITHUB_STEP_SUMMARY" -> summary.toString).!
    rc shouldBe 0

    val written = Files.readString(summary)
    written should include("| **aggregate** | **41.20%** | **30.50%** |")
    written should include("| common | 55.00% | 40.00% |")
    written should include("| worker | 38.12% | 27.90% |")
  }

  /**
   * `coverageAggregate` exits 0 and writes nothing when it finds no
   * scoverage-data — which is how run 34058606835 reached the summary at all —
   * and the summary's bare `ls` then died with `ls: cannot access …` at exit 2.
   * This step is the only place that notices, so it has to say what is missing
   * and which step to look at, not leave the reader to decode an ls error.
   */
  it should "fail with a clear message, not a bare ls error, when the aggregate report is missing" in {
    val root    = Files.createTempDirectory("coverage-summary-missing")
    val summary = root.resolve("summary.md")
    val script  = root.resolve("summary.sh")
    Files.writeString(script, RepoFile.stepScript(coverageYml, "Coverage summary"))

    val output = new StringBuilder
    val logger = ProcessLogger(line => output.append(line).append('\n'))
    val rc     = Process(Seq("bash", script.toString), root.toFile, "GITHUB_STEP_SUMMARY" -> summary.toString).!(logger)

    rc should not be 0
    output.toString should include("::error::")
    output.toString should include("scoverage-report/scoverage.xml")
    output.toString should include("coverageAggregate")
    output.toString should not include "cannot access"
  }
}
