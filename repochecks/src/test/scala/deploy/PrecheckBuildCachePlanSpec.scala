package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files
import scala.sys.process.*

/**
 * THE LANDING CHECK'S BUILD CACHE IS REBUILT ONLY WHEN SBT'S INPUTS MOVED (the owner, 2026-11-28). `precheck-build-cache.yml`'s
 * `plan` job decides it once, from the paths changed since the cache's last good run, and both build jobs read
 * `needs.plan.outputs.changed`. This reads the REAL filter out of the workflow and runs the real matcher
 * (`.github/actions/changed-paths/matches.sh`) over fabricated changes: a push of scripts only skips, a push of
 * `web/src/main` builds.
 */
class PrecheckBuildCachePlanSpec extends AnyFlatSpec with Matchers {

  private val Matcher  = ".github/actions/changed-paths/matches.sh"
  private val Workflow = RepoFile.read(".github/workflows/precheck-build-cache.yml")

  private lazy val filter: Vector[String] = {
    val lines = Workflow.linesIterator.toVector
    val start = lines.indexWhere(_.trim == "PATTERNS: |")
    withClue("the plan job hands no `PATTERNS: |` block to the matcher: ")(start should be >= 0)
    val indent = lines(start).takeWhile(_ == ' ').length
    lines.drop(start + 1).takeWhile(l => l.trim.isEmpty || l.takeWhile(_ == ' ').length > indent).map(_.trim).filter(_.nonEmpty)
  }

  private def runs(changed: Seq[String]): Boolean = {
    val patterns = Files.createTempFile("precheck-patterns", ".txt")
    val files    = Files.createTempFile("precheck-files", ".txt")
    try {
      Files.writeString(patterns, filter.mkString("", "\n", "\n"))
      Files.writeString(files, changed.mkString("", "\n", "\n"))
      (Seq("bash", Matcher, patterns.toString) #< files.toFile).!!.trim match {
        case "true"  => true
        case "false" => false
        case other   => fail(s"$Matcher printed `$other`")
      }
    } finally {
      Files.deleteIfExists(patterns)
      Files.deleteIfExists(files)
    }
  }

  "a change touching only scripts, docs or other workflows" should "skip the build" in {
    runs(Seq("scripts/ci/order_compare.py", "docs/tools.md", ".github/workflows/main.yml", "infra/nix/files/x.json")) shouldBe false
  }

  "a change under a module's src, the build definition or the walk's data" should "run the build" in {
    Seq(
      "web/src/main/scala/controllers/RepertoireController.scala",
      "worker/src/test/scala/services/FooSpec.scala",
      "build.sbt",
      "identity-impact.sbt",
      "project/Coupling.scala",
      "test/resources/fixtures/identity-judgements.tsv",
      ".github/actions/setup-sbt-build/action.yml"
    ).foreach(path => withClue(s"$path: ")(runs(Seq("scripts/x.py", path)) shouldBe true))
  }

  "every sbt module" should "have its sources in the filter" in {
    val modules = """project in file\("([\w-]+)"\)""".r.findAllMatchIn(RepoFile.read("build.sbt")).map(_.group(1)).toSeq
    modules should not be empty
    val missing = modules.filterNot(m => filter.contains(m + "/**") || filter.exists(_.startsWith(m + "/")))
    withClue("modules whose sources the cache compiles but the filter leaves out: ")(missing shouldBe empty)
  }

  "both build jobs" should "read the plan's decision, running unless it said `false`" in {
    val jobs = RepoFile.jobs(RepoFile.withoutComments(Workflow))
    Seq("build", "population-walk").foreach { job =>
      WorkflowText.needs(jobs(job)) should contain("plan")
      jobs(job) should include("if: ${{ !cancelled() && needs.plan.outputs.changed != 'false' }}")
    }
  }
}
