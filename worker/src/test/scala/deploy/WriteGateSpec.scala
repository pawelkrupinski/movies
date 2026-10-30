package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * SHADOW ON GITHUB.COM (github-return-plan 2026-10-30, 3.1): while Forgejo is primary, every workflow also runs on
 * github.com's hosted runners and must write NOTHING outside its runner there -- no deploy, image push, staged pin,
 * release or release asset, bot branch, PR or issue, metric push or Grafana marker -- unless the repository variable
 * CI_ROLE is `primary`. Unset counts as shadow. On Forgejo every gate is true, so nothing there changes.
 *
 * One expression gates jobs and steps; scripts/ci/ci-writes.sh is the same rule for writes inside scripts and
 * composite actions, which read CI_ROLE from the workflow-level env every workflow carries.
 */
class WriteGateSpec extends AnyFlatSpec with Matchers {
  private val Gate    = "(github.server_url != 'https://github.com' || vars.CI_ROLE == 'primary')"
  private val Primary = "vars.CI_ROLE == 'primary'"

  private lazy val workflows = RepoFile.listed(".github/workflows")(_.getName.endsWith(".yml"))
    .map(f => f.getName -> RepoFile.read(s".github/workflows/${f.getName}"))

  private def job(file: String, name: String): String =
    RepoFile.jobs(RepoFile.read(s".github/workflows/$file")).getOrElse(name, fail(s"$file has no job $name"))

  private def jobIf(text: String): String =
    text.linesIterator.drop(1).find(_.matches("""^\s+if:.*""")).map(_.trim).getOrElse("")

  "every workflow" should "hand its jobs CI_ROLE, so the scripts' gate can tell primary from shadow" in {
    workflows.filterNot { case (_, yml) =>
      RepoFile.block(yml, "env").linesIterator.exists(_.trim.startsWith("CI_ROLE: ${{ vars.CI_ROLE }}"))
    }.map(_._1) shouldBe empty
  }

  "Main's publishes" should "only run where writes are allowed: a registry tag Flux follows is a deploy" in {
    jobIf(job("main.yml", "publish-web")) should include(Gate)
    jobIf(job("main.yml", "publish-worker")) should include(Gate)
  }

  "nix-stage-closures' stage job" should "only run where writes are allowed: auto-apply activates a staged pin" in {
    jobIf(job("nix-stage-closures.yml", "stage")) should include(Gate)
  }

  "the bisect's commit comment" should "only be written where writes are allowed" in {
    jobIf(job("convergence-bisect.yml", "tell")) should include(Gate)
  }

  "every github.com-only PR step" should "also need CI_ROLE=primary" in {
    val prSteps = workflows.flatMap { case (name, yml) =>
      RepoFile.withoutComments(yml).linesIterator.sliding(2).collect {
        case Seq(ifLine, next) if ifLine.contains("if: github.server_url == 'https://github.com'") &&
            (next.contains("create-pull-request") || next.contains("scala-steward-action")) => name -> ifLine.trim
      }
    }
    prSteps should not be empty
    prSteps.filterNot(_._2.contains(Primary)) shouldBe empty
  }

  "a scheduled workflow" should "start no job on github.com in shadow (but the cache prune, github.com's own)" in {
    val scheduled = workflows.filter { case (name, yml) =>
      name != "prune-actions-caches.yml" && RepoFile.withoutComments(RepoFile.block(yml, "on")).contains("schedule:")
    }
    scheduled should not be empty
    val ungated = for {
      (name, yml) <- scheduled
      (job, text) <- RepoFile.jobs(yml)
      if !text.linesIterator.exists(_.matches("""^\s+needs:.*"""))
      if !jobIf(text).contains(s"(github.event_name != 'schedule' || $Gate)")
    } yield s"$name: $job"
    ungated shouldBe empty
  }

  "the release, image, issue and marker writers" should "all go through scripts/ci/ci-writes.sh" in {
    Seq(
      "scripts/ci/gh-release.sh",
      "scripts/ci/publish-rolling-release.sh",
      "scripts/ci/build-image.sh",
      "scripts/ci/scheduled-failure-issue.sh",
      "scripts/ci/bot-branch-ready.sh",
      ".github/actions/drop-bot-branch/drop.sh",
      ".github/actions/mark-grafana-deploy/mark.sh"
    ).filterNot(p => RepoFile.withoutComments(RepoFile.read(p)).contains("ci-writes.sh")) shouldBe empty
  }

  "the metric pushes" should "be shadow-gated" in {
    RepoFile.read(".github/actions/report-timings/action.yml") should include("scripts/ci/ci-writes.sh\" || push=")
    RepoFile.read(".github/workflows/identity-live-drift.yml") should include(s"$Gate && '--push'")
    RepoFile.read(".github/workflows/country-convergence-leg.yml") should include(s"$Gate && '--push'")
  }
}
