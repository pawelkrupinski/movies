package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The live-site specs run NIGHTLY and gate nothing (the owner, 2026-11-25: Metacritic answered 200 for any slug and
 * blocked two landings). Three places must agree: build.sbt excludes the `integration.LiveSites` tag from the `it`
 * config unless LIVE_SITES is set (so `itAll` never selects a tagged spec), live-sites.yml sets it and runs `itLive`,
 * and no gating workflow or registry row names the workflow. The tag itself is held by worker's LiveSitesTaggedSpec.
 */
class LiveSitesWorkflowSpec extends AnyFlatSpec with Matchers {
  private lazy val build    = RepoFile.read("build.sbt")
  private lazy val workflow = RepoFile.read(".github/workflows/live-sites.yml")

  "build.sbt" should "leave the LiveSites tag out of every integration run, and select it only under LIVE_SITES" in {
    build should include("""lazy val LiveSitesTag = "integration.LiveSites"""")
    build should include("""get("LIVE_SITES").exists(_.nonEmpty)) "-n" else "-l", LiveSitesTag""")
    build.linesIterator.count(_.contains(".settings(itReportSettings, itRowSettings, liveSitesSettings)")) shouldBe 2
    build should include("""addCommandAlias("itAll", "all web/IntegrationTest/test worker/IntegrationTest/test")""")
  }

  "live-sites.yml" should "run the tagged specs nightly through itLive, bounded, and report a failure" in {
    workflow should include("cron:")
    workflow should include("types: [scheduled-live-sites]")
    val run = RepoFile.stepScript(RepoFile.block(workflow, "jobs"), "Live-site specs")
    run should include(". scripts/ci/bounded-step.sh")
    run should include("sbt itLive")
    RepoFile.step(RepoFile.block(workflow, "jobs"), "Live-site specs") should include("LIVE_SITES: '1'")
    workflow should include("scripts/ci/bounded-step.sh --reap")
    workflow should include("uses: ./.github/actions/report-scheduled-run")
    workflow should include("github.event_name == 'schedule' || github.event_name == 'repository_dispatch'")
  }

  it should "gate nothing: no registry row, and no gate workflow runs it" in {
    RepoFile.read(".github/gating-checks.tsv") should not include "live-sites"
    RepoFile.read(".github/workflows/main.yml") should not include "itLive"
    RepoFile.read(".github/workflows/ci.yml") should not include "itLive"
    RepoFile.read(".github/workflows/ci.yml") should not include "LIVE_SITES"
  }
}
