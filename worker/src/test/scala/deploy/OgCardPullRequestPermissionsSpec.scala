package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The OG-card refresh PR is pushed and opened with the "Kinowo Sync Bot" App token -- that is
 * what makes Branch CI run on it (a PR from `GITHUB_TOKEN` never fires `pull_request`). So the
 * job's `GITHUB_TOKEN` writes nothing: checkout reads, the artifacts come over the runner's own
 * token, and create-pull-request replaces checkout's credentials with the App's. A write grant
 * there was a standing privilege nothing used.
 */
class OgCardPullRequestPermissionsSpec extends AnyFlatSpec with Matchers {
  private lazy val openPr =
    RepoFile.block(RepoFile.read(".github/workflows/regenerate-og-cards.yml"), "open-pr")

  "the OG-card open-pr job" should "push and open its PR with the App token" in {
    RepoFile.step(openPr, "Open regenerate PR") should include("token: ${{ steps.app-token.outputs.token }}")
  }

  it should "grant GITHUB_TOKEN nothing to write" in {
    val permissions = RepoFile.block(openPr, "permissions")
    permissions should include("contents: read")
    permissions should not include "write"
  }
}
