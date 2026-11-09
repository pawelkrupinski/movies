package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Scala Steward's coursier comes from the Actions cache, not GitHub's CDN on every run: scala-steward-action downloads
 * its `coursier-cli-url` each time, and two 504s failed Scala Steward #146 (2026-11-09, run 42571891138) with "Unable
 * to install coursier". .github/actions/coursier-launcher keeps the pinned launcher as one cache entry keyed by version
 * and checksum, verified by scripts/ci/fetch-verified.sh, and serves it on 127.0.0.1 (scripts/ci/serve-file.sh).
 */
class CoursierLauncherSpec extends AnyFlatSpec with Matchers {
  private lazy val steward  = RepoFile.withoutComments(RepoFile.read(".github/workflows/scala-steward.yml"))
  private lazy val launcher = RepoFile.withoutComments(RepoFile.read(".github/actions/coursier-launcher/action.yml"))

  "Scala Steward" should "read coursier from the cached launcher, never the action's GitHub default" in {
    steward should include("uses: ./.github/actions/coursier-launcher")
    steward should include("coursier-cli-url: ${{ steps.coursier.outputs.url }}")
    val checkout = steward.indexOf("uses: actions/checkout@")
    checkout should be >= 0
    checkout should be < steward.indexOf("uses: ./.github/actions/coursier-launcher")
  }

  "the coursier launcher" should "be one verified cache entry, keyed by its pinned version and checksum, saved only on main" in {
    launcher should include("key: coursier-launcher-${{ runner.os }}-${{ inputs.version }}-${{ inputs.sha256 }}")
    launcher should include("scripts/ci/fetch-verified.sh")
    launcher should include("if: github.ref == 'refs/heads/main' && steps.restore.outputs.cache-hit != 'true'")
    launcher should include("scripts/ci/serve-file.sh")
    launcher should not include "raw/master"
  }
}
