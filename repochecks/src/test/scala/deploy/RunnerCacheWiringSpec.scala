package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The sbt dependency and build caches' restore-only steps go through `.github/actions/cache-restore`, which serves the
 *  self-hosted runner's local copy of the entry GitHub picks (scripts/ci/runner_cache.py; ci-runner's mount, bitcashier
 *  infra movies-runners.nix). Those two restores were 57 of a landing check's ~67 s of build setup on ci-runner
 *  (precommit-check run 44045907609, 2026-11-17). The mirror must only ever serve the entry GitHub matched: it asks
 *  with `lookup-only` and extracts that matched key, never one of its own choosing. */
class RunnerCacheWiringSpec extends AnyFlatSpec with Matchers {
  private val Restore = ".github/actions/cache-restore/action.yml"

  private def stepAfter(file: String, marker: String): String = {
    val text = RepoFile.read(file)
    val at = text.indexOf(marker)
    withClue(s"$file has no `$marker`: ") { at should be >= 0 }
    text.substring(at).linesIterator.drop(1).takeWhile(l => !l.trim.startsWith("- ")).mkString("\n")
  }

  "the restore-only sbt caches" should "restore through the runner-local mirror" in {
    stepAfter(".github/actions/setup-jdk/action.yml", "id: sbt-deps-restore") should include(
      "uses: ./.github/actions/cache-restore")
    stepAfter(".github/actions/sbt-target-cache/action.yml", "id: restore") should include(
      "uses: ./.github/actions/cache-restore")
  }

  "the mirror action" should "extract only the entry GitHub matched, asked without a download" in {
    val lookup = stepAfter(Restore, "id: lookup")
    lookup should include("uses: actions/cache/restore@v5")
    lookup should include("lookup-only: 'true'")
    val mirror = stepAfter(Restore, "id: mirror")
    mirror should include("if: steps.lookup.outputs.cache-matched-key != ''")
    mirror should include("KEY: ${{ steps.lookup.outputs.cache-matched-key }}")
  }

  it should "fall back to the download on any mirror miss, and mirror only what it downloaded" in {
    stepAfter(Restore, "id: download") should include("if: steps.mirror.outputs.restored != 'true'")
    val text = RepoFile.read(Restore)
    text should include("KEY: ${{ steps.download.outputs.cache-matched-key }}")
    text should include("runner_cache.py\" save")
  }

  it should "never save to GitHub's cache: only main saves (CacheSavePolicySpec)" in {
    val text = RepoFile.withoutComments(RepoFile.read(Restore))
    text should not include "actions/cache/save@"
    text should not include "uses: actions/cache@"
  }
}
