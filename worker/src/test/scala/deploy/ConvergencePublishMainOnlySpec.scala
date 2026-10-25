package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Only `main` writes the shared `convergence-fixtures` release.
 *
 * Every convergence leg restores its enrichment tree from that one rolling release and
 * `--clobber`s it back on the way out, under `always()`. A leg dispatched on a branch to
 * VERIFY a fix would otherwise overwrite the tree every main run restores — with a capture
 * recorded by code main has never run. Gating the write lets a branch dispatch replay the
 * shared tree read-only.
 */
class ConvergencePublishMainOnlySpec extends AnyFlatSpec with Matchers {
  private val MainOnly = "github.ref == 'refs/heads/main'"
  // Forgejo starts a SCHEDULED run with the bare branch name as its ref: under the refs/heads/main test alone, no
  // nightly recording published its tree from 2026-10-20 to 10-23 (runs 647, 985, 1578).
  private val MainOrForgejoSchedule = "(github.ref == 'refs/heads/main' || github.ref == 'main')"
  private lazy val action = RepoFile.read(".github/actions/convergence-publish/action.yml")

  "the convergence publish action" should "upload to the shared release only from main" in {
    val publish = RepoFile.step(action, "Publish the tree to the rolling release")
    publish should include(".github/scripts/convergence-publish.sh tree")
    publish should include(MainOnly)
    RepoFile.read(".github/scripts/convergence-publish.sh") should include("\"$RELEASE\" upload")
  }

  "the convergence workflows" should "write the shared release nowhere except through that gated action" in {
    val writers = Seq(
      ".github/workflows/country-convergence-leg.yml",
      ".github/workflows/identity-model-convergence.yml")
      .filter(path => RepoFile.read(path).linesIterator
        .filterNot(_.trim.startsWith("#"))
        .exists(l => l.contains("gh release upload") || l.contains("gh release create") ||
          l.contains("gh-release.sh")))
    writers shouldBe empty
  }

  // The last-green corpus marker `CorpusProvenance` diffs against: only a main full leg
  // whose suite PASSED may write it, and setup must read back the same name.
  "the green-corpus marker" should "be written only by a passing main full leg, under the name setup reads" in {
    val record = RepoFile.step(action, "Record the corpus this green leg replayed")
    record should include(MainOrForgejoSchedule)
    record should include("inputs.green == 'true'")
    record should include("corpus-green-${{ inputs.code }}.txt")
    RepoFile.read(".github/actions/convergence-setup/action.yml") should include("corpus-green-${{ inputs.code }}.txt")
    val leg = RepoFile.read(".github/workflows/country-convergence-leg.yml")
    leg.split("\\n").count(_.contains("green: ${{ steps.suite.outcome == 'success' && steps.suite.outputs.failed != 'true' }}")) shouldBe 1
  }

  "every recording write" should "also run on a Forgejo scheduled run, whose ref is the bare branch name" in {
    Seq(
      "Publish the tree to the rolling release",
      "Publish this healthy recording's manifest as the next one's baseline",
      "Record the corpus this green leg replayed",
      "Pin this recording as the pair hermetic legs replay",
      "Mark this recording as prod-latest").foreach { name =>
      withClue(s"$name: ") { RepoFile.step(action, name) should include(s"if: $MainOrForgejoSchedule && ") }
    }
  }

  // The nightly recording becomes prod-latest, never the hermetic pair (the landing bar), unless a dispatch asks.
  "a nightly recording" should "be marked prod-latest without moving the hermetic pair" in {
    RepoFile.step(action, "Pin this recording as the pair hermetic legs replay") should include("inputs.pin == 'true'")
    RepoFile.step(action, "Mark this recording as prod-latest") should include("""convergence-publish.sh latest "$CODE"""")
    RepoFile.step(action, "Mark this recording as prod-latest") should not include "inputs.pin"
    RepoFile.read(".github/workflows/country-convergence-leg.yml") should include("pin: ${{ inputs.pin-hermetic }}")
    RepoFile.read(".github/workflows/record-scrape-fixtures.yml") should include(
      "pin-hermetic:   ${{ github.event_name == 'workflow_dispatch' && inputs.pin-hermetic == true }}")
  }
}
