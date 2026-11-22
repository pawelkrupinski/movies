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
  private lazy val action = RepoFile.read(".github/actions/convergence-publish/action.yml")

  "the convergence publish action" should "upload to the shared release only from main" in {
    val publish = RepoFile.step(action, "Publish the tree to the rolling release")
    publish should include(".github/scripts/convergence-publish.sh tree")
    publish should include(MainOnly)
    RepoFile.read(".github/scripts/convergence_publish.py") should include("""self.release("upload", tag, archive""")
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
    record should include(MainOnly)
    record should include("inputs.green == 'true'")
    record should include("corpus-green-${{ inputs.code }}.txt")
    RepoFile.read(".github/actions/convergence-setup/action.yml") should include("corpus-green-${{ inputs.code }}.txt")
    val leg = RepoFile.read(".github/workflows/country-convergence-leg.yml")
    leg.split("\\n").count(_.contains("green: ${{ steps.suite.outcome == 'success' && steps.suite.outputs.failed != 'true' }}")) shouldBe 1
  }

  "every recording write" should "run only from main" in {
    Seq(
      "Publish the tree to the rolling release",
      "Publish this healthy recording's manifest as the next one's baseline",
      "Record the corpus this green leg replayed",
      "Mark this recording as prod-latest").foreach { name =>
      withClue(s"$name: ") { RepoFile.step(action, name) should include(s"if: $MainOnly && ") }
    }
  }

  // A recording becomes prod-latest, never the hermetic pair (the landing bar), in its own run: the pair moves only through
  // THE PIN GUARD, once the recording's verify on main's code is green with no served-corpus finding -- so new data can
  // never turn the gate red by itself.
  "a recording" should "be marked prod-latest, and pinned only by its verify's pin guard" in {
    action should not include "Pin this recording as the pair hermetic legs replay"
    action should not include "inputs.pin "
    RepoFile.step(action, "Mark this recording as prod-latest") should include("""convergence-publish.sh latest "$CODE"""")
    val leg = RepoFile.read(".github/workflows/country-convergence-leg.yml")
    leg should not include "pin-hermetic"
    val recorder = RepoFile.read(".github/workflows/record-scrape-fixtures.yml")
    // Decided in the recorder's plan job (WorkflowPlanSpec), handed to the verify it dispatches -- never to its legs.
    recorder should include("pin-hermetic: ${{ github.event_name == 'workflow_dispatch' && inputs.pin-hermetic == true }}")
    RepoFile.block(recorder, "enrichment") should not include "pin-hermetic"
    RepoFile.step(recorder, "Dispatch the hermetic verify of this recording") should include("-f pin-hermetic=\"$PIN_HERMETIC\"")
    val verify = RepoFile.read(".github/workflows/verify-scrape-recording.yml")
    val pin = RepoFile.block(verify, "pin")
    pin should include("needs: [ preflight, leg, order-independence-p01, order-independence-p12 ]")
    pin should include("needs.preflight.outputs.pin-hermetic == 'true'")
    pin should include("needs.leg.result == 'success'")
    pin should include(".github/scripts/convergence-publish.sh verified-pin")
    pin should include("contents: write")
    // The only writer of the hermetic marker.
    val writers = Seq(".github/actions/convergence-publish/action.yml", ".github/scripts/convergence-publish.sh",
      ".github/scripts/convergence_publish_pin.py",
      ".github/workflows/verify-scrape-recording.yml", ".github/workflows/record-scrape-fixtures.yml",
      ".github/workflows/country-convergence-leg.yml", ".github/workflows/identity-model-convergence.yml")
      .filter(path => RepoFile.read(path).linesIterator.filterNot(_.trim.startsWith("#"))
        .exists(line => line.contains("> \"hermetic-") || line.contains("""marker = f"hermetic-""")))
    writers shouldBe Seq(".github/scripts/convergence_publish_pin.py")
    val pin = RepoFile.pythonFunction(".github/scripts/convergence_publish_pin.py", "country")
    pin should include("""marker = f"hermetic-{code}.txt"""")
    pin should include("""f.write(f"{self.run} {self.recorded(code) or 'unknown'} {tree}\n")""")
  }
}
