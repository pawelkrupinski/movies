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
  private val MainOnly = "if: github.ref == 'refs/heads/main'"
  private lazy val action = RepoFile.read(".github/actions/convergence-publish/action.yml")

  "the convergence publish action" should "upload to the shared release only from main" in {
    val publish = RepoFile.step(action, "Publish the tree to the rolling release")
    publish should include("\"$RELEASE\" upload")
    publish should include(MainOnly)
  }

  "the convergence workflows" should "write the shared release nowhere except through that gated action" in {
    val writers = Seq(
      ".github/workflows/country-convergence-leg.yml",
      ".github/workflows/country-convergence.yml",
      ".github/workflows/us-convergence.yml")
      .filter(path => RepoFile.read(path).linesIterator
        .filterNot(_.trim.startsWith("#"))
        .exists(l => l.contains("gh release upload") || l.contains("gh release create") ||
          l.contains("gh-release.sh")))
    writers shouldBe empty
  }
}
