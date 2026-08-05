package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards the split between BUILDING the container image and RELEASING it.
 *
 * The deploy leg used to do both — `flyctl deploy --remote-only` downloaded the
 * staged dist, built the image on Fly's builder and rolled the machines, ~86s of
 * which only the roll actually needed a green test run. Building is a pure
 * function of the sources, so `build-image` now does it concurrently with ci and
 * the leg only points machines at a tag that already exists.
 *
 * Two things must stay true for that to be both fast and safe:
 *
 *  - the leg must not build (a `--remote-only` creeping back puts the ~86s back
 *    on the post-CI tail, and the pre-built image becomes dead weight), and
 *  - the leg must still `needs: ci`, or the split turns into shipping untested
 *    code. That is the whole reason the build may run early: nothing it produces
 *    reaches a machine until the tests are green.
 *
 * Plus the hash agreement below, which is the subtle one.
 */
class DeployImageReuseSpec extends AnyFlatSpec with Matchers {
  private lazy val deployYml  = RepoFile.read(".github/workflows/deploy.yml")
  private lazy val deployJob  = RepoFile.block(deployYml, "deploy")
  private lazy val buildImage = RepoFile.block(deployYml, "build-image")

  "the deploy leg" should "release a pre-built image rather than build one" in {
    deployJob should include("-i registry.fly.io/${{ matrix.builder }}:${{ github.sha }}")
    deployJob should not include "--remote-only"
    deployJob should not include "download-artifact"
  }

  it should "still wait for a green build before releasing anything" in {
    deployJob should include("needs: [ci, build-image]")
  }

  it should "tag the image with the commit, so the leg can name it without an output" in {
    buildImage should include("--image-label ${{ github.sha }}")
  }

  /**
   * The worker's no-op guard compares a hash BAKED INTO the image against one it
   * recomputes from the checkout. Those two hashes are now written in different
   * jobs, so they can drift apart — and drift is silent in the worst direction:
   * the guard simply never matches, and every push pays the worker a cold
   * freshness re-hydrate plus a scrape boot storm, which is the exact thing the
   * guard exists to prevent.
   */
  it should "hash the same worker inputs in the image build as in the skip guard" in {
    val inputs = "for p in worker common build.sbt project Dockerfile"
    buildImage should include(s"$inputs fly.worker.toml")
    deployJob should include(s"$inputs $${{ matrix.toml }}")
  }

  /**
   * The Grafana deploy marker was a job of its own (`annotate`, `needs: deploy`),
   * which spent ~10s of runner spin-up on the critical path to run one curl. It
   * rides the web leg now — where it also lands at a truer moment, when the
   * user-visible tier shipped rather than when the last of six legs stopped.
   */
  it should "mark the deploy from the web leg rather than a job of its own" in {
    deployYml should not include "annotate:"
    deployJob should include("Mark deploy in Grafana")
    deployJob should include("matrix.app == 'kinowo'")
  }

  /**
   * The roll-back guard walks history with `git merge-base` against whatever
   * commit is live, so it needs full history — but only commits and trees, never
   * a file's contents. Fetching every blob in this repo's history (the fixture
   * corpus included) was ~15s of the deploy's critical path for data nothing
   * reads.
   */
  it should "check out history without the blobs the guard never reads" in {
    deployJob should include("fetch-depth: 0")
    deployJob should include("filter: blob:none")
  }
}
