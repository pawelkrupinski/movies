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
   * The deploy wants all 20 of the account's concurrent jobs, and a convergence
   * run in flight holds 3 of them for 25-90 minutes — so a push landing in that
   * window doesn't get 3 of its jobs until a convergence leg finishes. Measured:
   * 17 jobs started at once, 3 waited ~200s, and one of the three was the
   * longest job in the build; the run took 10m28s against a 7m critical path.
   *
   * Cancelling costs that suite almost nothing — its own workflow keeps one lane
   * with `cancel-in-progress`, so the next push was going to discard the run
   * anyway (6 of 10 consecutive runs ended `cancelled`), and `kick-convergence`
   * re-dispatches it later in this same run against the newer commit. What
   * matters is WHERE it happens: in `build-image`, which has no `needs` and so
   * starts with ci's jobs, the runners come free while they are still queueing.
   * Moved into a job that waits for ci, it would free nothing.
   */
  it should "take the runners back from an in-flight convergence run, before ci needs them" in {
    buildImage should include("""gh run list --workflow "Country convergence"""")
    buildImage should include("gh run cancel")
    buildImage should include("actions: write")
    buildImage should not include "needs:"
  }

  /**
   * Including one that has not STARTED yet. A queued convergence run holds no
   * runner, so it reads as harmless — but it takes the first runners to free up,
   * which are precisely the ones this deploy is queueing for. Selecting on
   * `in_progress` alone left the hole half-plugged.
   */
  it should "cancel a convergence run that is merely queued, not only one already running" in {
    buildImage should include("""select(.status != "completed")""")
    buildImage should not include "--status in_progress"
  }

  /**
   * `test` used to upload the staged dists for the deploy leg to download. The
   * leg builds from its own checkout now, so an upload here would be an artifact
   * with no consumer — 16s and a GB of storage per run, and the kind of thing
   * that survives for years because nothing complains. The two must move
   * together: no `download-artifact` in the leg means no `stage-*` upload in ci.
   */
  it should "not publish a build artifact nothing downloads" in {
    val ciYml = RepoFile.read(".github/workflows/ci.yml")
    ciYml should not include "name: stage-web"
    ciYml should not include "name: stage-worker"
    // …but the staging itself stays: it is what proves the dist still links.
    ciYml should include("""sbt "web/stage" "worker/stage"""")
    ciYml should include("Deploy artefacts carry no generated Scaladoc")
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
