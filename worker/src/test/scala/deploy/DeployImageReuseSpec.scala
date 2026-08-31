package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards the split between BUILDING the container image and RELEASING it.
 *
 * The deploy leg used to do both — `flyctl deploy --remote-only` downloaded the
 * staged dist, built the image on Fly's builder and rolled the machines, ~86s of
 * which only the roll actually needed a green test run.
 *
 * The build moved out, first to a `build-image` job running alongside ci, and
 * then out of Fly entirely: `build-web` / `build-worker` were already building
 * the same Dockerfile with the same build-args and pushing to GHCR for the
 * cluster, so the Fly copy was a second build of identical bytes — on a builder
 * this project does not run, which failed roughly two runs in five with
 * `timed out connecting to machine`. The leg now releases the GHCR image.
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
  private lazy val mainYml    = RepoFile.read(".github/workflows/main.yml")
  private lazy val deployJob  = RepoFile.block(mainYml, "deploy")
  private lazy val freeRunners = RepoFile.block(mainYml, "free-runners")
  private lazy val buildWeb    = RepoFile.block(mainYml, "build-web")
  private lazy val buildWorker = RepoFile.block(mainYml, "build-worker")

  "the deploy leg" should "release a pre-built image rather than build one" in {
    deployJob should include("-i ghcr.io/${{ github.repository_owner }}/${{ matrix.ghcr }}:${{ github.sha }}")
    deployJob should not include "--remote-only"
    deployJob should not include "download-artifact"
  }

  // ONE IMAGE, NOT TWO. Fly is released with the bytes the cluster already runs,
  // which is the whole point: a second build of the same Dockerfile could differ
  // from the first only by failing, and on Fly's builder it usually did.
  it should "not build an image on Fly at all" in {
    // On the COMMANDS, not the file: the comment above the release step names
    // both of these while explaining why they are gone, and a spec that forbids
    // saying so would delete the explanation along with the behaviour.
    val commands = mainYml.linesIterator.filterNot(_.trim.startsWith("#")).mkString("\n")
    commands should not include "--build-only"
    commands should not include "registry.fly.io"
  }

  it should "still wait for a green build before releasing anything" in {
    deployJob should include("needs: [ci, build-web, build-worker]")
  }

  it should "release a tag those builds actually push" in {
    buildWeb    should include("ghcr.io/${{ github.repository_owner }}/movies-web:${{ github.sha }}")
    buildWorker should include("ghcr.io/${{ github.repository_owner }}/movies-worker:${{ github.sha }}")
  }

  /**
   * ...and NOT release when they pushed nothing. Both builds are path-gated, so
   * a push that misses a tier pushes no tag for it — where the old `build-image`
   * built both unconditionally and the case could not arise. Without this the leg
   * names a tag that was never pushed, and an unchanged tier turns a green build
   * red.
   */
  it should "skip a tier whose build pushed no tag for this commit" in {
    deployJob should include("needs.build-web.outputs.changed")
    deployJob should include("needs.build-worker.outputs.changed")
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
    buildWorker should include(s"$inputs fly.worker.toml")
    buildWorker should include("WORKER_INPUT_HASH=")
    deployJob   should include(s"$inputs $${{ matrix.toml }}")
  }

  /**
   * The Grafana deploy marker was a job of its own (`annotate`, `needs: deploy`),
   * which spent ~10s of runner spin-up on the critical path to run one curl. It
   * rides the web leg now — where it also lands at a truer moment, when the
   * user-visible tier shipped rather than when the last of six legs stopped.
   */
  it should "mark the deploy from the web leg rather than a job of its own" in {
    mainYml should not include "annotate:"
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
   * matters is WHERE it happens: in a job with no `needs`, which starts with
   * ci's, the runners come free while they are still queueing. Moved into a job
   * that waits for ci, it would free nothing.
   *
   * This outlived the image build it used to share a job with. When that build
   * left for GHCR the cancelling stayed, in a job of its own, for exactly the
   * reason above — which is also why deleting the build freed no runner for ci.
   */
  it should "take the runners back from an in-flight convergence run, before ci needs them" in {
    freeRunners should include("""gh run list --workflow "Country convergence"""")
    freeRunners should include("gh run cancel")
    freeRunners should include("actions: write")
    freeRunners should not include "needs:"
  }

  /**
   * Including one that has not STARTED yet. A queued convergence run holds no
   * runner, so it reads as harmless — but it takes the first runners to free up,
   * which are precisely the ones this deploy is queueing for. Selecting on
   * `in_progress` alone left the hole half-plugged.
   */
  /**
   * But NOT the United States' build, which is the one run that has to finish.
   *
   * It is a separate workflow precisely so that nothing preempts it: its lane is
   * `cancel-in-progress: false`, and cancelling it here would reintroduce the
   * supersede this whole split removed — from the other side, and invisibly, since a
   * run cancelled by this step reads as an ordinary superseded run. The three warm
   * countries stay fair game; they re-run green in a dozen minutes.
   *
   * The cost is that the US leg holds two of the account's twenty concurrent jobs for
   * as long as it runs. That is the price of an answer that arrives at all.
   */
  it should "leave the US convergence build alone — it is the run that must finish" in {
    // The step's COMMENTS name it, which is the point of them; only the commands are
    // being asserted on here.
    val commands = freeRunners.linesIterator.map(_.trim).filterNot(_.startsWith("#")).mkString("\n")
    commands should not include "US convergence"
  }

  it should "cancel a convergence run that is merely queued, not only one already running" in {
    freeRunners should include("""select(.status != "completed")""")
    freeRunners should not include "--status in_progress"
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
