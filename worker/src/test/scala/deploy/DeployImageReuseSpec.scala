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
 */
class DeployImageReuseSpec extends AnyFlatSpec with Matchers {
  private lazy val mainYml    = RepoFile.read(".github/workflows/main.yml")
  private lazy val deployJob  = RepoFile.block(mainYml, "deploy")
  private lazy val buildWeb    = RepoFile.block(mainYml, "build-web")
  private lazy val buildWorker = RepoFile.block(mainYml, "build-worker")

  "the deploy leg" should "release a pre-built image rather than build one" in {
    deployJob should include("-i ghcr.io/${{ github.repository_owner }}/movies-web:${{ github.sha }}")
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
    deployJob should include("needs: [ci, preflight]")
  }

  it should "release a tag those builds actually push" in {
    buildWeb    should include("ghcr.io/${{ github.repository_owner }}/movies-web:${{ github.sha }}")
    buildWorker should include("ghcr.io/${{ github.repository_owner }}/movies-worker:${{ github.sha }}")
  }

  /**
   * ...and NOT release when it pushed nothing. `build-web` is path-gated, so a
   * push that misses the tier pushes no tag for it — where the old `build-image`
   * built both tiers unconditionally and the case could not arise. Without this
   * the job names a tag that was never pushed, and an unchanged tier turns a
   * green build red.
   */
  it should "skip a commit whose web build pushed no tag" in {
    deployJob should include("needs.preflight.outputs.web-changed")
  }

  /**
   * The worker's no-op guard compared a hash BAKED INTO the image against one it
   * recomputed from the checkout, so a push that changed nothing about the worker
   * artifact would not restart it into a cold freshness re-hydrate plus a scrape
   * boot storm. It existed for the FLY worker deploy, which is gone: the worker is
   * a k3s pod now, and what restarts it is Flux picking up an image tag — and
   * `build-worker` is already path-gated, so a push that leaves the tier alone
   * builds no image for Flux to pick up. Baking a hash nothing reads back is the
   * kind of thing that survives for years; assert it is gone in BOTH places, since
   * either half left behind is dead weight that reads as live wiring.
   */
  it should "not bake a worker input hash nothing reads back any more" in {
    buildWorker should not include "WORKER_INPUT_HASH"
    deployJob   should not include "WORKER_INPUT_HASH"
    RepoFile.read("Dockerfile") should not include "WORKER_INPUT_HASH"
  }

  /**
   * The Grafana deploy marker was a job of its own (`annotate`, `needs: deploy`),
   * which spent ~10s of runner spin-up on the critical path to run one curl. It
   * rides the deploy now — where it also lands at a truer moment, when the
   * user-visible tier shipped rather than when the last of six legs stopped.
   */
  it should "mark the deploy from the deploy job rather than one of its own" in {
    mainYml should not include "annotate:"
    deployJob should include("Mark deploy in Grafana")
  }

  /**
   * The `deploy` job above only ships the retired `kinowo` Fly redirect host —
   * it hasn't shipped the tiers a production dashboard cares about since CI
   * stopped rolling k3s itself (see `record-web`'s own comment on why its
   * marker "no longer means what is live"). A marker that rides only `deploy`
   * ships a "deploy" annotation for an app nobody watches and none at all for
   * the tiers that page. `record-web`/`record-worker` are the closest CI gets
   * to "this tier shipped a new image" now, so the marker has to ride there
   * too — found 2026-09-15 when a dashboard showed no deploy lines at all.
   */
  it should "also mark web and worker deploys from record-web/record-worker" in {
    val recordWeb    = RepoFile.block(mainYml, "record-web")
    val recordWorker = RepoFile.block(mainYml, "record-worker")
    recordWeb    should include("Mark deploy in Grafana")
    recordWorker should include("Mark deploy in Grafana")
    // Both need the composite action on disk, which needs a checkout — the
    // `gh api` ref-write step above them needs no working tree at all, so
    // without this a spec-less regression could drop the checkout silently.
    recordWeb    should include("actions/checkout")
    recordWorker should include("actions/checkout")
  }

  /**
   * `curl -f` only trips on HTTP >= 400 — a 302 (oauth2-proxy's front door
   * redirecting an unauthenticated request to a Google login page, or a stale
   * Grafana host) reads as success and the annotation is silently never
   * written. Confirmed live 2026-09-15: both GET and POST to Grafana's API
   * 302 through the SSO front door with a Bearer token. Assert the marker
   * checks the real status code instead of trusting `-f`.
   */
  it should "treat anything other than HTTP 200 as a failed Grafana marker" in {
    val action = RepoFile.read(".github/actions/mark-grafana-deploy/action.yml")
    action should include("%{http_code}")
    action should not include "curl -fsS"
    action should include("\"$code\" = \"200\"")
  }

  /**
   * `free-runners` — the job that used to sit here cancelling an in-flight
   * `Country convergence` run to take its three runners back for the deploy —
   * was retired 2026-09-08 alongside that workflow adopting
   * `cancel-in-progress: false`. Cancelling a run that lane no longer intends
   * to throw away would destroy real progress rather than merely accelerate a
   * supersede that was coming anyway, which was the entire justification for
   * the step (see its retirement note in `main.yml` and
   * `ConvergenceConcurrencyConfigSpec`).
   */
  it should "not bring free-runners back now that both convergence lanes queue instead of cancel" in {
    mainYml should not include "free-runners:"
    mainYml should not include """gh run cancel"""
  }

  /**
   * The images are BUILT inside ci, from the dists its `e2e (staging)` row staged, while ci's slowest
   * rows still run; main.yml's `build-web` / `build-worker` only PUBLISH them once ci is green.
   * Restaging in main.yml was ~2 min of cold `sbt stage`, and building there ~1.7 min more, both on
   * the post-ci critical path. The upload and the download move together: an upload nothing downloads
   * is a GB of storage per run nobody complains about, and a download with no upload fails the build.
   */
  it should "build both images in ci from the dists ci staged, and only publish them after it" in {
    val ciYml = RepoFile.read(".github/workflows/ci.yml")
    // On the COMMANDS: the steps' own comments name what they replaced.
    def commands(block: String) = block.linesIterator.filterNot(_.trim.startsWith("#")).mkString("\n")
    for ((tier, publish) <- Seq("web" -> buildWeb, "worker" -> buildWorker)) {
      withClue(s"$tier: ") {
        val image = RepoFile.block(ciYml, s"image-$tier")
        ciYml should include(s"name: stage-$tier")
        image should include("actions/download-artifact")
        image should include(s"name: stage-$tier")
        image should include("needs: e2e")
        commands(publish) should not include "sbt "
        commands(publish) should not include "build-push-action"
        commands(publish) should include("docker buildx imagetools create")
      }
    }
    // …and the staging itself stays in ci: it is what proves the dists still link on a PR run.
    ciYml should include("""sbt "web/stage" "worker/stage"""")
    ciYml should include("Deploy artefacts carry no generated Scaladoc")
  }

  /**
   * THE SAFETY OF BUILDING EARLY. ci's image jobs run before ci is green, so they may push the one
   * tag nothing deploys — the commit SHA — and nothing else: Flux deploys `main-<utc>-<sha7>`, and a
   * hand-applied manifest resolves `latest`. Those two are attached only by main.yml's `build-*`,
   * which `needs: ci`; and never on a PR run.
   */
  it should "push only the SHA tag before ci is green, and ship tags only after it" in {
    val ciYml = RepoFile.read(".github/workflows/ci.yml")
    for ((tier, publish) <- Seq("web" -> buildWeb, "worker" -> buildWorker)) {
      withClue(s"$tier: ") {
        val image = RepoFile.block(ciYml, s"image-$tier")
        if (tier == "web") {
          val tags = image.linesIterator.map(_.trim).filter(_.startsWith("tags:")).toSeq
          tags shouldBe Seq(s"tags: ghcr.io/$${{ github.repository_owner }}/movies-$tier:$${{ github.sha }}")
        } else {
          // The worker's build pushes its image BEFORE AOT training under two tags nothing deploys,
          // and the training step (scripts/ci/train-worker-aot.sh) pushes the trained image under
          // the SHA tag — still the one nothing deploys until main.yml names it.
          val lines = image.linesIterator.map(_.trim).toVector
          val tags  = lines.dropWhile(_ != "tags: |").drop(1).takeWhile(_.startsWith("ghcr.io/"))
          tags shouldBe Seq(
            s"ghcr.io/$${{ github.repository_owner }}/movies-worker:$${{ github.sha }}-untrained",
            s"ghcr.io/$${{ github.repository_owner }}/movies-worker:untrained-latest")
          image should include("scripts/ci/train-worker-aot.sh")
          lines should contain(s"ghcr.io/$${{ github.repository_owner }}/movies-worker:$${{ github.sha }}")
        }
        image should not include "steps.tag.outputs.value"
        image should include("if: github.event_name != 'pull_request'")
        publish should include("needs: [ci, preflight]")
        publish should include(s"-t ghcr.io/$${{ github.repository_owner }}/movies-$tier:$${{ steps.tag.outputs.value }}")
        publish should include(s"-t ghcr.io/$${{ github.repository_owner }}/movies-$tier:latest")
      }
    }
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
