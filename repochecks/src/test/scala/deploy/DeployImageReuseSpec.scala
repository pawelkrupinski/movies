package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards the split between BUILDING the container images and PUBLISHING them.
 *
 * ci builds both images from the dists its `e2e (corpus)` row staged, and pushes
 * them under the commit SHA alone — a tag nothing deploys. main.yml's `publish-web`
 * / `publish-worker` give those bytes the tags Flux ships only once ci is green, so
 * the early build never reaches a machine untested.
 *
 * It used to be a third copy: a Fly `deploy` leg released the same image to the
 * retired `kinowo.fly.dev` redirect host. That leg and the Fly app's config are
 * gone; the one assertion left about it below keeps a Fly build from creeping back.
 */
class DeployImageReuseSpec extends AnyFlatSpec with Matchers {
  private lazy val mainYml    = RepoFile.read(".github/workflows/main.yml")
  private lazy val publishWeb    = RepoFile.block(mainYml, "publish-web")
  private lazy val publishWorker = RepoFile.block(mainYml, "publish-worker")
  private lazy val ciYml       = RepoFile.read(".github/workflows/ci.yml")

  /** Where each tier's image is built: both in ci's `e2e (corpus)` row, which staged their dists (2026-11-01: the
   *  worker's was main.yml's `image-worker`, two queue hops after ci). */
  private def imageJob: String = RepoFile.block(ciYml, "e2e")

  // ONE IMAGE PER TIER, built once in ci. A Fly build of the same Dockerfile (on a
  // builder this project does not run) failed roughly two runs in five; with the
  // Fly app retired from CI there is nothing it could even be for.
  "the main workflow" should "not build or release anything on Fly" in {
    // On the COMMANDS, not the file: comments may name what is gone.
    val commands = mainYml.linesIterator.filterNot(_.trim.startsWith("#")).mkString("\n")
    commands should not include "--build-only"
    commands should not include "registry.fly.io"
    commands should not include "flyctl"
    commands should not include "FLY_API_TOKEN"
  }

  it should "release a tag those builds actually push" in {
    publishWeb    should include("${{ env.IMAGE_REPO }}/movies-web:${{ github.sha }}")
    publishWorker should include("${{ env.IMAGE_REPO }}/movies-worker:${{ github.sha }}")
  }

  /**
   * The worker's no-op guard compared a hash BAKED INTO the image against one it
   * recomputed from the checkout, so a push that changed nothing about the worker
   * artifact would not restart it into a cold freshness re-hydrate plus a scrape
   * boot storm. It existed for the FLY worker deploy, which is gone: the worker is
   * a k3s pod now, and what restarts it is Flux picking up an image tag — and
   * `publish-worker` is already path-gated, so a push that leaves the tier alone
   * builds no image for Flux to pick up. Baking a hash nothing reads back is the
   * kind of thing that survives for years; assert it is gone from both the build and
   * the image, since either half left behind is dead weight that reads as live wiring.
   */
  it should "not bake a worker input hash nothing reads back any more" in {
    publishWorker should not include "WORKER_INPUT_HASH"
    RepoFile.read("Dockerfile") should not include "WORKER_INPUT_HASH"
  }

  /**
   * `publish-web`/`publish-worker` are the closest CI gets to "this tier shipped a
   * new image" — CI does not roll the cluster itself — so the Grafana deploy
   * marker rides there. Found 2026-09-15 when a dashboard showed no deploy lines
   * at all: the marker then rode only the Fly redirect host's release, an app
   * nobody watched.
   */
  it should "mark web and worker deploys from publish-web/publish-worker" in {
    mainYml should not include "annotate:"
    publishWeb    should include("Mark deploy in Grafana")
    publishWorker should include("Mark deploy in Grafana")
    // Both need the composite action on disk, which needs a checkout — the
    // `gh api` ref-write step above them needs no working tree at all, so
    // without this a spec-less regression could drop the checkout silently.
    publishWeb    should include("actions/checkout")
    publishWorker should include("actions/checkout")
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
    val action = RepoFile.read(".github/actions/mark-grafana-deploy/mark.sh")
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
   * The images are BUILT inside ci, from the dists its `e2e (corpus)` row staged, while ci's slowest
   * rows still run; main.yml's `publish-web` / `publish-worker` only PUBLISH them once ci is green.
   * Restaging in main.yml was ~2 min of cold `sbt stage`, and building there ~1.7 min more, both on
   * the post-ci critical path. The upload and the download move together: an upload nothing downloads
   * is a GB of storage per run nobody complains about, and a download with no upload fails the build.
   */
  it should "build both images while ci runs, from the dists ci staged, and only publish them after it" in {
    // On the COMMANDS: the steps' own comments name what they replaced.
    def commands(block: String) = block.linesIterator.filterNot(_.trim.startsWith("#")).mkString("\n")
    for ((tier, publish) <- Seq("web" -> publishWeb, "worker" -> publishWorker)) {
      withClue(s"$tier: ") {
        val image = imageJob
        // Each image is built where its dist was staged, from the stage on disk, BESIDE the e2e run (2026-10-23; the
        // worker's since 2026-11-01), its verdict collected after it, failing the row. No dist is uploaded for a job
        // after ci to build from.
        ciYml should not include s"name: stage-$tier"
        val copy = if (tier == "web") "cp -R web/target/universal/stage stage" else """cp -R worker/target/universal/stage "$context/stage""""
        RepoFile.positionOf(image, """sbt "web/stage" "worker/stage"""") should be < RepoFile.positionOf(image, copy)
        RepoFile.positionOf(image, copy) should be < RepoFile.positionOf(image, s"in-background.sh start $tier-image")
        RepoFile.positionOf(image, s"in-background.sh start $tier-image") should be < RepoFile.positionOf(image, "run: sbt ${{ matrix.cmd }}")
        RepoFile.positionOf(image, "run: sbt ${{ matrix.cmd }}") should be < RepoFile.positionOf(image, s"in-background.sh wait $tier-image")
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
   * hand-applied manifest resolves `latest`. Those two are attached only by main.yml's `publish-*`,
   * which `needs: ci`; and never on a PR run.
   */
  it should "push only the SHA tag before ci is green, and ship tags only after it" in {
    for ((tier, publish) <- Seq("web" -> publishWeb, "worker" -> publishWorker)) {
      withClue(s"$tier: ") {
        val image = imageJob
        // both through build-image.sh, which tags `<repository>:<sha>` and nothing else (build-image-test.sh)
        val code = RepoFile.withoutComments(image)
        if (tier == "web") {
          val builds = """scripts/ci/build-image\.sh (.+)""".r.findAllMatchIn(code).map(_.group(1).trim).toSeq
          builds shouldBe Seq(s"web $${{ env.IMAGE_REPO }}/movies-web $${{ github.sha }}")
        } else {
          // From its own context (the Dockerfile's whitelist sees only `stage/`), through the same script.
          code should include("\"$PWD/scripts/ci/build-image.sh\" \\")
          code should include("'cd \"$0\" && \"$1\" worker \"$2\" \"$3\"'")
          code should include("${{ env.IMAGE_REPO }}/movies-worker ${{ github.sha }}")
        }
        RepoFile.withoutComments(image) should not include "--tag"
        image should not include "steps.tag.outputs.value"
        // Both builds sit in ci, which a PR run calls too: never on one.
        RepoFile.step(ciYml, s"Build and push the $tier image under this commit's SHA tag only") should include(
          "if: matrix.shard == 'corpus' && !github.event.pull_request")
        RepoFile.step(ciYml, s"The $tier image was built and pushed") should include("!github.event.pull_request")
        publish should include("needs: [ci, gates]")
        publish should include(s"-t $${{ env.IMAGE_REPO }}/movies-$tier:$${{ steps.tag.outputs.value }}")
        publish should include(s"-t $${{ env.IMAGE_REPO }}/movies-$tier:latest")
      }
    }
  }

  /** ONE QUEUE HOP AFTER ci (2026-11-01): no job builds an image after ci, so each publish needs only ci and gates. */
  it should "publish both tiers straight after ci, with no image job between" in {
    mainYml should not include "\n    image-worker:"
    publishWeb should include("needs: [ci, gates]")
    publishWorker should include("needs: [ci, gates]")
  }
}
