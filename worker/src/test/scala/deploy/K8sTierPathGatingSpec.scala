package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}

/**
 * A WEB-ONLY PUSH MUST NOT RESTART THE WORKER. That is the whole of this spec.
 *
 * Deploying a worker restarts it, and a restarted worker cold-boots into a
 * freshness re-hydrate plus a scrape storm that drains the shared-CPU credit —
 * so the worker's deploy has always been gated on the worker's own inputs
 * changing. That gating used to be `on.push.paths` in a workflow of its own
 * (`build-worker-image.yaml`), which is what its header comment meant by "the
 * two tiers must have DIFFERENT `paths:` filters" and why it was a second file
 * rather than a matrix leg.
 *
 * The two image workflows are folded into `main.yml` now, and `on.push.paths`
 * is workflow-level: one workflow cannot carry two filters. The gating moved to
 * the JOB level — each tier hands its own path list to the `changed-paths`
 * composite action, and both the build steps and the deploy job's `if:` read
 * that answer. A union of the two lists, or a copy-paste that leaves both jobs
 * on the same list, would silently restore the exact failure the split existed
 * to prevent and nothing else in the repo would notice.
 *
 * So this spec does not assert on the shape of the YAML and hope. It reads the
 * REAL filter lists out of the workflow, feeds them to the REAL matcher the
 * action runs, and asks the question directly: given a push that touched only
 * web sources, does the worker's filter set say yes?
 *
 * The cases that run that matcher are in [[K8sTierPathGatingIntegrationSpec]]; the ones here read the workflow alone.
 */
class K8sTierPathGatingSpec extends AnyFlatSpec with Matchers {

  private val Matcher = ".github/actions/changed-paths/matches.sh"

  private lazy val mainYml = RepoFile.read(".github/workflows/main.yml")

  private def job(name: String): String = RepoFile.block(mainYml, name)

  private lazy val webFilter    = K8sTierPathGatingSpec.webFilter
  private lazy val workerFilter = K8sTierPathGatingSpec.workerFilter

  "the two filter sets" should "keep each tier's own sources to itself" in {
    webFilter should contain("web/**")
    webFilter should not contain "worker/**"
    workerFilter should contain("worker/**")
    workerFilter should not contain "web/**"
  }

  /**
   * The matcher is invoked as `"$GITHUB_ACTION_PATH/matches.sh"`, not `bash
   * matches.sh`, so the mode bit is part of the contract. A checkout of a
   * non-executable file fails the gate step on the runner and nothing deploys.
   */
  it should "be applied by a matcher the runner can execute" in {
    Files.isExecutable(Paths.get(Matcher)) shouldBe true
  }

  /**
   * THE INTERVAL THE GATE MEASURES OVER, which is the half that was wrong.
   *
   * Diffing a tier against the push's own `before` assumes the previous push
   * deployed. It often did not: the workflow-level `concurrency` cancels a
   * superseding push's predecessor mid-flight BY DESIGN, so a commit that changes
   * a tier followed within minutes by one that does not has its deploy cancelled
   * and then skipped — merged, stale, and nothing red. That shipped a
   * `/api/catalog` field on 2026-08-31 that production never served, and the apps
   * silently fell back to the old behaviour for it.
   *
   * Measuring from the last DEPLOYED commit is what makes an undeployed change
   * stay pending instead of being lost. Both halves are load-bearing: the base
   * has to come from the marker, and the marker has to be moved by the deploy.
   */
  "each tier's gate" should "diff from the commit that tier last deployed, not from the push's parent" in {
    val gate = job("gates")
    Seq("web", "worker").foreach { tier =>
      withClue(s"the $tier gate does not resolve a deployed base: ")(
        gate should include(s"id: base-$tier"))
      withClue(s"the $tier gate resolves a base it then does not use: ")(
        gate should include(s"base: $${{ steps.base-$tier.outputs.base }}"))
      withClue(s"the $tier gate asks for the wrong tier's marker: ")(
        gate should include(s"tier: $tier"))
      withClue(s"publish-$tier publishes on a gate other than its own: ")(
        job(s"publish-$tier") should include(s"if: needs.gates.outputs.$tier-changed == 'true'"))
    }
    gate should include("uses: ./.github/actions/deployed-base")
  }

  it should "be recorded only by a run whose build actually produced an image" in {
    // ⚠️ WHAT THIS MARKER MEANS CHANGED WHEN CI STOPPED DEPLOYING. It used to be
    // written after `rollout status` returned, so it recorded what was LIVE. CI no
    // longer rolls anything — image-automation commits the winning tag and
    // kustomize-controller applies it — so it now records that an image was BUILT
    // AND PUSHED for this commit, which is exactly what the gate above asks.
    //
    // The load-bearing half survives the change unaltered: the marker must move
    // only on success, and only after the step that published the image. A marker moved
    // regardless — `if: always()`, or ahead of the publish —
    // would advance the base past a commit no image exists for, and the next push
    // would diff from it and skip work that was never shipped. That is the
    // 2026-08-31 shape: merged, stale, and nothing red.
    Seq("web", "worker").foreach { tier =>
      val publish = job(s"publish-$tier")
      withClue(s"publish-$tier never moves its marker, so the base can never advance: ")(
        publish should include(s"refs/tags/deployed-$tier"))
      withClue(s"publish-$tier moves its marker even when the publish failed: ")(
        publish should not include "always()")
      withClue(s"publish-$tier moves its marker before the image is published: ")(
        publish.indexOf("docker buildx imagetools create") should be < publish.indexOf(s"refs/tags/deployed-$tier"))
      withClue(s"publish-$tier would move its marker on a branch or a dispatch: ")(
        publish should include("github.ref == 'refs/heads/main' && github.event_name == 'push'"))
    }
  }

  /**
   * AND CI MUST NOT DEPLOY. The ssh forced command that used to roll images is
   * retired: the pin lives in git, image-automation writes it, and
   * kustomize-controller applies it. If a roll ever reappears here it would fight
   * Flux for the image field — CI writing the commit SHA tag, Flux writing the
   * automation's tag, each reverting the other every reconcile and rolling every
   * pod in between. The endpoint itself survives in k8s-deploy.nix as break-glass
   * for when Flux is the thing that is broken; what must not come back is CI
   * reaching for it on every push.
   */
  it should "not deploy at all — Flux does that now" in {
    val workflow = RepoFile.read(".github/workflows/main.yml")
    withClue("CI is holding the cluster deploy key again: ")(
      workflow should not include "K8S_DEPLOY_SSH_KEY")
    withClue("CI is rolling images again, which fights image-automation: ")(
      workflow should not include "k8sdeploy@")
  }

  /**
   * And the fallback stays a fallback. An unknown marker must mean "diff from the
   * push's parent", never "everything changed" — the latter reads as a worker
   * input change on any run where the tag is briefly unreachable, and buys a
   * needless restart into a scrape storm.
   */
  it should "fall back to the push's parent, never to a full rebuild, when the marker is unusable" in {
    val action = RepoFile.read(".github/actions/deployed-base/action.yml")
    action should include("base=$BEFORE")
    action should include("merge-base --is-ancestor")
    action should not include "changed=true"
  }

  /**
   * Publishing and RECORDING share one job, which holds a `contents: write` token —
   * safe only while nothing in it can run code this repository did not vet. The
   * marker used to move from a `record-$tier` job of its own because `build-$tier`
   * ran sbt and a Docker build; the images are built elsewhere now and this job only
   * copies registry tags. A third-party action (docker/login-action et al.) or a
   * build creeping back in would put the ref-write token beside it again.
   */
  "each tier" should "publish and record from a job that runs no build and no third-party action" in {
    Seq("web", "worker").foreach { tier =>
      val uses = job(s"publish-$tier").linesIterator.map(_.trim.stripPrefix("- ")).filter(_.startsWith("uses:")).toSeq
      withClue(s"publish-$tier runs an action beside its ref-write token: ")(
        uses.filterNot(u => u == "uses: actions/checkout@v5" || u.startsWith("uses: ./.github/actions/")) shouldBe empty)
      withClue(s"publish-$tier builds again: ")(job(s"publish-$tier") should not include "sbt")
    }
    mainYml should not include "\n    record-"
  }

  /**
   * Separate concurrency lanes, because the two tiers are independent: one group
   * would make a web run queue behind a worker run for no reason, and GitHub keeps
   * only one running plus one pending per group.
   *
   * The lanes matter for a second reason now that these jobs only move a tag.
   * Concurrent writes to the same ref are last-write-wins, and the loser could be
   * the NEWER commit — walking the marker backwards and re-shipping work already
   * covered. `cancel-in-progress: false` on a lane of its own is what serialises
   * them.
   */
  it should "move its marker on a concurrency lane of its own" in {
    job("publish-web") should include("group: publish-web\n")
    job("publish-worker") should include("group: publish-worker\n")
  }

  /**
   * THREE TAGS, ALWAYS, each with a different job — and it used to be two. The SHA
   * tag is provenance, mapping a running pod back to a commit. The
   * `main-<utc>-<sha7>` tag is the one that DEPLOYS, because image automation
   * picks the newest by sorting and neither of the others sorts (that pairing is
   * guarded end-to-end by FluxImageAutomationSpec). `latest` survives only so a
   * hand-applied manifest resolves to something.
   *
   * There is no longer a deploy job to assert against: what pins production is the
   * `image:` line in the tier's base manifest, which image-automation writes.
   */
  it should "push all three tags, so a build can be traced and deployed" in {
    Seq("web", "worker").foreach { tier =>
      val build = job(s"publish-$tier")
      build should include(s"movies-$tier:$${{ github.sha }}")
      build should include(s"movies-$tier:latest")
      withClue(s"publish-$tier pushes no sortable tag, so nothing would ever deploy: ") {
        build should include(s"movies-$tier:$${{ steps.tag.outputs.value }}")
      }
      withClue(s"$tier's manifest is pinned to a moving tag: ") {
        RepoFile.read(s"infra/kubernetes/$tier/base/all.yaml") should not include ":latest"
      }
    }
  }

  /**
   * The two standalone workflows deployed to the cluster the moment their image
   * built — a push that failed ci still rolled a broken web image onto
   * kinowo.net. Folding them in here put them behind the same green build the Fly
   * deploy waits for. It is also what keeps them off the t=0 runner budget
   * (CiRunnerBudgetSpec), so dropping the `needs:` breaks two things at once.
   */
  "neither k3s publish" should "start before ci is green" in {
    job("publish-web") should include("needs: [ci, gates]")
    job("publish-worker") should include("needs: [ci, gates, image-worker]")
  }

  /** The worker image is built only for a push that changes the worker: `publish-worker` would
   *  publish nothing else, and the build held a runner for minutes beside ci on every push. */
  "the worker image build" should "run only when the worker's paths changed" in {
    job("image-worker") should include("if: needs.gates.outputs.worker-changed == 'true'")
  }

  /** The fold is only done once the workflows it replaced are gone. */
  "the workflows this replaced" should "no longer exist" in {
    Files.exists(Paths.get(".github/workflows/build-web-image.yaml")) shouldBe false
    Files.exists(Paths.get(".github/workflows/build-worker-image.yaml")) shouldBe false
    Files.exists(Paths.get(".github/workflows/deploy.yml")) shouldBe false
  }
}

/** The tiers' filter sets as `main.yml` hands them to the `changed-paths` action; the cases that run the matcher over them
 *  are in [[K8sTierPathGatingIntegrationSpec]]. */
object K8sTierPathGatingSpec extends Matchers {

  private lazy val mainYml = RepoFile.read(".github/workflows/main.yml")

  private def job(name: String): String = RepoFile.block(mainYml, name)

  /** The `patterns:` block a tier's gate (in `gates`, step `changed-<tier>`) hands to the
   *  `changed-paths` action. The gates run at t=0 so each tier's publish starts the moment ci is green. */
  def filterSet(tier: String): Vector[String] = {
    val lines = job("gates").linesIterator.toVector
    val step  = lines.indexWhere(_.trim == s"id: changed-$tier")
    withClue(s"`gates` has no `changed-$tier` gate step: ")(step should be >= 0)
    val start = lines.indexWhere(_.trim == "patterns: |", step)
    withClue(s"`changed-$tier` hands no `patterns: |` block to the changed-paths action: ")(start should be >= 0)
    val indent = lines(start).takeWhile(_ == ' ').length
    lines
      .drop(start + 1)
      .takeWhile(line => line.trim.isEmpty || line.takeWhile(_ == ' ').length > indent)
      .map(_.trim)
      .filter(_.nonEmpty)
  }

  lazy val webFilter    = filterSet("web")
  lazy val workerFilter = filterSet("worker")
}
