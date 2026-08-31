package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}
import scala.sys.process.*

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
 */
class K8sTierPathGatingSpec extends AnyFlatSpec with Matchers {

  private val Matcher = ".github/actions/changed-paths/matches.sh"

  private lazy val mainYml = RepoFile.read(".github/workflows/main.yml")

  private def job(name: String): String = RepoFile.block(mainYml, name)

  /** The `patterns:` block a tier's build job hands to the `changed-paths` action. */
  private def filterSet(jobName: String): Vector[String] = {
    val lines = job(jobName).linesIterator.toVector
    val start = lines.indexWhere(_.trim == "patterns: |")
    withClue(s"`$jobName` hands no `patterns: |` block to the changed-paths action: ")(start should be >= 0)
    val indent = lines(start).takeWhile(_ == ' ').length
    lines
      .drop(start + 1)
      .takeWhile(line => line.trim.isEmpty || line.takeWhile(_ == ' ').length > indent)
      .map(_.trim)
      .filter(_.nonEmpty)
  }

  private lazy val webFilter    = filterSet("build-web")
  private lazy val workerFilter = filterSet("build-worker")

  /** The action's own matcher, over a fabricated push. `true` = this tier rebuilds and redeploys. */
  private def matches(patterns: Seq[String], changed: Seq[String]): Boolean = {
    val patternsFile = Files.createTempFile("changed-paths-patterns", ".txt")
    val changedFile  = Files.createTempFile("changed-paths-files", ".txt")
    try {
      Files.writeString(patternsFile, patterns.mkString("", "\n", "\n"))
      Files.writeString(changedFile, changed.mkString("", "\n", "\n"))
      (Seq("bash", Matcher, patternsFile.toString) #< changedFile.toFile).!!.trim match {
        case "true"  => true
        case "false" => false
        case other   => fail(s"$Matcher printed `$other`, which is neither true nor false")
      }
    } finally {
      Files.deleteIfExists(patternsFile)
      Files.deleteIfExists(changedFile)
    }
  }

  /** A push that touches the web tier and nothing else — the everyday case. */
  private val WebOnlyPush = Seq(
    "web/src/main/scala/controllers/RepertoireController.scala",
    "web/src/main/twirl/views/repertoire.scala.html",
    "web/src/test/resources/expected-poznan.html",
  )

  private val WorkerOnlyPush = Seq(
    "worker/src/main/scala/services/scrape/ScrapeReaper.scala",
    "worker/src/test/scala/deploy/K8sTierPathGatingSpec.scala",
  )

  "a web-only push" should "not match the worker's filter set, so nothing rebuilds or restarts the worker" in {
    withClue(s"worker filter $workerFilter matched $WebOnlyPush: ") {
      matches(workerFilter, WebOnlyPush) shouldBe false
    }
  }

  it should "still match the web's own filter set, or the web tier would stop deploying" in {
    matches(webFilter, WebOnlyPush) shouldBe true
  }

  "a worker-only push" should "not match the web's filter set" in {
    matches(webFilter, WorkerOnlyPush) shouldBe false
  }

  it should "match the worker's own filter set" in {
    matches(workerFilter, WorkerOnlyPush) shouldBe true
  }

  /**
   * The shared inputs are shared on purpose: `common/`, the build definition and
   * the image recipe are compiled into both artifacts, so a change to any of them
   * genuinely does change the worker's deployable artifact.
   */
  it should "still redeploy both tiers when a genuinely shared input moves" in {
    val shared = Seq("common/src/main/scala/models/Movie.scala")
    matches(webFilter, shared) shouldBe true
    matches(workerFilter, shared) shouldBe true
  }

  /**
   * And the two lists must not converge. The web sources pattern appearing in the
   * worker's set is the one edit that would make every web push restart the worker
   * while every assertion above still passed on the day it was made — the
   * fabricated pushes are a sample, this is the invariant.
   */
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
    Seq("web", "worker").foreach { tier =>
      val build = job(s"build-$tier")
      withClue(s"build-$tier does not resolve a deployed base: ")(
        build should include("uses: ./.github/actions/deployed-base"))
      withClue(s"build-$tier resolves a base it then does not use: ")(
        build should include("base: ${{ steps.base.outputs.base }}"))
      withClue(s"build-$tier asks for the wrong tier's marker: ")(
        build should include(s"tier: $tier"))
    }
  }

  it should "be recorded only by a deploy that actually rolled" in {
    Seq("web", "worker").foreach { tier =>
      val deploy = job(s"deploy-$tier")
      withClue(s"deploy-$tier never moves its marker, so the base can never advance: ")(
        deploy should include(s"refs/tags/deployed-$tier"))
      // The endpoint waits on `rollout status`, so a marker written in a LATER step
      // than the roll is a record of what is live. One written before it — or in a
      // step with `if: always()` — would record an attempt.
      val rollAt   = deploy.indexOf("Roll the new image onto k3s")
      val recordAt = deploy.indexOf(s"Record what the $tier tier is now running")
      withClue(s"deploy-$tier records its marker before it rolls: ")(recordAt should be > rollAt)
      withClue(s"deploy-$tier records its marker even when the roll failed: ")(
        deploy.substring(recordAt) should not include "if: always()")
    }
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
   * Building and DEPLOYING stay separate jobs: a failed build must not be able to
   * roll an image out, and a failed deploy has to be distinguishable from a failed
   * build in the run list — the two have completely different fixes. Both were
   * called out explicitly in the workflows this replaced.
   */
  "each tier" should "deploy from a job that needs its build" in {
    job("deploy-web") should include("needs: build-web")
    job("deploy-worker") should include("needs: build-worker")
  }

  /**
   * …and each deploy job reads ITS OWN tier's answer. Crossing these over is the
   * other one-word edit that reinstates the bug.
   */
  it should "gate its deploy on its own tier's changed-paths answer" in {
    job("deploy-web") should include("needs.build-web.outputs.changed == 'true'")
    job("deploy-worker") should include("needs.build-worker.outputs.changed == 'true'")
  }

  /**
   * Separate concurrency groups, because the two tiers are independent rollouts:
   * one group would make a web deploy queue behind a worker deploy for no reason,
   * and GitHub keeps only one running plus one pending per group.
   */
  it should "roll out on a concurrency lane of its own" in {
    job("deploy-web") should include("group: deploy-web-k8s")
    job("deploy-worker") should include("group: deploy-worker-k8s")
  }

  /**
   * BOTH TAGS, ALWAYS. The SHA tag is what a deploy pins, so a restarted pod comes
   * back on the exact build that was deployed; `latest` exists only so a
   * hand-applied manifest resolves to something.
   */
  it should "push the SHA tag a deploy pins, alongside latest" in {
    Seq("web", "worker").foreach { tier =>
      val build = job(s"build-$tier")
      build should include(s"movies-$tier:$${{ github.sha }}")
      build should include(s"movies-$tier:latest")
      job(s"deploy-$tier") should include(s"movies-$tier:$${{ github.sha }}")
      withClue(s"deploy-$tier rolls out `latest`, which nothing records: ") {
        job(s"deploy-$tier") should not include s"movies-$tier:latest"
      }
    }
  }

  /**
   * The two standalone workflows deployed to the cluster the moment their image
   * built — a push that failed ci still rolled a broken web image onto
   * kinowo.net. Folding them in here put them behind the same green build the Fly
   * legs wait for. It is also what keeps them off the t=0 runner budget
   * (CiRunnerBudgetSpec), so dropping the `needs:` breaks two things at once.
   */
  "neither k3s build" should "start before ci is green" in {
    job("build-web") should include("needs: ci")
    job("build-worker") should include("needs: ci")
  }

  /** The fold is only done once the workflows it replaced are gone. */
  "the workflows this replaced" should "no longer exist" in {
    Files.exists(Paths.get(".github/workflows/build-web-image.yaml")) shouldBe false
    Files.exists(Paths.get(".github/workflows/build-worker-image.yaml")) shouldBe false
    Files.exists(Paths.get(".github/workflows/deploy.yml")) shouldBe false
  }
}
