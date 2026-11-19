package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import WorkflowText.*

/**
 * A CHANGED BUILD'S DEPENDENCIES ARE FETCHED ONCE A RUN, NOT ONCE A JOB. setup-jdk restores the sbt dependency cache
 * under the build files' hash, falling back to main's newest entry, so a build change (a Scala Steward bump) missed the
 * exact key in every job of a run at once, and each job fetched the same new artifacts from Maven Central together.
 * Central answered with a 429 (Scala Steward PR #275, run 44250628374: `sentry-logback-8.65.0.pom`, one page-test row
 * of ~20) and, before the fallback key, 403s (run 36807612216).
 *
 * So where a changed build FIRST runs -- a pull request (Branch CI: every Scala Steward bump) and a push to main (Main) --
 * a workflow that can run more than one sbt job at once has every such job need (directly or through another job) a
 * WARMER: a job running scripts/ci/sbt-deps-resolve.sh, which resolves the changed build once and saves the exact key
 * first (ci.yml's `deps`). A workflow only ever
 * CALLED (`workflow_call`) may leave that to its callers: then every job calling it needs one.
 *
 * The rest (dispatches, schedules, `workflow_run`s, the landing check on a check/ branch) mostly run a build main has
 * already resolved, so they hit the exact key; where they do not (Main's convergence kick at t=0, a measure/ branch
 * of a bump) they still fetch the delta together. They are left without a warmer hop on purpose: their legs are held to
 * one runner hop (IdentityModelConvergenceWiringSpec) and to ci-runner's lanes (SelfHostedRunnersSpec).
 */
class SbtDependencyFanOutSpec extends AnyFlatSpec with Matchers {

  private val Resolve = "scripts/ci/sbt-deps-resolve.sh"

  /** A warmer: a job whose steps run [[Resolve]]. */
  private def isWarmer(job: String): Boolean = steps(job).exists(_.get("run").exists(_.trim == Resolve))

  private def code(path: String): String = RepoFile.withoutComments(RepoFile.read(path))

  /** A step that resolves sbt dependencies: setup-jdk or setup-java handed `cache: sbt`, or a composite action whose
   *  steps do. */
  private def stepResolves(step: Map[String, String], action: String => Boolean): Boolean =
    step.get("uses").map(_.trim).exists { uses =>
      if (uses == "./.github/actions/setup-jdk" || uses.startsWith("actions/setup-java@"))
        step.get("with").exists(_.linesIterator.exists(_.trim == "cache: sbt"))
      else uses.startsWith("./.github/actions/") && action(uses)
    }

  /** A composite action's top-level `runs:` block (an input may be called `runs` too: run-page-test's is). */
  private def topLevelRuns(action: String): String = {
    val lines = action.linesIterator.toVector
    val start = lines.indexWhere(_ == "runs:")
    withClue("a composite action without a top-level `runs:`: ") { start should be >= 0 }
    (lines(start) +: lines.drop(start + 1).takeWhile(l => l.trim.isEmpty || l.startsWith(" "))).mkString("\n")
  }

  private lazy val resolvingActions: Set[String] = {
    def resolves(uses: String, seen: Set[String]): Boolean = {
      val path = s"${uses.stripPrefix("./")}/action.yml"
      !seen(uses) && RepoFile.exists(path) &&
        steps(topLevelRuns(code(path))).exists(stepResolves(_, resolves(_, seen + uses)))
    }
    RepoFile.compositeActions().map(p => "./" + p.stripSuffix("/action.yml")).filter(resolves(_, Set.empty)).toSet
  }

  /** The local workflow a job calls, if it calls one. */
  private def called(job: String): Option[String] =
    keys(job).get("uses").map(_.trim).filter(_.startsWith("./.github/workflows/"))

  /** A job that resolves sbt dependencies with no warmer of its own ahead of it: its steps do, or it calls a workflow
   *  with such a job inside that the workflow does not warm itself (ci.yml does; country-convergence-leg.yml leaves it
   *  to its callers). */
  private def jobResolves(job: String): Boolean = called(job) match {
    case _ if isWarmer(job) => false
    case Some(workflow) =>
      val file = workflow.stripPrefix("./")
      val jobs = RepoFile.jobs(code(file))
      jobs.exists((name, j) => jobResolves(j) && !warmed(jobs, name))
    case None => steps(job).exists(stepResolves(_, resolvingActions))
  }

  /** How many runners a resolving job can hold at once: a matrix (or a call of a workflow that fans out) many. */
  private def runners(job: String): Int =
    if (keys(job).contains("strategy") || called(job).exists(w => fansOut(w.stripPrefix("./")))) 2 else 1

  private def resolving(file: String): Map[String, String] = RepoFile.jobs(code(file)).filter((_, j) => jobResolves(j))

  private def fansOut(file: String): Boolean = resolving(file).values.map(runners).sum > 1

  /** Whether `name` in `jobs` waits on a call of the warmer, directly or through the jobs it needs. */
  private def warmed(jobs: Map[String, String], name: String, seen: Set[String] = Set.empty): Boolean =
    jobs.get(name).exists { job =>
      isWarmer(job) || needs(job).exists(n => !seen(n) && warmed(jobs, n, seen + name))
    }

  private def onlyCalled(file: String): Boolean =
    RepoFile.block(code(file), "on").linesIterator.drop(1).map(_.trim).filter(_.endsWith(":"))
      .forall(_ == "workflow_call:")

  /** Every job, in any workflow, calling `file`, as (calling workflow, job name). */
  private def callers(file: String): Seq[(String, String)] = for {
    w         <- RepoFile.workflows().map(_.getPath)
    (name, j) <- RepoFile.jobs(code(w)).toSeq
    if called(j).contains(s"./$file")
  } yield w -> name

  /** The jobs of `file` that resolve sbt dependencies alongside others with no warmer ahead of them. */
  private def unwarmed(file: String): Seq[String] =
    if (!fansOut(file)) Nil
    else {
      val jobs   = RepoFile.jobs(code(file))
      val byCall = onlyCalled(file) && callers(file).nonEmpty &&
        callers(file).forall((w, name) => warmed(RepoFile.jobs(code(w)), name))
      if (byCall) Nil
      else resolving(file).keys.toSeq.sorted.filterNot(warmed(jobs, _)).map(n => s"$file: $n")
    }

  /** The workflows a pull request or a push to main starts, and every workflow they call. */
  private lazy val firstToRun: Seq[String] = {
    def starts(file: String): Boolean = {
      val on = RepoFile.block(code(file), "on")
      on.linesIterator.exists(_.trim == "pull_request:") ||
        (on.linesIterator.exists(_.trim == "push:") && RepoFile.block(on, "push").contains("branches: [main]"))
    }
    def withCalled(file: String): Seq[String] =
      file +: RepoFile.jobs(code(file)).values.flatMap(called).map(_.stripPrefix("./"))
        .flatMap(withCalled).toSeq
    RepoFile.workflows().map(_.getPath).filter(starts).flatMap(withCalled).distinct
  }

  "every workflow a changed build first runs in" should "resolve its dependencies once, before its sbt jobs" in {
    val files = firstToRun
    files should contain allOf (".github/workflows/main.yml", ".github/workflows/branch-ci.yml", ".github/workflows/ci.yml")
    // ci.yml's 18 sbt rows: the rule has to see them to mean anything.
    files.filter(fansOut) should contain(".github/workflows/ci.yml")
    withClue(s"these sbt jobs run beside others with no `needs` on a job running $Resolve (direct or transitive): ") {
      files.flatMap(unwarmed) shouldBe empty
    }
  }

  it should "see the dependency cache in every composite action that sets an sbt job up" in {
    resolvingActions should contain allOf ("./.github/actions/setup-sbt-build", "./.github/actions/run-page-test",
      "./.github/actions/convergence-setup")
    resolvingActions should not contain "./.github/actions/setup-jdk"
  }

  "ci.yml's warmer" should "save under the very key and paths setup-jdk restores, only when GitHub has none" in {
    val warmer = RepoFile.jobs(code(".github/workflows/ci.yml"))("deps")
    isWarmer(warmer) shouldBe true
    val jdk = code(".github/actions/setup-jdk/action.yml")
    for (text <- Seq(warmer, jdk)) {
      text should include("uses: ./.github/actions/sbt-deps-key")
      text should include("key: ${{ steps.sbt-deps-key.outputs.key }}")
      text should include("path: ${{ steps.sbt-deps-key.outputs.paths }}")
    }
    warmer should include("lookup-only: 'true'")
    val save = steps(warmer).find(_.get("uses").exists(_.trim.startsWith("actions/cache/save@")))
    // On main too (Main #2678): setup-jdk's own save is the post step of an `actions/cache` inside a composite action,
    // whose `path` -- a step output of that action -- is gone by then ("Input required and not supplied: path"), so it
    // saved nothing; the rows started 3 s after the warmer each restored the older fallback and fetched from Central.
    withClue("the warmer saves the exact key itself on a miss, on every ref, main included: ") {
      val condition = save.flatMap(_.get("if")).getOrElse("")
      condition should include("steps.lookup.outputs.cache-hit != 'true'")
      condition should not include "refs/heads/main"
    }
  }
}
