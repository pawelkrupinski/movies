package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Every job records its runner's resources from its start to its end (the owner, 2026-11-22: "add cpu usage stats to ci
 * jobs"): .github/actions/job-telemetry's `start` right after the job's checkout (machine-id.sh's CPU model and cores,
 * the resource sampler -- cgroup-sampler.sh's /proc series on Linux, darwin_sampler.py on macOS), its
 * `finish` as the job's last step (the series uploaded as the job's `ci-metrics-*` artifact). Report run timings
 * (scripts/ci/ci_timings.py) reads each job's artifact after the run and pushes its CPU beside its time
 * (`kinowo_ci_job_cpu_busy_ratio`, `kinowo_ci_job_cpu_seconds`, `kinowo_ci_job_cores`) to the ci-builds board. A job
 * without the two steps is charted for its time and silently not for its CPU: 102 of 129 jobs were, before this spec.
 *
 * The action and the script it runs come from the job's own checkout, so a sparse checkout must include both. A job
 * that checks out nothing cannot run it: [[NoCheckout]] names each, with why it needs no checkout.
 */
class JobTelemetryCoverageSpec extends AnyFlatSpec with Matchers {
  private val Action    = ".github/actions/job-telemetry/action.yml"
  private val Telemetry = "uses: ./" + Action.stripSuffix("/action.yml")
  /** What the job's checkout must hold for the action to run: the action and the scripts it calls. */
  private lazy val needed: Seq[String] =
    Action.stripSuffix("/action.yml") +: """\$GITHUB_WORKSPACE/([\w/.-]+)/[\w.-]+""".r
      .findAllMatchIn(RepoFile.read(Action)).map(_.group(1)).toSeq.distinct

  /** Jobs that check out nothing: a few `gh` calls or a dispatch, seconds long, no CPU worth charting. */
  private val NoCheckout: Map[(String, String), String] = Map(
    (".github/workflows/convergence-bisect.yml", "tell")                      -> "one `gh issue comment` on the bisect's verdict",
    (".github/workflows/convergence-fill.yml", "countries")                   -> "echoes the input's country list as a matrix",
    (".github/workflows/identity-model-convergence.yml", "request-bisect")    -> "one `gh workflow run` of the bisect",
    (".github/workflows/identity-model-convergence.yml", "timings")           -> "one `gh workflow run` of Report run timings",
    (".github/workflows/identity-takes.yml", "timings")                       -> "one `gh workflow run` of Report run timings"
  )

  private case class Job(path: String, name: String, body: String) {
    val code: String          = RepoFile.withoutComments(body)
    val reusable: Boolean     = code.linesIterator.exists(_.matches("""^\s{2,8}uses: \./\.github/workflows/.*"""))
    // a convergence row fetches its commit by hand (country-convergence-leg.yml) and runs the action all the same
    val checksOut: Boolean    = code.contains("uses: actions/checkout@") || code.contains(Telemetry)
    /** The job's steps, each as its text: a step starts at a `- ` line at the steps' own indentation. */
    lazy val steps: Seq[String] = {
      val lines = code.linesIterator.toVector
      val at    = lines.indexWhere(_.trim == "steps:")
      if (at < 0) Seq.empty
      else {
        val own   = lines(at).takeWhile(_ == ' ').length
        val body  = lines.drop(at + 1).takeWhile(l => l.trim.isEmpty || l.takeWhile(_ == ' ').length > own)
        val item  = body.find(_.trim.startsWith("- ")).map(_.takeWhile(_ == ' ').length).getOrElse(0)
        body.foldLeft(Vector.empty[Vector[String]]) { (acc, line) =>
          if (line.takeWhile(_ == ' ').length == item && line.trim.startsWith("- ")) acc :+ Vector(line)
          else if (acc.isEmpty) acc
          else acc.init :+ (acc.last :+ line)
        }.map(_.mkString("\n"))
      }
    }
  }

  private lazy val jobs: Seq[Job] =
    RepoFile.workflows().map(_.getPath).sorted.flatMap { path =>
      RepoFile.jobs(RepoFile.read(path)).toSeq.sortBy(_._1).map { case (name, body) => Job(path, name, body) }
    }.filterNot(_.reusable)

  private def telemetry(step: String, phase: String) = step.contains(Telemetry) && step.contains(s"phase: $phase")

  "the workflows" should "be read, or this spec checks nothing" in {
    needed.size should be >= 2 // the action and the scripts it runs
    jobs.size should be > 100
    jobs.map(j => j.path -> j.name) should contain(".github/workflows/main.yml" -> "gates")
    jobs.find(_.name == "gates").get.steps.size should be > 3
  }

  "every job that checks out the repository" should "start the job telemetry once it is checked out" in {
    val problems = jobs.filter(_.checksOut).flatMap { job =>
      val checkout = job.steps.indexWhere(_.contains("uses: actions/checkout@"))
      val start    = job.steps.indexWhere(telemetry(_, "start"))
      // ci.yml's `deps` starts it only once its cache lookup missed and it checked out the whole tree
      if (start < 0) Some(s"${job.path} ${job.name}: no job-telemetry `start`: its CPU is charted nowhere")
      else Option.when(start < checkout)(s"${job.path} ${job.name}: job-telemetry `start` before the checkout holds it")
    }
    problems shouldBe empty
  }

  it should "finish it as its last step, unless the job is cancelled" in {
    val problems = jobs.filter(_.checksOut).flatMap { job =>
      val finish = job.steps.lastIndexWhere(telemetry(_, "finish"))
      // a leg's verdict fails the job on purpose AFTER its metrics are out
      val after = job.steps.drop(finish + 1).filterNot(_.contains("uses: ./.github/actions/leg-verdict"))
      if (finish < 0) Seq(s"${job.path} ${job.name}: no job-telemetry `finish`: the series is never uploaded")
      else Seq(
        Option.when(after.nonEmpty)(s"${job.path} ${job.name}: steps after the job-telemetry `finish` are not measured"),
        // `!cancelled()`, maybe with more: `deps` only when it started, a page-test row only when its checkout holds the script
        Option.when(!job.steps(finish).contains("if: ${{ !cancelled()"))(s"${job.path} ${job.name}: `finish` not `if: $${{ !cancelled() ... }}`"),
        Option.when(!job.steps(finish).contains("job-status: ${{ job.status }}"))(s"${job.path} ${job.name}: `finish` without the job's status")
      ).flatten
    }
    problems shouldBe empty
  }

  it should "check out the action and the scripts it runs when its checkout is sparse" in {
    val problems = jobs.flatMap(job => job.steps.find(_.contains("uses: actions/checkout@")).map(job -> _)).flatMap { case (job, checkout) =>
      // cone mode: a listed directory brings everything under it
      val listed  = checkout.linesIterator.map(_.trim).toSeq
      val missing = needed.filterNot(path => listed.exists(dir => path == dir || path.startsWith(dir + "/")))
      Option.when(checkout.contains("sparse-checkout") && missing.nonEmpty)(
        s"${job.path} ${job.name}: a sparse checkout without ${missing.mkString(" and ")}")
    }
    problems shouldBe empty
  }

  "a job that checks out nothing" should "be named, with why it needs no telemetry" in {
    val bare = jobs.filterNot(_.checksOut).map(j => j.path -> j.name).toSet
    (bare -- NoCheckout.keySet) shouldBe empty
    (NoCheckout.keySet -- bare) shouldBe empty // a stale row: that job checks out now, or is gone
  }
}
