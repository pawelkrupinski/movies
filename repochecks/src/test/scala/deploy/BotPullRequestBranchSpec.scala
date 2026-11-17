package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A bot PR opened by create-pull-request lives on a FIXED branch, which it rebuilds on top of
 * `main` and force-pushes. When the branch outlives its PR -- merged by fast-forwarding `main`,
 * which GitHub counts as merged but never deletes -- that force-push rewinds weeks of `main`,
 * and if those weeks touched `.github/workflows/` GitHub refuses the push from an App without
 * the `workflows` permission. The OG-card refresh stopped landing that way on 2026-09-25. So
 * every such job drops its branch first (`.github/actions/drop-bot-branch`) and the push is
 * always a fresh branch off `main`.
 *
 * "Off `main`" means `main`'s tip when the PR job runs, not the commit the run was triggered
 * on: `actions/checkout` defaults to the triggering SHA, and a run long enough for `main` to
 * gain a workflow change meanwhile builds a branch that reverts it -- the same refusal. The
 * OG-card refresh hit that on 2026-10-09 (two `ci.yml` commits landed during its 35 minutes).
 */
class BotPullRequestBranchSpec extends AnyFlatSpec with Matchers {
  private val CreatePr   = "uses: peter-evans/create-pull-request@"
  private val DropBranch = "uses: ./.github/actions/drop-bot-branch"
  private val Branch     = """^\s*branch:\s*(\S+)\s*$""".r

  /** (workflow, job, job text) for every job that opens a PR with create-pull-request. */
  private lazy val prJobs: Seq[(String, String, String)] = for {
    file       <- RepoFile.workflows()
    yml         = RepoFile.read(file.getPath)
    if yml.contains(CreatePr)
    (name, job) <- RepoFile.jobs(yml).toSeq.sortBy(_._1)
    if job.contains(CreatePr)
  } yield (file.getName, name, job)

  private val Checkout = "uses: actions/checkout@"
  private val RefMain  = """^\s*ref:\s*main\s*$""".r

  /** Whether the job's first checkout pins `ref: main` in its own `with:` block. */
  private def checksOutMainTip(job: String): Boolean =
    job.linesIterator.dropWhile(!_.contains(Checkout)).drop(1)
      .takeWhile(line => !line.trim.startsWith("- ")).exists(RefMain.matches)

  /** The `branch:` value of the first `with:` block after `marker` in `job`. */
  private def branchAfter(job: String, marker: String): Option[String] =
    job.linesIterator.dropWhile(!_.contains(marker)).collectFirst { case Branch(branch) => branch }

  "every job that opens a bot PR" should "drop its fixed branch before rebuilding it" in {
    prJobs should not be empty
    val offending = prJobs.collect {
      case (file, name, job)
          if !job.contains(DropBranch) ||
            job.indexOf(DropBranch) > job.indexOf(CreatePr) ||
            branchAfter(job, DropBranch) != branchAfter(job, CreatePr) =>
        s"$file:$name"
    }
    offending shouldBe empty
  }

  it should "build that branch on main's current tip, not the run's triggering commit" in {
    val offending = prJobs.collect { case (file, name, job) if !checksOutMainTip(job) => s"$file:$name" }
    offending shouldBe empty
  }
}
