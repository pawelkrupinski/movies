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
}
