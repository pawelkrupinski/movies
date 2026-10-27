package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The "Kinowo Sync Bot" App and pull requests are github.com's alone. On Forgejo the App's secrets do not exist and
 * the job image's gh shim refuses `gh pr`, and movies takes no PRs: Discover UK cinemas (run 2893) and Identity
 * capture (run 2915) went red on 2026-10-26 asking Forgejo for SYNC_PR_APP_ID. So a bot publishes its branch on
 * Forgejo with the job's own token and names it to the owner in an issue (.github/actions/publish-bot-branch,
 * scripts/ci/bot-branch-ready.sh), and keeps its App and PR steps for github.com:
 *
 *  - every step that reads the App's secrets, mints its token, or opens a PR runs on github.com only (`if:` asks
 *    `github.server_url == 'https://github.com'`), and one that hands the App's token on falls back to the job's;
 *  - every job that opens a PR on github.com also publishes on Forgejo.
 */
class BotBranchOnForgejoSpec extends AnyFlatSpec with Matchers {
  private val OnGitHub  = "github.server_url == 'https://github.com'"
  private val OnForgejo = "github.server_url != 'https://github.com'"

  /** The parts of a step that make it GitHub-only: the App, its secrets, a PR. */
  private val GitHubOnly = Seq("secrets.SYNC_PR_APP_", "uses: actions/create-github-app-token@",
    "uses: peter-evans/create-pull-request@", "gh pr create", "gh pr edit")

  private val OpensPr     = Seq("uses: peter-evans/create-pull-request@", "gh pr create")
  private val PublishesFj = Seq("uses: ./.github/actions/publish-bot-branch", "scripts/ci/bot-branch-ready.sh")

  /** Each `- ` item under the job's `steps:`, comment lines dropped. */
  private def steps(job: String): Seq[String] = {
    val lines  = RepoFile.withoutComments(job).linesIterator.toVector.dropWhile(_.trim != "steps:").drop(1)
    val indent = lines.find(_.trim.startsWith("- ")).map(_.takeWhile(_ == ' ').length).getOrElse(0)
    lines.foldLeft(Vector.empty[Vector[String]]) { (acc, line) =>
      if (line.takeWhile(_ == ' ').length == indent && line.trim.startsWith("- ")) acc :+ Vector(line)
      else if (acc.isEmpty) acc
      else acc.init :+ (acc.last :+ line)
    }.map(_.mkString("\n"))
  }

  private def ifOf(step: String): String =
    step.linesIterator.map(_.trim.stripPrefix("- ")).collectFirst { case s"if: $cond" => cond }.getOrElse("")

  /** (workflow:job, its steps) for every workflow job. */
  private lazy val allJobs: Seq[(String, Seq[String])] = for {
    file        <- RepoFile.workflows()
    (name, job) <- RepoFile.jobs(RepoFile.read(file.getPath)).toSeq.sortBy(_._1)
  } yield s"${file.getName}:$name" -> steps(job)

  "a step needing the Sync Bot App or a PR" should "run on github.com only" in {
    val offending = for {
      (job, jobSteps) <- allJobs
      step            <- jobSteps
      if GitHubOnly.exists(step.contains) && !ifOf(step).contains(OnGitHub)
    } yield s"$job: ${step.linesIterator.next().trim}"
    offending shouldBe empty
  }

  "a step that may run on Forgejo" should "fall back to the job's token when it takes the App's" in {
    val offending = for {
      (job, jobSteps) <- allJobs
      step            <- jobSteps
      if step.contains("steps.app-token.outputs.token") && !ifOf(step).contains(OnGitHub) &&
        !step.contains("steps.app-token.outputs.token || github.token")
    } yield s"$job: ${step.linesIterator.next().trim}"
    offending shouldBe empty
  }

  "every job that opens a PR on github.com" should "publish its branch on Forgejo too" in {
    val prJobs = allJobs.filter { case (_, jobSteps) => jobSteps.exists(s => OpensPr.exists(s.contains)) }
    prJobs.map(_._1) should contain allOf ("discover-cinemas.yml:discover-cinemas", "identity-capture.yml:publish")
    val offending = prJobs.collect {
      case (job, jobSteps)
          if !jobSteps.exists(s => PublishesFj.exists(s.contains) && ifOf(s).contains(OnForgejo)) => job
    }
    offending shouldBe empty
  }

  it should "drop its old branch first, on either forge, so the branch is always a fresh one off main" in {
    val Drop = "uses: ./.github/actions/drop-bot-branch"
    val offending = for {
      (job, jobSteps) <- allJobs
      at              = jobSteps.indexWhere(_.contains("uses: ./.github/actions/publish-bot-branch"))
      if at >= 0
      drop            = jobSteps.indexWhere(_.contains(Drop))
      if drop < 0 || drop > at || ifOf(jobSteps(drop)).contains(OnGitHub)
    } yield job
    offending shouldBe empty
  }

  "the Forgejo publish" should "commit, push the branch and name it to the owner" in {
    val action = RepoFile.read(".github/actions/publish-bot-branch/action.yml")
    action should include("""git push origin "HEAD:refs/heads/$BRANCH"""")
    action should not include "--force"
    action should include("scripts/ci/bot-branch-ready.sh")
    withClue("bot-branch-ready-test.sh is run by CI's script-test runner: ")(
      ScriptTests.runInCi("scripts/ci/bot-branch-ready-test.sh") shouldBe true)
  }
}
