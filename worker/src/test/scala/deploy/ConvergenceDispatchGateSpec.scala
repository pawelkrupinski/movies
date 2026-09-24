package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}
import scala.sys.process.*

/**
 * Main dispatches a convergence suite only for a push that can change its verdict.
 *
 * Each suite's lane keeps ONE run pending and every newer dispatch replaces it, so a push
 * that touched only the iOS app used to evict the pending run of the pipeline commit before
 * it — 239 of 398 `Country convergence` runs and 253 of 388 `US convergence` runs ended
 * cancelled between 2026-08-31 and 09-24, ~78% of them without reaching a runner, while
 * only 44% of main's commits touched a pipeline path. This runs the real gate script over a
 * scratch repository with a stub `gh` that records what was dispatched.
 */
class ConvergenceDispatchGateSpec extends AnyFlatSpec with Matchers {

  private val Script = Paths.get(".github/scripts/kick-convergence.sh").toAbsolutePath

  /** Runs the gate for both suites; `lastRun` is the head SHA each suite's newest run
   *  reports ("" for a suite that has never run). Returns what was dispatched. */
  private def kick(repo: ScratchGitRepository, head: String, lastRun: String): Seq[String] = {
    val (status, dispatched) = kickWithStatus(repo, head, lastRun)
    withClue("the gate's exit status: ")(status shouldBe 0)
    dispatched
  }

  /** The gate's exit status and what it dispatched; `refuse` names a suite whose
   *  `gh workflow run` fails. */
  private def kickWithStatus(repo: ScratchGitRepository, head: String, lastRun: String,
                             refuse: String = ""): (Int, Seq[String]) = {
    val dispatched = Files.createTempFile("dispatched", ".log")
    val gh = repo.script("gh",
      s"""case "$$1 $$2" in
         |  "run list")      echo "$lastRun" ;;
         |  "workflow run")  [ "$$3" = "$refuse" ] && { echo "HTTP 403" >&2; exit 1; }
         |                   echo "$$3" >> "$dispatched" ;;
         |esac
         |""".stripMargin)
    val status = Process(Seq("bash", Script.toString, head, "main", "Country convergence", "US convergence"),
      repo.root.toFile, "PATH" -> s"${gh.getParent}:${sys.env.getOrElse("PATH", "")}").!(ProcessLogger(_ => ()))
    (status, Files.readString(dispatched).linesIterator.toSeq)
  }

  private def repoWithBase(): (ScratchGitRepository, String) = {
    val repo = new ScratchGitRepository
    (repo, repo.commit("base", "worker/src/main/Pipeline.scala" -> "v1\n"))
  }

  "the convergence dispatch gate" should "leave the pending run alone for a push that cannot change the verdict" in {
    val (repo, base) = repoWithBase()
    repo.commit("ios: tweak", "ios/App.swift" -> "// app\n")
    val head = repo.commit("docs", "docs/note.md" -> "words\n")

    kick(repo, head, lastRun = base) shouldBe empty
  }

  it should "dispatch both suites for a push that touches the pipeline" in {
    val (repo, base) = repoWithBase()
    val head = repo.commit("pipeline", "worker/src/main/Pipeline.scala" -> "v2\n")

    kick(repo, head, lastRun = base) shouldBe Seq("Country convergence", "US convergence")
  }

  // e2e depends on web: the legs project and read their schedules through the web's own
  // read model and `MovieControllerService`, so a web-only change can move a verdict.
  it should "dispatch for a push that touches the web code the legs read their schedules through" in {
    val (repo, base) = repoWithBase()
    val head = repo.commit("web", "web/src/main/scala/controllers/MovieControllerService.scala" -> "// v2\n")

    kick(repo, head, lastRun = base) should have size 2
  }

  // The base is the suite's last run, not the push's parent: Main's own lane cancels
  // superseded pushes, so a pipeline commit can sit in an EARLIER push whose Main run never
  // reached the dispatch — and a diff from this push's parent would never see it.
  it should "see a pipeline commit from an earlier push that never dispatched" in {
    val (repo, base) = repoWithBase()
    repo.commit("pipeline", "e2e/src/test/scala/Spec.scala" -> "// new\n")
    val head = repo.commit("docs", "docs/note.md" -> "words\n")

    kick(repo, head, lastRun = base) should have size 2
  }

  it should "dispatch when there is no earlier run to diff against" in {
    val (repo, _) = repoWithBase()
    val head = repo.commit("docs", "docs/note.md" -> "words\n")

    kick(repo, head, lastRun = "") should have size 2
    kick(repo, head, lastRun = "0123456789abcdef0123456789abcdef01234567") should have size 2
  }

  it should "fail when a dispatch fails, after still trying the other suite" in {
    val (repo, base) = repoWithBase()
    val head = repo.commit("pipeline", "worker/src/main/Pipeline.scala" -> "v2\n")

    kickWithStatus(repo, head, lastRun = base, refuse = "Country convergence") shouldBe (1, Seq("US convergence"))
  }

  it should "dispatch when it cannot tell what changed, rather than read a failed diff as nothing" in {
    val (repo, base) = repoWithBase()

    kickWithStatus(repo, "0123456789abcdef0123456789abcdef01234567", lastRun = base) shouldBe
      (0, Seq("Country convergence", "US convergence"))
  }

  "Main" should "dispatch the convergence suites through the gate, not unconditionally" in {
    val kickJob = RepoFile.block(RepoFile.read(".github/workflows/main.yml"), "kick-convergence")
    kickJob should include(".github/scripts/kick-convergence.sh")
    kickJob should include("fetch-depth: 0")
    kickJob.linesIterator.filterNot(_.trim.startsWith("#")).mkString("\n") should not include "gh workflow run"
  }
}
