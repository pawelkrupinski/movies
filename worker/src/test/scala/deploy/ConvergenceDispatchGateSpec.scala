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
    val dispatched = Files.createTempFile("dispatched", ".log")
    val gh = repo.script("gh",
      s"""case "$$1 $$2" in
         |  "run list")      echo "$lastRun" ;;
         |  "workflow run")  echo "$$3" >> "$dispatched" ;;
         |esac
         |""".stripMargin)
    Process(Seq("bash", Script.toString, head, "main", "Country convergence", "US convergence"),
      repo.root.toFile, "PATH" -> s"${gh.getParent}:${sys.env.getOrElse("PATH", "")}").!!
    Files.readString(dispatched).linesIterator.toSeq
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

  "Main" should "dispatch the convergence suites through the gate, not unconditionally" in {
    val kickJob = RepoFile.block(RepoFile.read(".github/workflows/main.yml"), "kick-convergence")
    kickJob should include(".github/scripts/kick-convergence.sh")
    kickJob should include("fetch-depth: 0")
    kickJob.linesIterator.filterNot(_.trim.startsWith("#")).mkString("\n") should not include "gh workflow run"
  }
}
