package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path, Paths}
import scala.sys.process.*

/**
 * The auto-bisect a red HERMETIC convergence leg triggers, run for real against a scratch
 * repository: real `git bisect`, the real pipeline-path list, a step command standing in
 * for the sample leg.
 *
 * What it has to get right, each of which a wrong answer would make worse than no answer:
 *  - it names the first bad commit when the caps allow, and a RANGE — never a guess — when
 *    they do not;
 *  - it never spends a replay on a commit that cannot change the verdict (docs, the apps);
 *  - it spends nothing at all when the answer is already known: the same SHA was green
 *    before (a flake or a recording change), or no pipeline commit is in range;
 *  - it refuses to bisect a full-leg failure the sample cannot reproduce.
 */
class ConvergenceBisectSpec extends AnyFlatSpec with Matchers {

  private val Script     = Paths.get(".github/scripts/convergence-bisect.sh").toAbsolutePath
  private val PathsFile  = Paths.get(".github/convergence-paths.txt").toAbsolutePath

  /** A main line of six pipeline commits with two app-only commits between them; the bug
   *  lands in pipeline commit number `badAt` (1-based) and stays. The step cannot test the
   *  pipeline commits numbered in `untestable` (exit 125, bisect's "skip"). `redEverywhere`
   *  is a recording that fails the sample whatever the code, the base included. */
  private final class History(badAt: Int, untestable: Set[Int] = Set.empty, redEverywhere: Boolean = false) {
    val repo = new ScratchGitRepository
    val good: String = repo.commit("base", "worker/src/main/State.scala" -> "ok\n")
    private val pipeline = (1 to 6).map { n =>
      if (n == 3) repo.commit("ios: tweak", "ios/App.swift" -> "// app only\n")
      if (n == 5) repo.commit("docs: note", "docs/note.md" -> "words\n")
      repo.commit(s"pipeline change $n",
        "worker/src/main/State.scala" -> (if (n >= badAt) "bad\n" else "ok\n"),
        s"worker/src/main/Change$n.scala" -> s"// $n\n")
    }
    val bad: String      = pipeline.last
    lazy val firstBad: String = pipeline(badAt - 1)

    /** Exits 1 (bad) once the bug is in, 0 before; logs the subject of every commit it
     *  was asked to replay — the last green `base` first, confirmed before any bisecting. */
    val steps: Path = Files.createTempFile("bisect-steps", ".log")
    private val skipped =
      if (untestable.isEmpty) "__none__" else untestable.map(n => s"\"pipeline change $n\"").mkString("|")
    val step: Path = repo.script("step.sh",
      s"""git log -1 --format=%s >> "$steps"
         |case "$$(git log -1 --format=%s)" in $skipped) exit 125 ;; esac
         |${if (redEverywhere) "exit 1" else ""}
         |grep -q bad worker/src/main/State.scala && exit 1 || exit 0
         |""".stripMargin)
    def tested: Seq[String] = Files.readString(steps).linesIterator.toSeq
    def stepCount: Int = tested.size
  }

  private def bisect(history: History, good: String, bad: String, env: (String, String)*): (Int, String, Option[String]) = {
    val verdict  = Files.createTempFile("bisect-verdict", ".md")
    val firstBad = Files.createTempFile("bisect-first-bad", ".txt")
    Files.delete(firstBad)
    val all = Seq(
      "COUNTRY" -> "poland", "STEP_COMMAND" -> history.step.toString, "STEP_MINUTES" -> "1",
      "BUDGET_MINUTES" -> "30", "MAX_STEPS" -> "3", "SAMPLE_FAILED" -> "true",
      "VERDICT_FILE" -> verdict.toString, "FIRST_BAD_FILE" -> firstBad.toString,
      "CONVERGENCE_PATHS_FILE" -> PathsFile.toString) ++ env
    val status = Process(Seq("bash", Script.toString, "bisect", good, bad), history.repo.root.toFile, all*)
      .!(ProcessLogger(_ => ()))
    val pinned = Option.when(Files.exists(firstBad))(Files.readString(firstBad).trim)
    (status, Files.readString(verdict), pinned)
  }

  private def short(sha: String) = sha.take(7)

  "the convergence bisect" should "name the first bad commit within three replays" in {
    val history = new History(badAt = 4)
    val (status, verdict, pinned) = bisect(history, history.good, history.bad)

    status shouldBe 0
    pinned shouldBe Some(history.firstBad)
    verdict should include("First bad commit:")
    verdict should include("pipeline change 4")
    history.tested.head shouldBe "base"
    withClue("six pipeline commits need at most three halvings: ")(history.stepCount - 1 should be <= 3)
  }

  it should "spend no replay on a commit that touches no pipeline path" in {
    val history = new History(badAt = 4)
    val (_, verdict, _) = bisect(history, history.good, history.bad, "SAMPLE_FAILED" -> "false")

    verdict should include("6 pipeline commit(s)")
    history.tested should not be empty
    history.tested.filterNot(t => t.startsWith("pipeline change") || t == "base") shouldBe empty
  }

  it should "report the remaining range, not a guess, when the cap stops it first" in {
    val history = new History(badAt = 4)
    val (status, verdict, pinned) = bisect(history, history.good, history.bad, "MAX_STEPS" -> "1")

    status shouldBe 0
    pinned shouldBe None
    history.stepCount shouldBe 2 // the last green, then one halving
    verdict should include("Stopped after 1 replay(s)")
    verdict should include(short(history.firstBad))
    verdict should not include "ios: tweak"
  }

  it should "keep a skipped commit in the reported range, since it may be the first bad one" in {
    val history = new History(badAt = 4, untestable = Set(4))
    val (status, verdict, pinned) = bisect(history, history.good, history.bad, "MAX_STEPS" -> "6")

    status shouldBe 0
    pinned shouldBe None
    verdict should include("The first bad commit is one of:")
    verdict should include(short(history.firstBad))
    verdict should include("pipeline change 4")
    verdict should include("untested")
  }

  it should "not start a replay the budget cannot fit" in {
    val history = new History(badAt = 4)
    val (_, verdict, pinned) = bisect(history, history.good, history.bad,
      "STEP_MINUTES" -> "20", "BUDGET_MINUTES" -> "10")

    history.stepCount shouldBe 0
    pinned shouldBe None
    verdict should include("No time left")
  }

  it should "spend nothing when the same commit was green before" in {
    val history = new History(badAt = 4)
    val (_, verdict, pinned) = bisect(history, history.bad, history.bad)

    history.stepCount shouldBe 0
    pinned shouldBe None
    verdict should include("was GREEN in an earlier run")
  }

  it should "spend nothing when no pipeline commit is in range" in {
    val history = new History(badAt = 4)
    val docsOnly = history.repo.commit("docs: more", "docs/more.md" -> "x\n")
    val (_, verdict, _) = bisect(history, history.bad, docsOnly)

    history.stepCount shouldBe 0
    verdict should include("touches the pipeline")
  }

  it should "pin the only pipeline commit in range once its last green replays green" in {
    val history = new History(badAt = 6)
    val beforeLast = history.repo.git("rev-parse", s"${history.bad}~1")
    val (_, verdict, pinned) = bisect(history, beforeLast, history.bad)

    history.tested shouldBe Seq("pipeline change 5")
    pinned shouldBe Some(history.bad)
    verdict should include("the only pipeline commit in the range")
  }

  // The pair is re-pinned nightly, so the last green leg may have replayed an older one.
  it should "blame no commit when the recording is red at the last green commit too" in {
    Seq(
      { val h = new History(badAt = 6, redEverywhere = true); (h, h.repo.git("rev-parse", s"${h.bad}~1")) },
      { val h = new History(badAt = 4, redEverywhere = true); (h, h.good) }
    ).foreach { case (history, good) =>
      val (status, verdict, pinned) = bisect(history, good, history.bad)

      status shouldBe 0
      pinned shouldBe None
      history.stepCount shouldBe 1
      verdict should include("red at the last green commit")
      verdict should not include "First bad commit"
    }
  }

  it should "refuse to bisect a full-leg failure the sample does not reproduce" in {
    val history = new History(badAt = 7) // never bad: the sample is green everywhere
    val (_, verdict, pinned) = bisect(history, history.good, history.bad, "SAMPLE_FAILED" -> "false")

    history.stepCount shouldBe 1
    pinned shouldBe None
    verdict should include("does not reproduce")
  }

  "the last-green lookup" should "take the newest run whose FULL leg passed, skipping this one" in {
    val history = new History(badAt = 4)
    // The stub answers the way `gh … --jq` would: the list already projected, and a job
    // name only for the run whose full leg was green.
    val gh = history.repo.script("gh",
      s"""case "$$1 $$2" in
         |  "run list")  printf '30 ${history.bad}\\n20 ${history.firstBad}\\n10 ${history.good}\\n' ;;
         |  "run view")  [ "$$3" = "10" ] && echo "poland / convergence" ;;
         |esac
         |exit 0
         |""".stripMargin)
    val path = s"${gh.getParent}:${sys.env.getOrElse("PATH", "")}"
    val out = Process(Seq("bash", Script.toString, "last-green"), history.repo.root.toFile,
      "PATH" -> path, "COUNTRY" -> "poland", "WORKFLOW" -> "Country convergence", "RUN_ID" -> "30").!!.trim

    out shouldBe history.good
  }
}
