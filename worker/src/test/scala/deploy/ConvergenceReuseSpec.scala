package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.sys.process.*

/**
 * Main reuses a convergence verdict only when it provably applies (scripts/ci/convergence_reuse.py).
 *
 * A landing check runs the whole `Identity model convergence` suite over the commit it calls LANDABLE; Main's kick then
 * dispatched the same 17 rows again over the same tree. Main's kick now names that run (`reuse-run`), and the run's
 * preflight skips every leg only when the source covered every row main runs -- both order-independence phases among
 * them -- over the same tree key, pairs, inputs and takes bases, and every row passed. These run the real judgement over
 * the cases that must NOT reuse, and hold the wiring that keeps the daily full run and the legs' inputs honest.
 */
class ConvergenceReuseSpec extends AnyFlatSpec with Matchers {

  private val Script = "scripts/ci/convergence_reuse.py"
  private def suite  = RepoFile.read(".github/workflows/identity-model-convergence.yml")
  private def leg    = RepoFile.read(".github/workflows/country-convergence-leg.yml")

  private lazy val rows: Seq[String] = Seq("python3", Script, "expected-rows").!!.linesIterator.toSeq
  private val mainInputs =
    """{"profile": "false", "jvm-options": "", "runner": "", "takes-base": "", "takes-only": "false", "recording": "",
      |"takes-countries": "", "takes-no-store": "false", "takes-skip": "shadow"}""".stripMargin

  private def json(text: String): Path = {
    val file = Files.createTempFile("reuse", ".json")
    Files.writeString(file, text)
    file
  }

  private def jobs(names: Seq[String], failed: String): String = {
    val callers = names.map(n => "leg " + n.substring(n.indexOf('('))).distinct
    (Seq("preflight") ++ names ++ callers).map { n =>
      s"""{"name": "$n", "status": "completed", "conclusion": "${if (n == failed) "failure" else "success"}"}"""
    }.mkString("[", ",", "]")
  }

  /** Why the source does not hold; empty when it does. */
  private def judge(conclusion: String = "success", sourceKey: String = "k1", orderPhases: String = "both",
                    takesOnly: String = "false", ran: Seq[String] = rows, failed: String = ""): Seq[String] = {
    val run   = s"""{"id": 2821, "path": ".github/workflows/identity-model-convergence.yml", "status": "completed",
                   |"conclusion": "$conclusion", "head_sha": "${"a" * 40}"}""".stripMargin
    val facts = s"""{"key": "$sourceKey", "pairs": {"pl": "573 x tree"}, "order_phases": "$orderPhases", "reused_from": "",
                   |"inputs": ${mainInputs.replace("\"takes-only\": \"false\"", s"\"takes-only\": \"$takesOnly\"")}}""".stripMargin
    val mine  = s"""{"key": "k1", "pairs": {"pl": "573 x tree"}, "order_phases": "both", "inputs": $mainInputs,
                   |"expected_rows": [${rows.map(r => s"\"$r\"").mkString(",")}],
                   |"takes_bases": {"pl": ["${"b" * 40}", "${"b" * 40}", true]}}""".stripMargin
    Seq("python3", Script, "judge", "--run", json(run).toString, "--jobs", json(jobs(ran, failed)).toString,
      "--facts", json(facts).toString, "--mine", json(mine).toString).!!.linesIterator.filter(_.nonEmpty).toSeq
  }

  "a source run" should "be reused when it ran every row main runs, green, over the same tree" in {
    judge() shouldBe empty
  }

  it should "not be reused over a different tree" in {
    judge(sourceKey = "k2").mkString should include("the tree differs")
  }

  // Off main, order-phases.sh may run p01 alone (two arrival orders compared, not three): never main's verdict.
  it should "not be reused when it ran one order-independence phase" in {
    judge(orderPhases = "one").mkString should include("order-independence phases 'one'")
    judge(ran = rows.filterNot(_.startsWith("order-independence-p12"))).mkString should
      include("`order-independence-p12 (germany)` did not run there")
  }

  it should "not be reused when it is red, or any row of it is" in {
    judge(conclusion = "failure").mkString should include("not a green completed run")
    judge(failed = "takes (spain)").mkString should include("`takes (spain)` ended completed/failure there")
  }

  it should "not be reused when it ran part of the suite" in {
    val partial = judge(takesOnly = "true", ran = rows.filter(_.startsWith("takes")))
    partial.mkString should include("takes-only: only the takes rows ran")
    partial.mkString should include("`convergence (poland)` did not run there")
  }

  "the rows main expects" should "be the leg's own expansion of main's matrix" in {
    // The judgement models the leg's `phase:` expression; a change to it must change the model with it.
    RepoFile.block(leg, "matrix") should include("phase: " + Seq("python3", Script, "phases-expression").!!.trim)
    rows should have size 27
    rows should contain allOf ("order-independence-p01 (germany)", "order-independence-p12 (germany)",
      "next-day (united-states)", "takes (poland)", "takes-drift-2 (poland)", "convergence (spain)")
  }

  // Forgejo expands the matrix `leg` call only once preflight is done and hands its `with:` that job's outputs and nothing
  // else (RunnerClassSpec): from 2026-10-23 every `inputs.<name>` there read empty -- every takes-only measurement ran the
  // whole suite and every takes-base was dropped. The reuse facts are the preflight's outputs, so they are what the legs ran.
  "main's legs" should "read every input through the preflight's outputs" in {
    val call = RepoFile.withoutComments(RepoFile.block(RepoFile.jobs(suite)("leg"), "with"))
    call should not include "inputs."
    call should not include "github."
    call should include("reused-from:                   ${{ needs.preflight.outputs.reused-from }}")
    RepoFile.block(RepoFile.jobs(suite)("preflight"), "outputs") should include(
      "reused-from: ${{ steps.reuse.outputs.reused-from || '' }}")
  }

  it should "skip every row only on a reuse the preflight proved" in {
    RepoFile.block(leg, "convergence").linesIterator.find(_.trim.startsWith("if:")).getOrElse("") should
      include("if: ${{ inputs.reused-from == '' && ")
    RepoFile.step(suite, "Does the named run's verdict hold for this commit?") should
      include("if: ${{ inputs.reuse-run != '' }}")
  }

  // The daily full run (convergence-daily.yml) exists so that one run a day takes every take in full: it never reuses.
  // Only Main's kick names a run to reuse.
  "the daily full run" should "never name a run to reuse" in {
    RepoFile.read(".github/workflows/convergence-daily.yml") should not include "reuse-run"
    RepoFile.read(".github/scripts/kick-convergence.sh") should include("-f reuse-run=")
  }
}
