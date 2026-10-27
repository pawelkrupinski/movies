package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}
import scala.sys.process.*

/**
 * The cases of [[ConvergenceReuseSpec]] that run the real `scripts/ci/convergence_reuse.py` (python3): the judgement
 * over the cases that must NOT reuse, and the rows main expects.
 */
class ConvergenceReuseIntegrationSpec extends AnyFlatSpec with Matchers {

  private val Script = "scripts/ci/convergence_reuse.py"
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
}
