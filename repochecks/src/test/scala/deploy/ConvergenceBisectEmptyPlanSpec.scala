package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A plan with nothing to bisect must END the run, not strand it.
 *
 * The matrix always has a row -- a placeholder when the plan has no country -- and the job's own `if:`
 * skips it, which `tell` and `timings` read as a skipped `bisect`.
 */
class ConvergenceBisectEmptyPlanSpec extends AnyFlatSpec with Matchers {
  private val bisect = RepoFile.jobs(RepoFile.read(".github/workflows/convergence-bisect.yml"))("bisect")

  private val countryRow = RepoFile.block(bisect, "matrix").linesIterator
    .map(_.trim).find(_.startsWith("country:"))
    .getOrElse(throw new AssertionError("the bisect matrix has no `country:` row"))

  "the bisect matrix" should "never hand fromJson an empty country list" in {
    countryRow shouldBe
      """country: ${{ fromJson(needs.plan.outputs.countries != '[]' && needs.plan.outputs.countries || '["none"]') }}"""
  }

  "the placeholder row" should "be skipped by the job's own if when there is no request" in {
    val cond = bisect.linesIterator.map(_.trim).find(_.startsWith("if:")).getOrElse("")
    cond should include("needs.plan.outputs.requests != '[]'")
  }
}
