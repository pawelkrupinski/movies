package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * A plan with nothing to bisect must END the run, not strand it.
 *
 * Forgejo 15 expands a matrix before it judges the job's `if:`, and a matrix that expands to no
 * row deletes the job outright: `tell`, `timings` and the timings report then `need` a job that no
 * longer exists and wait as "blocked" forever (run 273, 2026-10-21, dispatched for a convergence
 * run that left no bisect request; runs 156 and 167 the day before, cancelled by hand). github.com
 * skips such a job instead. So the matrix always has a row -- a placeholder when the plan has no
 * country -- and the job's own `if:` skips it on both forges, which `tell` and `timings` read as a
 * skipped `bisect`.
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
