package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Locks each country's convergence leg to ITS OWN sample gate.
 *
 * `needs:` in GitHub Actions joins JOBS, not matrix legs — so a `convergence`
 * matrix declaring `needs: sample` waits for EVERY country's sample, not its
 * own. Three countries then moved as one: Poland's sample held Germany's and the
 * UK's full legs, the UK's sample (slowest, largest corpus) held everyone's, and
 * one country's flap cost the day's answer for the other two.
 *
 * The fix is one reusable workflow holding a single sample → convergence pair,
 * called once per country. That is only correct while three things hold, and
 * none of them is visible at a glance in the YAML:
 *
 *   - the pair is genuinely chained (`convergence` needs `sample`) inside the
 *     called file, where "the sample" can only mean this country's;
 *   - the caller's matrix pairs each country's full alias with the SAME
 *     country's sample alias — a mis-paired row would gate Germany's leg on
 *     Poland's sample and still be green;
 *   - the called file declares no `concurrency:` of its own. All three calls
 *     live in one run, so a group named there would be shared and the legs would
 *     cancel each other. The lane belongs to the caller. (See
 *     [[ConvergenceConcurrencyConfigSpec]].)
 */
class ConvergenceLegWiringSpec extends AnyFlatSpec with Matchers {
  private lazy val caller = RepoFile.read(".github/workflows/country-convergence.yml")
  private lazy val leg    = RepoFile.read(".github/workflows/country-convergence-leg.yml")
  private lazy val build  = RepoFile.read("build.sbt")

  /**
   * The caller's matrix rows: country → (full alias, sample alias).
   *
   * Each `- { … }` row is read as an unordered set of `key: value` pairs rather than
   * matched positionally — a spec that hard-codes the field order fails loudly on a
   * harmless reshuffle and says "no countries found", which reads as a structural
   * break when nothing structural changed.
   */
  private lazy val countries: Map[String, (String, String)] =
    """-\s*\{([^}]*)}""".r
      .findAllMatchIn(RepoFile.block(caller, "matrix"))
      .map { row =>
        val fields = """(\w+):\s*([\w-]+)""".r.findAllMatchIn(row.group(1)).map(f => f.group(1) -> f.group(2)).toMap
        fields("country") -> (fields("cmd"), fields("sample"))
      }
      .toMap

  /** The leg workflow's `timeout-minutes:` in file order — the sample job, the full job, then its suite step. */
  private lazy val budgets: Seq[Int] =
    """timeout-minutes:\s*(\d+)""".r.findAllMatchIn(leg).map(_.group(1).toInt).toSeq

  "the convergence caller" should "run every country whose worker is live through the single-country leg workflow" in {
    // Poland alone since 2026-08-02. The germany / united-kingdom rows were
    // removed with their workers: this suite scores the run against PRODUCTION's
    // read model, and a stopped worker freezes that baseline while the archive
    // keeps growing, so those legs could never be green again. The per-country
    // wiring below is unchanged and still guards each row that IS here.
    countries.keySet shouldBe Set("poland")
    caller should include("uses: ./.github/workflows/country-convergence-leg.yml")
  }

  it should "hold no sample job of its own, which every country would then wait on" in {
    // The regression this whole split exists to prevent: a `sample` job here is by
    // construction shared, so the only safe place for one is inside the per-country
    // workflow.
    caller.linesIterator.map(_.trim).toList should not contain "sample:"
  }

  it should "let one country's failure stop only that country" in {
    RepoFile.block(caller, "strategy") should include("fail-fast: false")
  }

  it should "gate each country's full leg on that same country's sample" in {
    countries.foreach { case (country, (command, sample)) =>
      withClue(s"$country: ") { sample shouldBe s"${command}Sample" }
    }
  }

  it should "name only aliases the build actually defines" in {
    countries.values.flatMap { case (command, sample) => Seq(command, sample) }.foreach { alias =>
      withClue(s"$alias: ") { build should include("addCommandAlias(\"" + alias + "\"") }
    }
  }

  "the single-country leg workflow" should "run its full leg behind its own sample" in {
    RepoFile.block(leg, "convergence") should include("needs: sample")
  }

  it should "leave the concurrency lane to the caller, so its three calls don't cancel each other" in {
    leg.linesIterator.map(_.trim).toList should not contain "concurrency:"
  }

  it should "keep the full job's ceiling clear of the suite step it wraps" in {
    // A job that hits `timeout-minutes` is CANCELLED, and a cancelled job runs its
    // `always()` publish steps only inside a short grace window — so a leg that
    // overruns discards the very capture that would have made the next run fast
    // enough not to overrun. The gap between the two numbers is what pays for setup
    // and the four publishes; raising the step without the job reintroduces exactly
    // that. Order in the file is: sample job, full job, suite step.
    val Seq(sampleJob, fullJob, suiteStep) = budgets.take(3)
    withClue(s"sample job $sampleJob, full job $fullJob, suite step $suiteStep: ") {
      fullJob should be > suiteStep
      fullJob - suiteStep should be >= 10
    }
  }
}
