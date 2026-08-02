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

  /** The caller's matrix rows: country → (full alias, sample alias). */
  private lazy val countries: Map[String, (String, String)] = {
    val row = """-\s*\{\s*country:\s*([\w-]+),\s*code:\s*(\w+),\s*cmd:\s*(\w+),\s*sample:\s*(\w+)\s*\}""".r
    row
      .findAllMatchIn(RepoFile.block(caller, "matrix"))
      .map(m => m.group(1) -> (m.group(3), m.group(4)))
      .toMap
  }

  "the convergence caller" should "run every country through the single-country leg workflow" in {
    countries.keySet shouldBe Set("poland", "germany", "united-kingdom")
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
}
