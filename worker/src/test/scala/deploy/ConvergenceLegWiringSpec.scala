package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Locks the convergence suite to a matrix of PAIRS: one leg per country, each
 * running that country's sample and then, only if it passed, that country's full
 * run.
 *
 * The shape it replaced was two jobs, `sample` and `convergence`, the second
 * declaring `needs: sample`. That reads like per-country gating and isn't —
 * `needs:` joins JOBS, not matrix legs, so every country waited for EVERY
 * country's sample. Poland's held Germany's and the UK's full legs; the UK's
 * (slowest, largest corpus) held everyone's; one country's flap cost the day's
 * answer for the other two. Sequencing the pair inside a single leg removes the
 * shared edge entirely — but only while these hold, and none is visible at a
 * glance in the YAML:
 *
 *   - the sample step comes BEFORE the full run, which is the whole gate. A
 *     failed step skips the ones after it, so order is the wiring; reverse them
 *     and the 73-minute run happens first, gated by nothing.
 *   - each leg pairs a country's full alias with the SAME country's sample
 *     alias, and both aliases exist. A mis-paired row would gate Germany's run
 *     on Poland's sample and still be green.
 *   - the job's ceiling stays clear of both step budgets combined. A job that
 *     hits `timeout-minutes` is CANCELLED, and a cancelled job runs its
 *     `always()` publish steps only inside a short grace window — so a leg that
 *     overruns discards the very capture that would have made the next run fast
 *     enough not to overrun. Adding the sample's 10 minutes to a ceiling sized
 *     for the full run alone is exactly how that gets reintroduced.
 *   - no `needs:` survives anywhere in the file. One reappearing is the
 *     regression itself.
 */
class ConvergenceLegWiringSpec extends AnyFlatSpec with Matchers {
  private lazy val yml   = RepoFile.read(".github/workflows/country-convergence.yml")
  private lazy val build = RepoFile.read("build.sbt")

  /**
   * The matrix rows: country → (sample alias, full alias).
   *
   * Each `- { … }` row is read as an unordered set of `key: value` pairs rather than
   * matched positionally — a spec that hard-codes the field order fails loudly on a
   * harmless reshuffle and says "no countries found", which reads as a structural
   * break when nothing structural changed.
   */
  private lazy val countries: Map[String, (String, String)] =
    """-\s*\{([^}]*)}""".r
      .findAllMatchIn(RepoFile.block(yml, "matrix"))
      .map { row =>
        val fields = """(\w+):\s*([\w-]+)""".r.findAllMatchIn(row.group(1)).map(f => f.group(1) -> f.group(2)).toMap
        fields("country") -> (fields("sample"), fields("cmd"))
      }
      .toMap

  /** Every `timeout-minutes:` in file order — the job's ceiling, then the sample's, then the suite's. */
  private lazy val budgets: Seq[Int] =
    """timeout-minutes:\s*(\d+)""".r.findAllMatchIn(yml).map(_.group(1).toInt).toSeq

  "the convergence suite" should "run one leg per country" in {
    countries.keySet shouldBe Set("poland", "germany", "united-kingdom")
  }

  it should "let one country's failure stop only that country" in {
    RepoFile.block(yml, "strategy") should include("fail-fast: false")
  }

  it should "join no country to another, at any distance" in {
    // `needs:` between jobs is what made all three move as one. There is only one
    // job now, so any `needs:` at all is either dead or the regression returning.
    yml.linesIterator.map(_.trim).filter(_.startsWith("needs:")).toList shouldBe empty
  }

  it should "pair each country's full run with that same country's sample" in {
    countries.foreach { case (country, (sample, command)) =>
      withClue(s"$country: ") { sample shouldBe s"${command}Sample" }
    }
  }

  it should "name only aliases the build actually defines" in {
    countries.values.flatMap { case (sample, command) => Seq(sample, command) }.foreach { alias =>
      withClue(s"$alias: ") { build should include("addCommandAlias(\"" + alias + "\"") }
    }
  }

  it should "run the sample before the full run, which is the gate" in {
    val sampleStep = yml.indexOf("sbt ${{ matrix.sample }}")
    val suiteStep  = yml.indexOf("sbt ${{ matrix.cmd }}")
    sampleStep should be >= 0
    withClue(s"sample at $sampleStep, suite at $suiteStep: ") { sampleStep should be < suiteStep }

    // And both are reached through the matrix, not hard-coded per country — three
    // copied step blocks would drift the moment one country's budget changes.
    countries.values.foreach { case (sample, command) =>
      yml should not include s"sbt $sample"
      yml should not include s"sbt $command"
    }
  }

  it should "keep the job's ceiling clear of both step budgets combined" in {
    val Seq(job, sample, suite) = budgets.take(3)
    withClue(s"job $job vs sample $sample + suite $suite: ") { job should be > (sample + suite) }
    // The margin pays for setup and the `always()` publish steps, which is what a
    // cancelled job loses. Ten minutes is the floor those steps were measured at.
    job - (sample + suite) should be >= 10
  }
}
