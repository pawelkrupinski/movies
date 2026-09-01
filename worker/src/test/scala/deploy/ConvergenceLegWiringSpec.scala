package deploy

import models.Country
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
  private lazy val caller   = RepoFile.read(".github/workflows/country-convergence.yml")
  /** The United States runs the same leg from a BUILD OF ITS OWN, because it is the one
   *  country whose leg must not be superseded — see [[ConvergenceConcurrencyConfigSpec]].
   *  Everything below is about the leg wiring, which is identical either side of that
   *  split, so every rule here reads both callers rather than the shared one. A rule
   *  that looked only at `country-convergence.yml` would go quiet about the country it
   *  matters most for. */
  private lazy val usCaller = RepoFile.read(".github/workflows/us-convergence.yml")
  private lazy val callers  = Seq(caller, usCaller)
  private lazy val leg      = RepoFile.read(".github/workflows/country-convergence-leg.yml")
  private lazy val build    = RepoFile.read("build.sbt")

  /**
   * The caller's matrix rows: country → (full alias, sample alias).
   *
   * Each `- { … }` row is read as an unordered set of `key: value` pairs rather than
   * matched positionally — a spec that hard-codes the field order fails loudly on a
   * harmless reshuffle and says "no countries found", which reads as a structural
   * break when nothing structural changed.
   */
  private def matrixRows(yaml: String): Seq[Map[String, String]] =
    """-\s*\{([^}]*)}""".r
      .findAllMatchIn(RepoFile.block(yaml, "matrix"))
      .map(row => """(\w+):\s*([\w-]+)""".r.findAllMatchIn(row.group(1)).map(f => f.group(1) -> f.group(2)).toMap)
      .toSeq

  private lazy val rows: Seq[Map[String, String]] = callers.flatMap(matrixRows)

  private lazy val countries: Map[String, (String, String)] =
    rows.map(fields => fields("country") -> (fields("cmd"), fields("sample"))).toMap

  /** Every (job ceiling, suite step) budget pair the legs actually run on, labelled.
   *
   *  The numbers used to be literals inside the leg's two job blocks. They are per-COUNTRY
   *  now — the United States has no enrichment tree yet, so its first legs fetch every
   *  lookup live and need hours where a warm country needs a couple — which means the
   *  pairs live in the caller's matrix, with the leg's `default:`s standing in for a
   *  caller that says nothing. Both sources are checked, because a gap that closes in
   *  either place cancels a leg just as dead. */
  private def budgetPairs: Seq[(String, Int, Int)] = {
    val defaults = Seq(
      ("leg default (full)",   "job-timeout-minutes",        "suite-timeout-minutes"),
      ("leg default (sample)", "sample-job-timeout-minutes", "sample-suite-timeout-minutes"))
      .map { case (label, jobKey, suiteKey) => (label, defaultOf(jobKey), defaultOf(suiteKey)) }
    val perCountry = rows.flatMap { fields =>
      val country = fields("country")
      Seq((s"$country full",   fields("job").toInt,       fields("suite").toInt),
          (s"$country sample", fields("sampleJob").toInt, fields("sampleSuite").toInt))
    }
    defaults ++ perCountry
  }

  /** The `default:` under one of the leg's `workflow_call` inputs. */
  private def defaultOf(input: String): Int =
    s"$input:[\\s\\S]*?default:\\s*(\\d+)".r.findFirstMatchIn(leg)
      .getOrElse(fail(s"no default declared for input `$input` in the leg workflow"))
      .group(1).toInt

  /** Both jobs publish through the same composite action, so neither can drift from the
   *  other on what "publish the tree" means. */
  private val PublishAction = "uses: ./.github/actions/convergence-publish"

  private val Jobs = Seq("sample", "convergence")

  /** Derived from the MODEL, not from a hard-coded list.
   *
   *  A hard-coded triple is green the day a fourth country is added and silently leaves
   *  it with no convergence cover at all — which is exactly what happened: the United
   *  States shipped a full `Country.all` entry, a `kinowo_us` database, 5,031 venues and
   *  a running worker, and no leg here ever asked whether its pipeline converged. */
  "the convergence caller" should "run every country through the single-country leg workflow" in {
    rows.map(_("code")).toSet shouldBe Country.all.map(_.code).toSet
    callers.foreach(_ should include("uses: ./.github/workflows/country-convergence-leg.yml"))
  }

  /** The split itself, from both ends — a US row left behind in the shared caller would
   *  run the country TWICE per push, once in a lane that cancels it four hours in. */
  it should "run the United States from its own build, and only from there" in {
    matrixRows(caller).map(_("code")) should not contain "us"
    matrixRows(usCaller).map(_("code")) shouldBe Seq("us")
  }

  /** The recorder's CREDENTIAL, pinned for the same reason as its matrix.
   *
   *  `mongo-ci-read.nix` documents the `db.createUser` an operator runs on mongo-1 — the
   *  role is explicit that it does NOT create the user itself — and that command names the
   *  country databases one by one. It is therefore a third hand-maintained list of the
   *  same countries, and it rotted exactly like the other two: the recorder's US leg
   *  failed with `not authorized on kinowo_us` while the other three succeeded, because
   *  the reader holds `read` on three databases and there are four.
   *
   *  A doc comment cannot grant anything, so this does not make the grant happen — it
   *  makes the omission VISIBLE at the same moment the country is modelled, instead of
   *  the day someone runs the recorder and reads a stack trace. */
  it should "document a read grant for every country database the recorder must read" in {
    val role = RepoFile.read("infra/nix/modules/roles/mongo-ci-read.nix")
    val granted = """role:\s*"read",\s*db:\s*"(\w+)"""".r
      .findAllMatchIn(role).map(_.group(1)).toSet
    granted shouldBe Country.all.map(_.mongoDb).toSet
  }

  /** The leg's INPUT, pinned in the same breath as the leg.
   *
   *  A convergence leg with no recorded corpus is not a failing leg — it is a leg that
   *  falls back to a generated corpus and reports nonsense (the UK's `tmdbId 0` run,
   *  which cost eleven runs and a wrong diagnosis). The recorder's matrix and the
   *  convergence caller's matrix have to name the same countries, so neither can gain a
   *  country the other doesn't. */
  it should "record a corpus for every country it then replays" in {
    val recorder = RepoFile.read(".github/workflows/record-scrape-fixtures.yml")
    matrixRows(recorder).map(_("code")).toSet shouldBe Country.all.map(_.code).toSet
  }

  it should "hold no sample job of its own, which every country would then wait on" in {
    // The regression this whole split exists to prevent: a `sample` job here is by
    // construction shared, so the only safe place for one is inside the per-country
    // workflow.
    callers.foreach(_.linesIterator.map(_.trim).toList should not contain "sample:")
  }

  it should "let one country's failure stop only that country" in {
    callers.foreach(RepoFile.block(_, "strategy") should include("fail-fast: false"))
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

  it should "keep EVERY job's ceiling clear of the suite step it wraps" in {
    // A job that hits `timeout-minutes` is CANCELLED, and a cancelled job runs its
    // `always()` publish steps only inside a short grace window — so a leg that
    // overruns discards the very capture that would have made the next run fast
    // enough not to overrun. The gap between the two numbers is what pays for setup
    // and the publishes; raising the step without the job reintroduces exactly that.
    //
    // The rule was written for the full leg and applied only there, and the sample —
    // which had no step ceiling at all, so it could only ever be cancelled — is the
    // job that then spent ten consecutive runs discarding its own progress.
    budgetPairs.foreach { case (label, ceiling, suiteStep) =>
      withClue(s"$label: job $ceiling, suite step $suiteStep: ") {
        ceiling should be > suiteStep
        ceiling - suiteStep should be >= 10
      }
    }
  }

  /** GitHub cancels a hosted job at 360 minutes whatever `timeout-minutes` says, so a
   *  budget above that is not a longer leg — it is the same cancellation with the guard
   *  that was supposed to prevent it silently disarmed. */
  /** The heap every leg's sbt JVM runs on, labelled — one entry per caller row.
   *
   *  Read from the CALLERS, because that is where the number that actually binds lives.
   *  The leg's `default:` is what a caller silently inherits, and inheriting it is how
   *  the ceiling stopped being anybody's decision in the first place. */
  private def heaps: Seq[(String, String)] =
    rows.map(fields => (fields("country"), fields.getOrElse("heap",
      fail(s"${fields("country")} declares no `heap` — its leg would inherit a ceiling nobody chose"))))

  private def heapGigabytes(value: String): Int =
    "^(\\d+)g$".r.findFirstMatchIn(value)
      .getOrElse(fail(s"heap `$value` is not an -Xmx value in whole gigabytes"))
      .group(1).toInt

  /** The ceiling that killed the United States' leg twice on 2026-09-01.
   *
   *  Nothing was passing `-Xmx` at all: `.jvmopts` names `-Xmx4g` for the unforked local
   *  `testUnit`, the leg's sbt launcher reads that file, and a 16 GB runner's JVM would
   *  have landed on the same 4g by default anyway — so every leg ran on a number nobody
   *  had chosen for it. A convergence leg holds the whole country resident at once, and
   *  the US leg spent its last 45 seconds above 70% GC and exited 3 on
   *  `-XX:+ExitOnOutOfMemoryError` before ScalaTest reported a single assertion. A budget
   *  in minutes buys nothing when the heap runs out first, and a timeout rule that never
   *  looked at the heap said the leg was comfortably inside its 315. */
  it should "run every leg on a heap its caller chose, passed to sbt rather than inherited" in {
    heaps.foreach { case (country, heap) =>
      withClue(s"$country: ")(heapGigabytes(heap) should be >= 4)
    }
    withClue("neither sbt invocation may fall back to `.jvmopts`' 4g: ") {
      leg.linesIterator.filter(_.trim.startsWith("sbt ")).toList.foreach(
        _ should include("-J-Xmx${{ inputs.heap }}"))
    }
  }

  /** The US is the country the knob exists for, so a US row that drifts back to the warm
   *  countries' 4g is the regression this rule is here to catch — and it would read as a
   *  timeout, not as a heap, every time. */
  it should "give the United States more heap than the warm countries" in {
    val us   = heapGigabytes(heaps.toMap.apply("united-states"))
    val warm = heaps.filterNot(_._1 == "united-states").map { case (_, heap) => heapGigabytes(heap) }
    warm.foreach(gigabytes => us should be > gigabytes)
  }

  it should "keep every budget under the platform's own 360-minute ceiling" in {
    budgetPairs.foreach { case (label, ceiling, _) =>
      withClue(s"$label: ")(ceiling should be <= 360)
    }
  }

  /** The check-run that told us 4,381 tests had passed on a leg that ran none of them.
   *
   *  `target/` is restored from the actions cache WHOLE, every module writes its JUnit
   *  XML into the one root-level `target/test-reports/unit/` (build.sbt's
   *  `unitReportSettings`), and the restore-keys fall through to the key `ci.yml`'s unit
   *  job saves. So a leg begins with a report directory full of specs from another
   *  workflow, and the publish step globs that directory. A leg that FINISHES overwrites
   *  them and nobody notices; the United States' leg died on an OOM before ScalaTest
   *  reported a result, and its check-run went green naming `CinemaScraperCatalogSpec`.
   *
   *  Both halves are load-bearing and neither works alone: clearing the directory
   *  without `require_tests` turns the lie into a shrug, and `require_tests` over a
   *  directory nobody cleared still reports somebody else's passes. */
  it should "report only the tests THIS leg ran, and admit it when there are none" in {
    val setup = RepoFile.read(".github/actions/convergence-setup/action.yml")
    withClue("the cache restores another job's test reports; the leg must discard them: ") {
      setup should include("rm -rf target/test-reports")
    }
    withClue("a leg that produced no report must fail the check rather than skip it: ") {
      RepoFile.block(leg, "convergence") should include("require_tests: true")
    }
  }

  it should "publish the tree it recorded from BOTH jobs, not just the full one" in {
    // The gate REPLAYS a fixture tree it is not allowed to extend, and every recorded
    // response in that tree expires after `EnrichmentFreshness.Ttl` (5 days). Only the
    // full leg republished it, and the full leg is `needs: sample` — so a country whose
    // sample failed for five days had its tree pruned to nothing, which made the sample
    // slower still, which kept the full leg from ever running again. Germany sat in
    // exactly that loop for ten runs from 2026-08-09: an asset that could only be
    // refreshed by a job that could only run once the asset was fresh.
    Jobs.foreach { job =>
      withClue(s"$job: ") { RepoFile.block(leg, job) should include(PublishAction) }
    }
  }

  it should "keep the capture when tar reports the tree changing under it" in {
    // The publish runs on `always()`, so its most valuable case is a leg that just
    // ran out of time — and the runner reaps that leg's JVM in its POST-job phase,
    // well after this step. `RecordingHttpFetch` is therefore still writing into the
    // tree being read; GNU tar prints "file changed as we read it" and exits 1, a
    // warning status beside a complete archive. Under `set -e` that failed the step
    // and discarded the whole capture — the exact trap the publish exists to close,
    // and it cost Germany's first full leg in a week its entire corpus capture.
    val publish = RepoFile.read(".github/actions/convergence-publish/action.yml")
    publish should include("packed=$?")
    withClue("tar's warning status (1) must not fail the step; 2+ still must: ") {
      publish should include("""if [ "$packed" -gt 1 ]""")
    }
  }

  it should "let the sample write the release it now publishes to" in {
    // `contents: read` was right while the sample only consumed the tree. It publishes
    // now, and a permission short of `write` fails that step and nothing else — the
    // suite still passes, and the loop above quietly stays open.
    RepoFile.block(RepoFile.block(leg, "sample"), "permissions") should include("contents: write")
  }
}
