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
 * The fix is one reusable workflow holding a country's sample and the run behind it,
 * called once per country. That run is ONE job with a matrix: a single row for a country
 * that folds order-independence into its standard run, and a second row for the one whose
 * corpus outgrew a single job. Every row runs the sample as its first suite step, so a red
 * sample fails the step and the suite behind it never starts — the gate a separate
 * `sample` job used to be, without its second checkout and setup. That is only correct
 * while three things hold, and none of them is visible at a glance in the YAML:
 *
 *   - the pair is genuinely chained (the sample step ahead of the suite step, in the
 *     same job, without `continue-on-error` outside a recording) inside the called
 *     file, where "the sample" can only mean this country's;
 *   - the caller's matrix pairs each country's full alias with the SAME
 *     country's sample alias — a mis-paired row would gate Germany's leg on
 *     Poland's sample and still be green;
 *   - the called file declares no `concurrency:` of its own. All three calls
 *     live in one run, so a group named there would be shared and the legs would
 *     cancel each other. The lane belongs to the caller. (See
 *     [[ConvergenceConcurrencyConfigSpec]].)
 */
class ConvergenceLegWiringSpec extends AnyFlatSpec with Matchers {
  private lazy val caller   = RepoFile.read(".github/workflows/identity-model-convergence.yml")
  /** Every verdict caller of the leg. The United States ran from a build of its own
   *  (`us-convergence.yml`) until its legs went hermetic; since 2026-09-30 it is a row of
   *  the shared caller, so every rule below reads that one file. */
  private lazy val callers  = Seq(caller)
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
  private def matrixRows(yaml: String): Seq[Map[String, String]] = RepoFile.matrixRows(yaml)

  private lazy val rows: Seq[Map[String, String]] = callers.flatMap(matrixRows)

  private lazy val countries: Map[String, (String, String)] =
    rows.map(fields => fields("country") -> (fields("cmd"), fields("sample"))).toMap

  /** Every (job ceiling, step ceilings inside it) budget the legs actually run on, labelled.
   *
   *  The numbers used to be literals inside the leg's two job blocks. They are per-COUNTRY
   *  now — the United States carries larger ceilings than the others — which means the
   *  pairs live in the caller's matrix, with the leg's `default:`s standing in for a
   *  caller that says nothing. Both sources are checked, because a gap that closes in
   *  either place cancels a leg just as dead.
   *
   *  The steps are the sample's AND the suite's: the sample runs first in every row's job
   *  (there is no sample job of its own), so each row's ceiling has to clear the two
   *  together — GitHub expressions have no arithmetic, so the leg cannot add them itself. */
  private def budgetPairs: Seq[(String, Int, Int)] = {
    val sampleDefault = defaultOf("sample-suite-timeout-minutes")
    val defaults = Seq(
      ("leg default (full)",  defaultOf("job-timeout-minutes"),       defaultOf("suite-timeout-minutes") + sampleDefault),
      ("leg default (order)", defaultOf("order-job-timeout-minutes"), defaultOf("order-suite-timeout-minutes") + sampleDefault))
    val perCountry = rows.flatMap { fields =>
      val country = fields("country")
      val sample  = fields("sampleSuite").toInt
      Seq((s"$country full", fields("job").toInt, fields("suite").toInt + sample)) ++
        // Only the country that splits its order-independence replay out declares these.
        fields.get("orderJob").map(job => (s"$country order", job.toInt, fields("orderSuite").toInt + sample))
    }
    defaults ++ perCountry
  }

  /** The `default:` under one of the leg's `workflow_call` inputs. */
  private def defaultOf(input: String): Int =
    s"$input:[\\s\\S]*?default:\\s*(\\d+)".r.findFirstMatchIn(leg)
      .getOrElse(fail(s"no default declared for input `$input` in the leg workflow"))
      .group(1).toInt

  /** The leg's job ids in file order — every one of which GitHub renders, for every
   *  country that calls this file, whether or not it has anything to do. */
  private lazy val legJobs: Seq[String] =
    leg.linesIterator.dropWhile(_.trim != "jobs:")
      .collect { case line if """^ {4}([\w-]+):\s*$""".r.matches(line) => line.trim.dropRight(1) }
      .toSeq

  /** The leg with its commentary stripped — for rules about what the workflow DOES, which
   *  a comment explaining why it no longer does it would otherwise fail. */
  private lazy val legDirectives: String =
    leg.linesIterator.filterNot(_.trim.startsWith("#")).mkString("\n")

  /** The one composite action that publishes the tree. */
  private val PublishAction = "uses: ./.github/actions/convergence-publish"

  /** And every job that RUNS a suite renders its findings through one, for the same
   *  reason: the report is a filter over stdout deciding what counts as narration, and a
   *  second copy that fell behind would quietly stop rendering the phase timings that are
   *  the only way to read a leg while it is still running. */
  private val FindingsAction = "uses: ./.github/actions/convergence-findings"

  /** The countries that run their order-independence replay as a row of its own rather
   *  than inside the full leg, as country → that row's sbt alias. */
  private lazy val splitOrder: Map[String, String] =
    rows.flatMap(fields => fields.get("order").map(fields("country") -> _)).toMap

  /** The ScalaTest tag the split filters on, spelled once. */
  private val OrderTag = "services.movies.OrderIndependence"

  /** The spec an alias' `testOnly` names, before any `--` runner flags. */
  private def specOf(aliasBody: String): String = aliasBody.split(" -- ").head

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

  /** Once, from the shared caller: the US ran from a build of its own until its legs went
   *  hermetic, and a second US row or a revived second caller would run the country twice
   *  per push, each holding 10g and three runners. */
  it should "run the United States once, from the shared caller" in {
    matrixRows(caller).map(_("code")).count(_ == "us") shouldBe 1
    RepoFile.exists(".github/workflows/us-convergence.yml") shouldBe false
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
      // A full row whose order-independence replay runs in a row of its own is the same country's suite.
      withClue(s"$country: ") { sample shouldBe s"${command.stripSuffix("WithoutOrder")}Sample" }
    }
  }

  it should "name only aliases the build actually defines" in {
    countries.values.flatMap { case (command, sample) => Seq(command, sample) }.foreach { alias =>
      withClue(s"$alias: ") { build should include("addCommandAlias(\"" + alias + "\"") }
    }
  }

  /** The sample step, first in every row. */
  private val SampleStep = "Run the ${{ inputs.country }} sample ahead of the suite"
  private val SuiteStep  = "Run the ${{ inputs.country }} ${{ matrix.phase }} suite"

  /** The gate is the step ORDER inside one job: a failed step skips every later step that
   *  is not `always()`/`failure()`, so a red sample stops the suite exactly as a red sample
   *  JOB stopped the job behind it — provided the sample neither continues on error (only a
   *  recording does, to record the whole corpus past it) nor is skipped in a row. */
  "the single-country leg workflow" should "run its full suite behind its own sample, in the same job" in {
    val convergence = RepoFile.block(leg, "convergence")
    val sample = RepoFile.step(convergence, SampleStep)
    convergence.indexOf(s"- name: $SampleStep") should be < convergence.indexOf(s"- name: $SuiteStep")
    sample should include("continue-on-error: ${{ inputs.mode == 'record' }}")
    withClue("a convergence row whose sample is skipped runs its suite ungated — only a leg that asks for a " +
             "`sample` row of its own (`sample-row`) may move the sample out of it: ") {
      sample.linesIterator.map(_.trim).filter(_.startsWith("if:")).toSeq shouldBe
        Seq("if: matrix.phase == 'sample' || (matrix.phase == 'convergence' && !inputs.sample-row)")
    }
    withClue("the suite must not run past a failed sample — an `if:` without a status function keeps the " +
             "implicit success(), and this one only spares a recording's sample row: ") {
      RepoFile.step(convergence, SuiteStep).linesIterator.map(_.trim).filter(_.startsWith("if:")).toSeq shouldBe
        Seq("if: matrix.phase != 'sample'")
    }
  }

  /** The separate `sample` job cost every hermetic and overlay leg a second checkout and
   *  convergence-setup (~1 minute) ahead of the full leg, and one more runner per country
   *  per push. */
  it should "hold no sample job, and no `needs:` edge for a row to wait on" in {
    legJobs shouldBe Seq("convergence")
    legDirectives should not include "needs:"
    legDirectives should not include "needs.sample"
  }

  it should "leave the concurrency lane to the caller, so its three calls don't cancel each other" in {
    leg.linesIterator.map(_.trim).toList should not contain "concurrency:"
  }

  it should "keep EVERY job's ceiling clear of the sample and suite steps it wraps" in {
    // A job that hits `timeout-minutes` is CANCELLED, and a cancelled job runs its
    // `always()` publish steps only inside a short grace window — so a leg that
    // overruns discards the very capture that would have made the next run fast
    // enough not to overrun. The gap between the two numbers is what pays for setup
    // and the publishes; raising the step without the job reintroduces exactly that.
    //
    // The rule was written for the full leg and applied only there, and the sample —
    // which had no step ceiling at all, so it could only ever be cancelled — is the
    // job that then spent ten consecutive runs discarding its own progress. The sample
    // now runs inside every row's job, so its ceiling counts against every row's.
    budgetPairs.foreach { case (label, ceiling, steps) =>
      withClue(s"$label: job $ceiling, sample + suite steps $steps: ") {
        ceiling should be > steps
        ceiling - steps should be >= 10
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
   *  Every module writes its JUnit XML into the one root-level `target/test-reports/unit/`
   *  (build.sbt's `unitReportSettings`), and the leg's sbt cache used to restore the root
   *  `target/` whole, falling through to the key `ci.yml`'s unit job saves. So a leg began
   *  with a report directory full of specs from another workflow, and the publish step
   *  globs that directory. The United States' leg died on an OOM before ScalaTest reported
   *  a result, and its check-run went green naming `CinemaScraperCatalogSpec`.
   *
   *  Both halves are load-bearing and neither works alone: a cache that leaves the root
   *  `target/` behind without `require_tests` turns the lie into a shrug, and
   *  `require_tests` over a restored directory still reports somebody else's passes. */
  it should "report only the tests THIS leg ran, and admit it when there are none" in {
    val cachePaths = RepoFile.read(".github/actions/convergence-setup/action.yml").linesIterator
      .dropWhile(_.trim != "path: |").drop(1).map(_.trim).takeWhile(_.nonEmpty).takeWhile(!_.contains(":")).toSeq
    withClue("the cache must carry the module classes and NOT the root target, which holds another job's test reports: ") {
      cachePaths should (contain("*/target/scala-*") and not contain "target")
    }
    withClue("a leg that produced no report must fail the check rather than skip it: ") {
      RepoFile.block(leg, "convergence") should include("require_tests: true")
    }
  }

  it should "publish what a recording's sample recorded, not just what the full leg did" in {
    // The gate REPLAYS a fixture tree, and every recorded response in that tree expires
    // after `EnrichmentFreshness.Ttl` (5 days). Only the full leg republished it, and the
    // full leg is `needs: sample` — so a country whose sample failed for five days had its
    // tree pruned to nothing, which made the sample slower still, which kept the full leg
    // from ever running again. Germany sat in exactly that loop for ten runs from
    // 2026-08-09: an asset that could only be refreshed by a job that could only run once
    // the asset was fresh.
    //
    // Recording is the recorder's alone now, and every leg runs its sample as the first
    // suite step of the full leg's job — so that job's publish, under `always()`, is what
    // carries the sample's recordings, red sample or green.
    val convergence = RepoFile.block(leg, "convergence")
    convergence should include(s"- name: $SampleStep")
    convergence should include(s"$PublishAction\n              if: always()")
    convergence.indexOf(s"- name: $SampleStep") should be < convergence.indexOf(PublishAction)
  }

  it should "keep the capture when tar reports the tree changing under it" in {
    // The publish runs on `always()`, so its most valuable case is a leg that just
    // ran out of time — and the runner reaps that leg's JVM in its POST-job phase,
    // well after this step. `RecordingHttpFetch` is therefore still writing into the
    // tree being read; GNU tar prints "file changed as we read it" and exits 1, a
    // warning status beside a complete archive. Under `set -e` that failed the step
    // and discarded the whole capture — the exact trap the publish exists to close,
    // and it cost Germany's first full leg in a week its entire corpus capture.
    val packer = RepoFile.read(".github/scripts/pack-enrichment-tree.sh")
    // tar's OWN status, out of the pipe into the compressor — and the compressor's graded apart.
    packer should include("statuses=(\"${PIPESTATUS[@]}\")")
    packer should include("packed=${statuses[0]}")
    packer should include("""if [ "$compressed" -ne 0 ]""")
    withClue("tar's warning status (1) must not fail the step; 2+ still must: ") {
      packer should include("""if [ "$packed" -gt 1 ]""")
    }
    withClue("the publish must pack through that script, not a copy of it: ") {
      RepoFile.read(".github/actions/convergence-publish/action.yml") should
        include(".github/scripts/pack-enrichment-tree.sh")
    }
  }

  it should "let the sample write the release it now publishes to" in {
    // `contents: read` was right while the sample only consumed the tree. It publishes
    // now (a recording's tree, an overlay leg's overlay), and a permission short of
    // `write` fails that step and nothing else — the suite still passes, and the loop
    // above quietly stays open.
    RepoFile.block(RepoFile.block(leg, "convergence"), "permissions") should include("contents: write")
  }

  /**
   * The United States' order-independence replay, split out of the full leg.
   *
   * The full leg boots the corpus in 167 minutes; the three concurrent whole-corpus
   * replays cost ~1.5x a boot again (the UK's measured ratio — 2,586s of replays behind
   * a 1,676s boot — applied to a 10,027s one). Together that is 5.5 hours in a job
   * GitHub cancels at 6, and every US leg to that point had died inside the replays'
   * own guard having diverged on nothing.
   *
   * The split is only real if BOTH aliases hold up their end: the full leg must EXCLUDE
   * the tag, and the order leg must run exactly it. An alias that drops one of the two
   * flags is silent — the full leg quietly goes back to five and a half hours, or the
   * claim stops being checked on this country at all.
   */
  "the order-independence split" should "run the tagged test in the order leg and nowhere else in the full one" in {
    splitOrder should not be empty
    splitOrder.foreach { case (country, orderAlias) =>
      val fullAlias = countries(country)._1
      val full  = RepoFile.commandAlias(fullAlias)
      val order = RepoFile.commandAlias(orderAlias)
      withClue(s"$country full leg ($fullAlias) = `$full`: ") {
        full should include(s"-l $OrderTag")
        full should not include s"-n $OrderTag"
      }
      withClue(s"$country order leg ($orderAlias) = `$order`: ") {
        order should include(s"-n $OrderTag")
        order should not include s"-l $OrderTag"
      }
      withClue(s"$country's two legs must run the same spec: ") {
        specOf(order) shouldBe specOf(full)
      }
    }
  }

  /** The tag has to exist as a ScalaTest `Tag` whose name matches the one the aliases
   *  filter on. A typo either side is not an error — `-l` on a name nothing carries
   *  excludes nothing, and `-n` on one selects nothing and reports a green run of zero
   *  tests, which `require_tests` catches only because the leg writes no XML at all. */
  it should "filter on a tag the e2e module actually defines" in {
    RepoFile.read("e2e/src/test/scala/services/movies/OrderIndependence.scala") should
      include("""Tag("services.movies.OrderIndependence")""")
  }

  /**
   * The split costs the countries that DON'T split nothing at all.
   *
   * "A job that exists only sometimes" is not something GitHub can express: a job
   * carrying `if: inputs.order-command != ''` is still a job, rendered and skipped in
   * every run and posted as a skipped check-run on every commit. Four warm countries
   * times every push was four `order-independence` entries that meant nothing, on the
   * page where the ones that do mean something are read.
   *
   * So the second run is a matrix ROW, not a job. A country that folds
   * order-independence into its standard run expands to one row and renders nothing
   * extra; the United States expands to two.
   */
  "the order-independence split" should "render nothing for a country that folds it into the standard run" in {
    withClue("a third job would be rendered, skipped, for every country that doesn't split: ") {
      legJobs shouldBe Seq("convergence")
    }
    legDirectives should not include "if: inputs.order-command"
    RepoFile.block(leg, "convergence") should include(
      """phase: ${{ fromJson(format('["convergence"{0}{1}]', inputs.order-command != '' && ',"order-independence"' || '', """ +
        """inputs.sample-row && ',"sample"' || '')) }}""")
  }

  /**
   * The matrix carries the PHASE and nothing else.
   *
   * The first cut had the sample plan whole rows — alias, both budgets and a publish
   * flag — and publish them as a JSON output the full run expanded. It worked, and it
   * moved four numbers out of the `workflow_call` inputs, where every other budget in
   * this file is declared with a default and a description and is read by the rules
   * above. A phase-only matrix keeps them there: the row picks its alias and its
   * ceilings off the same inputs the caller already passes.
   */
  it should "carry only the phase in its matrix, and pick the rest off the leg's inputs" in {
    val block = RepoFile.block(leg, "convergence")
    block should include("name: ${{ matrix.phase }}")
    // The MATRIX is planned off the inputs alone — never off another job's outputs.
    RepoFile.block(block, "strategy") should not include "needs."
  }

  /** Both rows carry their OWN budgets — the caller's `orderJob`/`orderSuite` — and a
   *  row that fell back to the full leg's would be the 135/120 that cancelled the US
   *  leg before its heap was raised. */
  it should "run the replay row on its own alias and its own budgets" in {
    val block = RepoFile.block(leg, "convergence")
    Seq("inputs.order-command", "inputs.order-job-timeout-minutes",
        "inputs.order-suite-timeout-minutes").foreach { input =>
      withClue(s"$input: the replay row would fall back to the full leg's: ") {
        block should include(input)
      }
    }
  }

  /** Side by side, not behind the full leg: chaining a 4-hour row to a 3-hour one is the
   *  6-hour cancellation the split exists to escape — and one row's failure must not cancel
   *  the other's answer.
   *
   *  ONLY the convergence row runs the sample. No row can wait on a step of another row, so the
   *  order row would either run its own copy or run ungated; its own copy sat 97 s in front of the
   *  identity lane's critical path (run 37144949783), so it runs ungated — a red sample then costs
   *  that row's runner, never wall time, and the convergence row still gates and reports. */
  it should "run the sample in the convergence row only, never ahead of the order row's suite" in {
    val block = RepoFile.block(leg, "convergence")
    block should include("fail-fast: false")
    RepoFile.step(block, SampleStep) should include(
      "if: matrix.phase == 'sample' || (matrix.phase == 'convergence' && !inputs.sample-row)")
  }

  /** ONE writer to the rolling release per leg.
   *
   *  The full run's two rows run concurrently and finish into the same
   *  `enrichment-<code>.tar.gz`, and `gh release upload --clobber` is a last-writer-wins
   *  overwrite of a 428 MB asset — two in flight is how the next run restores a
   *  truncated tree. The replay row has nothing to publish anyway: its replays share the
   *  preloaded cache, and the leg that recorded that cache measured 4 live fills across
   *  the whole run. */
  it should "leave the publish to the row that is not racing another for it" in {
    RepoFile.block(leg, "convergence") should
      include(s"$PublishAction\n              if: always() && matrix.phase == 'convergence' && inputs.mode != 'overlay'")
  }

  /**
   * Every job in the leg checks the repo out and restores the fixture tree from the
   * rolling release, and both of those read `contents`.
   *
   * Naming a `permissions:` block sets every scope it OMITS to `none` — it is a
   * replacement, not an addition — so a job that lists `checks: write` and forgets
   * `contents` has not inherited read, it has revoked it, and `actions/checkout` fails
   * on the first step with a 403 that reads as a token problem rather than a config one.
   */
  it should "grant every job the contents read its checkout and fixture restore need" in {
    legJobs.foreach { job =>
      withClue(s"$job: ") {
        RepoFile.block(leg, job) should include regex """\bcontents:\s*(read|write)\b"""
      }
    }
  }

  it should "render every suite job's findings through the one report" in {
    RepoFile.block(leg, "convergence") should include(FindingsAction)
  }

  /** A country that names an order row must name its budgets too, and vice versa — a
   *  half-declared row inherits the warm countries' 135/120 for a job that needs hours,
   *  which is the exact shape that cancelled the US leg before its heap was raised. */
  it should "declare a budget for every order row, and an order row for every budget" in {
    rows.foreach { fields =>
      withClue(s"${fields("country")}: ") {
        fields.contains("order") shouldBe fields.contains("orderJob")
        fields.contains("order") shouldBe fields.contains("orderSuite")
      }
    }
  }

  /** The enrichment tree is zstd since the packer moved off gzip, but the release still holds gzip
   *  pairs pinned before that (and the scrape corpus is gzip) — so every reader fetches the tree by
   *  a `.tar.*` glob and unpacks by the archive's magic (`unpack-fixture-archive.sh`, which inflates
   *  gzip through pigz). One `tar -xzf` left anywhere fails on the first zstd tree it meets. */
  "the convergence setup" should "fetch the tree by either compressor's name and unpack it by its magic" in {
    val setup  = RepoFile.read(".github/actions/convergence-setup/action.yml")
    val unpack = RepoFile.step(setup, "Unpack whichever fixtures are present")
    unpack should include(".github/scripts/unpack-fixture-archive.sh")
    unpack should include("set -euo pipefail")
    unpack should not include "tar -xzf"
    setup should include("KINOWO_CONVERGENCE_TREE_ASSET=enrichment-${{ inputs.code }}.tar.*")
    setup should not include "enrichment-${{ inputs.code }}.tar.gz"
    RepoFile.read(".github/scripts/unpack-fixture-archive.sh") should include("command -v pigz")
  }

  it should "leave no reader of the tree assuming gzip" in {
    Seq(".github/workflows/identity-decorations.yml", "scripts/hard-clusters.sh", "scripts/convergence-local.sh").foreach { file =>
      val text = RepoFile.read(file)
      withClue(s"$file: ")(text should include("unpack-fixture-archive.sh"))
      withClue(s"$file still names a gzip tree: ")(text should not include regex("""enrichment-\$[^ ]*\.tar\.gz"""))
    }
    RepoFile.read(".github/workflows/identity-measure.yml") should include("-$RECORDING.tar.*")
  }

  it should "publish the tree as zstd, retire the gzip working asset, and prune pinned pairs of either kind" in {
    val publish = RepoFile.read(".github/actions/convergence-publish/action.yml")
    publish should include("enrichment-upload/enrichment-${{ inputs.code }}.tar.zst")
    publish should include("pinned=\"enrichment-$code-$KINOWO_CONVERGENCE_CORPUS_RUN.tar.zst\"")
    publish should include("""legacy="enrichment-${{ inputs.code }}.tar.gz"""")
    publish should include("delete-asset \"$TAG\" \"$legacy\"")
    publish should include("""\\.tar\\\\.(gz|zst)$""")
  }

  /** The leg's MongoDB starts in the BACKGROUND (~20 s of image pull, boot and election that the JDK,
   *  caches and fixture unpack overlap), so setup must end by waiting for it and failing as the start
   *  would have — or the suite would run against a server that is not up yet. */
  it should "start MongoDB in the background and wait for it before it ends" in {
    val setup = RepoFile.read(".github/actions/convergence-setup/action.yml")
    val steps = setup.linesIterator.map(_.trim).filter(_.startsWith("- name:")).toSeq
    setup should include("scripts/ci/in-background.sh start mongo-start \"$GITHUB_WORKSPACE\"/scripts/ci/start-mongo-replset.sh")
    steps.last shouldBe "- name: Wait for MongoDB, started in the background above"
    RepoFile.step(setup, "Wait for MongoDB, started in the background above") should include(
      "scripts/ci/in-background.sh wait mongo-start 240")
  }

  /** The enrichment tree's download and unpack (13-19 s, run 37105119296) overlap the JDK, sbt
   *  and build-cache restores rather than queueing behind them — unpacked OUTSIDE the workspace,
   *  because those restores' keys glob it and 130k tree files would cost them more than the
   *  overlap saves — and are waited for, and moved in, before anything reads the tree. */
  it should "fetch the enrichment tree in the background, outside the workspace, before the JDK and caches" in {
    val setup = RepoFile.read(".github/actions/convergence-setup/action.yml")
    val steps = setup.linesIterator.map(_.trim).filter(_.startsWith("- ")).toSeq
    def at(fragment: String) = steps.indexWhere(_.contains(fragment))
    RepoFile.step(setup, "Fetch the enrichment tree in the background") should include(
      "scripts/ci/in-background.sh start tree-restore \"$GITHUB_WORKSPACE\"/.github/scripts/restore-enrichment-tree.sh " +
        "\"${{ inputs.code }}\" \"${{ inputs.mode }}\" \"$RUNNER_TEMP/tree-stage\"")
    withClue("the pair it fetches is resolved first: ")(at("Resolve the recorded pair") should be < at("Fetch the enrichment tree"))
    at("Fetch the enrichment tree") should be < at("./.github/actions/setup-jdk")
    val unpack = RepoFile.step(setup, "Unpack whichever fixtures are present")
    unpack should include("scripts/ci/in-background.sh wait tree-restore")
    unpack should include("""stage="$RUNNER_TEMP/tree-stage/test/resources/fixtures"""")
    withClue("moved in before the overlay is laid over it: ")(at("Unpack whichever fixtures") should be < at("Unpack the identity model's overlay"))
  }

  /** The prod-Mongo tunnel's `socat` is not on the runner image, and installing it inside the
   *  tunnel step was 9-17 s of every recording's critical path (run 37105119296). Setup starts it
   *  at the top of a RECORDING job, beside the JDK and caches, and the tunnel waits for the rest —
   *  falling back to installing it itself, so a chore that never ran costs seconds, not the leg. */
  it should "install a recording's socat in the background, and have the tunnel wait for it" in {
    val setup  = RepoFile.read(".github/actions/convergence-setup/action.yml")
    val socat  = RepoFile.step(setup, "Install socat for the prod-Mongo tunnel, in the background")
    socat should include("if: inputs.mode == 'record'\n")
    socat should include("scripts/ci/in-background.sh start socat-install \"$GITHUB_WORKSPACE\"/scripts/ci/install-apt-package.sh socat")
    val steps = setup.linesIterator.map(_.trim).filter(_.startsWith("- ")).toSeq
    withClue("started before the JDK and caches it is meant to overlap: ") {
      steps.indexWhere(_.contains("Install socat")) should be < steps.indexWhere(_.contains("./.github/actions/setup-jdk"))
    }
    val tunnel = RepoFile.read("scripts/ci/wait-for-mongo-tunnel.sh")
    val waits  = tunnel.indexOf("in-background.sh\" wait socat-install")
    withClue("the tunnel waits for the background install: ")(waits should be >= 0)
    withClue("...and still installs socat itself when that never ran or failed: ") {
      tunnel.indexOf("install-apt-package.sh\" socat") should be > waits
    }
  }

  /** A recording leg's identity lookup sweep is minutes of live lookups; untimed, it read as a
   *  silent gap between `reloadReadModel` and `boot complete`, and nobody could tell whether a
   *  change to it helped (recording 37063331034). */
  "a convergence leg" should "time its identity lookup sweep as a phase of its own" in {
    RepoFile.read("e2e/src/test/scala/services/movies/CountryConvergenceBehaviour.scala") should include(
      """step("identityLookupSweep")(IdentityLookupSweep.over(""")
  }

  /** Under `pipefail`, a `| head -N` that closes the pipe early fails the writer before it with
   *  SIGPIPE: Spain's green recording leg failed its findings step (exit 4) once its report passed
   *  150 lines (run 37096642753). The report must keep its first lines by READING to the end. */
  "the convergence findings report" should "never cut its summary with an early-closing head" in {
    val commands = RepoFile.read(".github/actions/convergence-findings/action.yml").linesIterator
      .filterNot(_.trim.startsWith("#")).mkString("\n")
    commands should not include regex ("""\|\s*head\s+-[0-9]+""")
    commands should include("sed -n 's/^\\[info\\] //; 1,150p'")
  }
}
