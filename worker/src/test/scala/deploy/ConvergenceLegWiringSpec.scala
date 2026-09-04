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
 * called once per country. That run is ONE job with a matrix the gate plans: a single
 * row for a country that folds order-independence into its standard run, and a second
 * row for the one whose corpus outgrew a single job. That is only correct while three
 * things hold, and none of them is visible at a glance in the YAML:
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
    val orderDefault = Seq(("leg default (order)",
      defaultOf("order-job-timeout-minutes"), defaultOf("order-suite-timeout-minutes")))
    val perCountry = rows.flatMap { fields =>
      val country = fields("country")
      Seq((s"$country full",   fields("job").toInt,       fields("suite").toInt),
          (s"$country sample", fields("sampleJob").toInt, fields("sampleSuite").toInt)) ++
        // Only the country that splits its order-independence replay out declares these.
        fields.get("orderJob").map(job => (s"$country order", job.toInt, fields("orderSuite").toInt))
    }
    defaults ++ orderDefault ++ perCountry
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

  /** Both jobs publish through the same composite action, so neither can drift from the
   *  other on what "publish the tree" means. */
  private val PublishAction = "uses: ./.github/actions/convergence-publish"

  /** And every job that RUNS a suite renders its findings through one, for the same
   *  reason: the report is a filter over stdout deciding what counts as narration, and a
   *  second copy that fell behind would quietly stop rendering the phase timings that are
   *  the only way to read a leg while it is still running. */
  private val FindingsAction = "uses: ./.github/actions/convergence-findings"

  /** The jobs that publish the tree. The full run's replay ROW is deliberately not one
   *  of them — see the rule below that pins it. */
  private val Jobs = Seq("sample", "convergence")

  /** The countries that run their order-independence replay as a row of its own rather
   *  than inside the full leg, as country → that row's sbt alias. */
  private lazy val splitOrder: Map[String, String] =
    rows.flatMap(fields => fields.get("order").map(fields("country") -> _)).toMap

  /** The ScalaTest tag the split filters on, spelled once. */
  private val OrderTag = "services.movies.OrderIndependence"

  /** What `addCommandAlias("<name>", …)` maps `name` to. Read by name rather than
   *  matched as a whole line, so the alias table stays free to align its columns. */
  private def aliasBody(alias: String): String =
    ("addCommandAlias\\(\"" + alias + "\",\\s*\"([^\"]*)\"").r.findFirstMatchIn(build)
      .getOrElse(fail(s"build.sbt defines no `$alias` alias")).group(1)

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
    val packer = RepoFile.read(".github/scripts/pack-enrichment-tree.sh")
    packer should include("packed=$?")
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
    // now, and a permission short of `write` fails that step and nothing else — the
    // suite still passes, and the loop above quietly stays open.
    RepoFile.block(RepoFile.block(leg, "sample"), "permissions") should include("contents: write")
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
      val full  = aliasBody(fullAlias)
      val order = aliasBody(orderAlias)
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
   * So the second run is a matrix ROW the sample plans, not a job. A country that folds
   * order-independence into its standard run expands to one row and renders nothing
   * extra; the United States expands to two.
   */
  "the order-independence split" should "render nothing for a country that folds it into the standard run" in {
    withClue("a third job would be rendered, skipped, for every country that doesn't split: ") {
      legJobs shouldBe Seq("sample", "convergence")
    }
    legDirectives should not include "if: inputs.order-command"
    RepoFile.block(leg, "convergence") should
      include("include: ${{ fromJson(needs.sample.outputs.runs) }}")
  }

  /** Both rows carry their OWN budgets — the caller's `orderJob`/`orderSuite` — and a
   *  row that fell back to the full leg's would be the 135/120 that cancelled the US
   *  leg before its heap was raised. */
  it should "plan the replay row with its own alias and its own budgets" in {
    val plan = RepoFile.block(leg, "sample")
    Seq("inputs.order-command", "inputs.order-job-timeout-minutes",
        "inputs.order-suite-timeout-minutes").foreach { input =>
      withClue(s"$input: ") { plan should include(input) }
    }
    withClue("the rows' ceilings must be the row's, not the job's one input: ") {
      RepoFile.block(leg, "convergence") should include("timeout-minutes: ${{ matrix.job }}")
      RepoFile.block(leg, "convergence") should include("timeout-minutes: ${{ matrix.suite }}")
    }
  }

  /** Side by side off the sample, not behind the full leg: chaining a 4-hour row to a
   *  3-hour one is the 6-hour cancellation the split exists to escape — and one row's
   *  failure must not cancel the other's answer. */
  it should "run its rows off the sample, independently of each other" in {
    val block = RepoFile.block(leg, "convergence")
    block should include("needs: sample")
    block should include("fail-fast: false")
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
    RepoFile.block(leg, "convergence") should include(s"$PublishAction\n              if: always() && matrix.publish")
    withClue("exactly one planned row may publish: ") {
      val plan = RepoFile.block(leg, "sample")
      plan should include("\"publish\":true")
      plan should include("\"publish\":false")
    }
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
}
