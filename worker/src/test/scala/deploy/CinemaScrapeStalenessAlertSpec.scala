package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards the alerting over ROSTER FRESHNESS — whether every cinema is still
 * being scraped at all.
 *
 * The failure this covers is invisible to every other signal the fleet has.
 * `kinowo_worker_http_total{phase="scrape"}`, the handler durations, the queue
 * depth: all of them are counters a working scrape increments, so a cinema that
 * quietly stops being scraped emits nothing and a roster with a dark venue looks
 * identical to a healthy one. `CinemaScrapeCensus` publishes the two gauges that
 * do see it, and until this spec they were CHARTED (panels 58/59) and unwatched
 * — a graph nobody is looking at while a country's sweep falls behind, which is
 * exactly how DE ran ~4x over its window for weeks in July 2026 with every other
 * panel clean.
 *
 * THE RULES LIVE IN PROMETHEUS, NOT GRAFANA, and that is load-bearing rather
 * than stylistic. The sibling `Grafana*AlertSpec`s in this package all read
 * `fly/grafana/provisioning/alerting/alert-rules.yaml`, whose every rule queries
 * the `fly-prometheus` datasource — and that datasource has had no working
 * credential since 2026-08-29, so those rules evaluate to an execution error and
 * fall to Normal. A new rule written in that file would be born silent. The
 * local Prometheus on monitoring-1 scrapes all five workers directly by NodePort
 * (`infra/nix/files/monitoring/scrape-kinowo-apps.yaml`), so a rule there
 * actually runs.
 *
 * WHAT IS ASSERTED, AND WHAT IS DELIBERATELY NOT. The per-country THRESHOLDS are
 * pinned, because they are derived — 1.5x each country's deployed
 * `KINOWO_SCRAPE_FRESHNESS_MINUTES` — and a derived literal that nothing checks
 * drifts the moment the value it was derived from moves. That is not
 * hypothetical: the oldest-scrape PANEL misstated DE's window within an hour of
 * being written, which is why `WorkerScrapeCadenceConfigSpec` now generates its
 * sentence. The MULTIPLIER itself is a judgement about alert noise and is
 * asserted only to be uniform across countries, not to be any particular number.
 */
class CinemaScrapeStalenessAlertSpec extends AnyFlatSpec with Matchers {

  private val Rules      = "infra/nix/files/monitoring/rules/cinema-scrape.rules"
  private val RuleNames  = "infra/nix/modules/roles/prometheus.nix"
  private val RuleFiles  = "infra/nix/files/monitoring/prometheus.yaml"
  private val OverlayDir = "infra/kubernetes/worker/overlays"

  /** The wiggle room over each country's own sweep window. HEALTHY IS A SAWTOOTH
   *  RIDING JUST UNDER THAT WINDOW — cinemas fall due, get swept, and the maximum
   *  resets — so a rule firing at 1.0x would fire on healthy operation every
   *  cycle. One half-window of slack sits clear of the normal peak (DE measured
   *  9.6h against its 10h window on 2026-08-30 while running AHEAD of schedule)
   *  and still far inside the shape that has actually bitten, at >4x. */
  private val WiggleRoom = 1.5

  private lazy val rules = RepoFile.read(Rules)

  /** Every country that actually deploys a worker. Read from the overlay
   *  directory rather than listed, so a country onboarded tomorrow is covered by
   *  this spec the day it lands — an unwatched roster is the whole failure mode,
   *  and a new country is the likeliest way to acquire one. */
  private lazy val deployedCountries: Seq[String] =
    Option(new java.io.File(OverlayDir).listFiles())
      .getOrElse(Array.empty[java.io.File])
      .filter(_.isDirectory)
      .map(_.getName)
      .sorted
      .toSeq

  /** The per-country age thresholds the rule file actually spells, in seconds. */
  private lazy val thresholds: Map[String, Long] =
    """kinowo_worker_cinema_scrape_oldest_age_seconds\{country="([a-z]+)"\}\s*>\s*(\d+)""".r
      .findAllMatchIn(rules)
      .map(m => m.group(1) -> m.group(2).toLong)
      .toMap

  "the fleet" should "deploy at least one worker to watch" in {
    // Guards the guard: an empty overlay directory would make every per-country
    // assertion below vacuously true.
    deployedCountries should not be empty
  }

  "the cinema-scrape alert rules" should "watch the oldest-scrape gauge for every deployed country" in {
    deployedCountries.foreach { cc =>
      withClue(
        s"no alert clause for country=\"$cc\" in $Rules. Its roster is UNWATCHED: the scrape " +
          "counters cannot see a cinema that stopped being scraped, because a cinema nobody " +
          "scrapes increments nothing. Add a clause to CinemaScrapeOldestAgeHigh. "
      ) {
        thresholds.keySet should contain(cc)
      }
    }
  }

  it should "pin each country's threshold to its own deployed sweep window" in {
    // THE POINT OF THE WHOLE SPEC. The cadences are an order of magnitude apart —
    // 60min for PL against 840 for the US, because the US roster is ~5,000 venues
    // behind a measured 200ms pace and PL's is ~300 — so one shared threshold
    // would either page PL eleven hours late or page the US every cycle. Each
    // country is compared to ITS OWN window, and PromQL cannot read a ConfigMap,
    // so the literals are generated here from the same overlays the workers read.
    deployedCountries.foreach { cc =>
      val window = RepoFile
        .deployedFreshnessMinutes(cc)
        .getOrElse(fail(s"$OverlayDir/$cc/patch.yaml has no KINOWO_SCRAPE_FRESHNESS_MINUTES"))
      val expected = (window * 60 * WiggleRoom).toLong

      withClue(
        s"$cc deploys a ${window}min sweep window, so its clause in $Rules must read " +
          s"`> $expected` (${WiggleRoom}x, in seconds) and reads `> ${thresholds.getOrElse(cc, 0L)}`. " +
          "A threshold that no longer matches the cadence it was derived from is worse than none: " +
          "too low it pages on the healthy sawtooth until it is muted, too high it stays quiet " +
          "through the overrun it exists for. "
      ) {
        thresholds.get(cc) shouldBe Some(expected)
      }
    }
  }

  it should "give every country the same wiggle room, rather than tuning one quietly" in {
    // A per-country multiplier is how a country with a noisy sweep gets its alert
    // relaxed during an incident and left relaxed afterwards. If the sawtooth
    // genuinely does not fit, the CADENCE is the lever — `WorkerScrapeCadenceConfigSpec`
    // asserts the arithmetic for that — not this threshold.
    val ratios = deployedCountries.flatMap { cc =>
      for {
        window    <- RepoFile.deployedFreshnessMinutes(cc)
        threshold <- thresholds.get(cc)
      } yield threshold.toDouble / (window * 60)
    }

    withClue(s"the per-country thresholds in $Rules imply different multipliers: ${ratios.distinct}. ") {
      ratios.distinct should have size 1
    }
  }

  it should "also alert on cinemas that have NEVER scraped" in {
    // The hole the age rule is structurally blind to. A cinema with no successful
    // scrape carries NO age — the census counts it separately rather than folding
    // an unknown age in as a zero one — so the venue that has been dark LONGEST
    // moves `oldest_age_seconds` not at all.
    withClue(
      s"$Rules has no rule reading kinowo_worker_cinema_scrape_never_scraped. Without it the " +
        "staleness alerting is silent on exactly the worst case: a venue added to the roster and " +
        "never once fetched. "
    ) {
      rules should include("kinowo_worker_cinema_scrape_never_scraped")
    }
  }

  it should "wait out a cold start before calling a cinema never-scraped" in {
    // Every cinema is legitimately never-scraped on a fresh worker until the first
    // sweep reaches it, so this `for:` has to clear the SLOWEST country's alerting
    // horizon or a rollout pages. Asserted as the invariant rather than the number,
    // so onboarding a country slower than the US fails here instead of quietly
    // turning every deploy into an alert.
    val grace = """(?s)alert:\s*CinemaScrapeNeverScraped.*?\n\s*for:\s*(\d+)h""".r
      .findFirstMatchIn(rules)
      .map(_.group(1).toInt)
      .getOrElse(fail(s"CinemaScrapeNeverScraped in $Rules has no `for:` in whole hours"))

    val slowestHorizonHours =
      deployedCountries.flatMap(RepoFile.deployedFreshnessMinutes).max * WiggleRoom / 60

    withClue(
      s"CinemaScrapeNeverScraped waits ${grace}h, but the slowest country's alerting horizon is " +
        f"$slowestHorizonHours%.1fh. A shorter wait pages on every rollout of that country's worker. "
    ) {
      grace.toDouble should be >= slowestHorizonHours
    }
  }

  it should "have an absent() companion, so a stopped census cannot read as a fresh roster" in {
    // The house rule throughout nix/files/monitoring/rules: a rule that can only
    // fire on a series is paired with one that fires on the series' ABSENCE.
    // Both rules above are the quiet kind — a worker that stopped publishing its
    // census gauges would leave them permanently Normal, which looks exactly like
    // health.
    rules should include("absent(kinowo_worker_cinema_scrape_oldest_age_seconds)")
  }

  "the cinema-scrape rule file" should "be both INSTALLED and LOADED" in {
    // A RULE FILE HAS TO BE IN TWO PLACES, as roles/prometheus.nix says at length:
    // `ruleNames` installs it into /etc, `rule_files` in prometheus.yaml loads it.
    // Present in one and absent from the other it is either a file nothing reads —
    // silent, the alerts simply never fire — or a path Prometheus cannot find, in
    // which case it REFUSES TO START and takes the fleet's monitoring with it.
    // That comment asks for a check script this repository does not have; for this
    // file, this is it.
    withClue(s"$RuleNames's `ruleNames` does not install cinema-scrape, so the rules never reach /etc: ") {
      RepoFile.read(RuleNames) should include("\"cinema-scrape\"")
    }
    withClue(s"$RuleFiles's `rule_files` does not load cinema-scrape, so Prometheus never reads it: ") {
      RepoFile.read(RuleFiles) should include("/etc/prometheus/rules/cinema-scrape.rules")
    }
  }
}
