package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards the per-worker credit-backoff wiring against per-country drift.
 *
 * The worker's AUTHORITATIVE self-throttle is `CpuCreditPoller`, which polls
 * Fly Prometheus for its OWN `FLY_APP_NAME` — that path is per-app by
 * construction and needs no guarding. What this spec protects is the
 * independent Grafana BACKSTOP: the `kinowo-worker-credit-low` rule POSTs a
 * webhook to the worker's `/throttle` endpoint when the poller is unavailable
 * (no `KINOWO_FLY_PROM_TOKEN`, Fly API down).
 *
 * That backstop cannot be made country-blind the way the dashboard matchers
 * were, because Grafana's webhook integration does NOT support notification
 * templates in the `url` field — only `title`/`message`. The target worker
 * therefore has to be resolved at ROUTING time, which forces a triple per
 * worker app: a contact point holding that app's `.internal` URL, a route
 * matching that app's label, and a rule matcher wide enough to emit an alert
 * instance for it.
 *
 * Miswiring is silent and actively harmful, which is why this is a test rather
 * than a checklist line: `ExternalThrottleGate.parse` reads ONLY the top-level
 * `status` and ignores labels entirely, so a webhook delivered to the wrong
 * worker throttles the wrong fleet with no error anywhere. Until 2026-07-18 the
 * whole backstop was `app="kinowo-worker"` — UK and DE had no second layer at
 * all under a poller outage.
 *
 * The worker set is derived from `fly.worker*.toml` (see [[RepoFile]]), so
 * onboarding a country automatically tightens what this demands.
 */
class GrafanaWorkerThrottleCoverageSpec extends AnyFlatSpec with Matchers {

  private val AlertRules   = "fly/grafana/provisioning/alerting/alert-rules.yaml"
  private val ContactPoints = "fly/grafana/provisioning/alerting/contact-points.yaml"
  private val Policies      = "fly/grafana/provisioning/alerting/notification-policies.yaml"

  private val AppNameLine = """(?m)^app\s*=\s*['"]([^'"]+)['"]""".r

  /** Deployed worker apps, from fly.worker.toml / fly.worker.<cc>.toml. */
  private lazy val workerApps: Seq[String] =
    RepoFile
      .flyTomls()
      .filter(_.getName.startsWith("fly.worker"))
      .flatMap(f => AppNameLine.findFirstMatchIn(RepoFile.read(f.getPath)).map(_.group(1)))

  private lazy val contactPoints = RepoFile.read(ContactPoints)
  private lazy val policies      = RepoFile.read(Policies)
  private lazy val alertRules    = RepoFile.read(AlertRules)

  "the throttle backstop" should "have a webhook contact point per worker app" in {
    workerApps.size should be > 1
    workerApps.foreach { app =>
      withClue(
        s"no contact point posts to http://$app.internal:9000/throttle — $app has no Grafana " +
          "backstop, so it silently loses reaper backoff whenever CpuCreditPoller is " +
          s"unavailable. Add a contact point for $app in $ContactPoints. "
      ) {
        contactPoints should include(s"http://$app.internal:9000/throttle")
      }
    }
  }

  it should "route each worker app's credit alert to that app's own contact point" in {
    workerApps.foreach { app =>
      withClue(
        s"notification-policies.yaml has no route matching app=$app — its credit alert would " +
          "fall through to the default Telegram receiver and page instead of throttling. " +
          s"Add a route pairing $app with its contact point in $Policies. "
      ) {
        policies should include(s"'$app'")
      }
    }
  }

  it should "emit one alert instance per worker app, not just the Polish one" in {
    // The rule has to be wide enough for the per-app routes above to have anything
    // to match; `app="kinowo-worker"` would starve every non-PL route.
    alertRules should include("""fly_instance_cpu_balance{app=~"kinowo-worker.*"}""")
  }

  "the worker health alerts" should "cover every country's worker, not only kinowo-worker" in {
    // These page a human (Telegram, grouped by app) rather than driving backoff, so
    // they need no per-app contact point — just a matcher that isn't PL-pinned.
    alertRules should include("""fly_instance_cpu_throttle{app=~"kinowo-worker.*"}""")
    alertRules should include("""process_start_time_seconds{app=~"kinowo-worker.*"}""")
  }

  it should "not page for the CPU-starved flare while a JVM is still warming up" in {
    // A cold JVM re-JITs the whole parse surface, and a deploy-replaced machine starts at
    // ~0 credit — so it sits throttled at baseline for ~an hour. That is benign and
    // self-limiting; without an uptime guard the rule fired for ~31% of a 24h window on
    // kinowo-worker alone. Only starvation that OUTLASTS warmup is actionable.
    val starved = alertRules
      .linesIterator
      .dropWhile(!_.contains("uid: kinowo-worker-cpu-starved"))
      .takeWhile(!_.contains("uid: kinowo-worker-crash-looping"))
      .mkString("\n")

    withClue("the CPU-starved rule lost its warm-JVM guard — it will page on every deploy: ") {
      starved should include("process_start_time_seconds")
      starved should include("> 3600")
    }
    withClue("the guard must JOIN on app+instance, or it would gate the wrong worker: ") {
      starved should include("and on (app, instance)")
    }
  }

  "the derived worker set" should "cover every country the repo deploys" in {
    workerApps should contain allOf ("kinowo-worker", "kinowo-worker-uk", "kinowo-worker-de")
  }
}
