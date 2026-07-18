package deploy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Guards the Fly-host panels and fleet alerts against per-country drift.
 *
 * `fly_instance_*` / `fly_app_*` carry NO `country` label — Fly's managed
 * Prometheus only knows `app`. So the "Country" dropdown on the Fly-health
 * dashboard cannot scope those panels, and they are deliberately fleet-wide:
 * every country's instances on one chart. The only scoping lever is the app
 * name, and the failure mode is that someone enumerates the apps that existed
 * that day — `app=~"kinowo.*|showtimes-uk"` — and the NEXT country silently
 * goes invisible. That is exactly what happened to `showtimes-de`: it was
 * absent from all six Fly-host panels and from three alert rules, so the DE web
 * server had no CPU, credit, throttle, steal, memory or latency line, and the
 * "alarm < 500" / "alarm < 8%" promised in those panel titles never applied
 * to it.
 *
 * The invariant is COUNTRY-BLINDNESS, not universality: a web-only panel (HTTP
 * latency) or a worker-only panel is fine. What is never fine is a matcher that
 * accepts one country's app of a role and rejects another's. So for each role
 * — web, worker — every `app=~"..."` regex must match ALL of that role's apps
 * or NONE of them.
 *
 * The app set is derived from the `fly*.toml` files at the repo root rather
 * than hardcoded here, so onboarding a country (which adds `fly.<cc>.toml` +
 * `fly.worker.<cc>.toml`, see docs/adding-a-country.md) automatically widens
 * what this spec demands, instead of needing a matching edit in the guard.
 *
 * Out of scope: EXACT `app="..."` matchers. Those are deliberately per-app,
 * one rule each with an app-specific summary (`showtimes-uk-serving-down`), and
 * whether every country deserves its own copy is an alerting-noise judgement,
 * not a drift bug.
 */
class GrafanaCountryBlindAppMatcherSpec extends AnyFlatSpec with Matchers {

  private val FlyOverview = "fly/grafana/provisioning/dashboards/fly-overview.json"
  private val AlertRules  = "fly/grafana/provisioning/alerting/alert-rules.yaml"

  /** `app = 'kinowo-worker-de'` in fly.worker.de.toml → the deployed app name. */
  private val AppNameLine = """(?m)^app\s*=\s*['"]([^'"]+)['"]""".r

  /** Deployed apps split by role: `fly.worker*.toml` → worker, the rest → web. */
  private lazy val (workerApps, webApps) = {
    val byRole = RepoFile
      .flyTomls()
      .flatMap { file =>
        AppNameLine
          .findFirstMatchIn(RepoFile.read(file.getPath))
          .map(m => (file.getName.startsWith("fly.worker"), m.group(1)))
      }
      .partition(_._1)
    (byRole._1.map(_._2), byRole._2.map(_._2))
  }

  /**
   * Every `app=~"…"` regex in a provisioning file, JSON escaping undone so the
   * dashboard's `app=~\"kinowo.*\"` and the YAML's `app=~"kinowo.*"` compare
   * the same.
   */
  private def regexMatchers(path: String): Seq[String] =
    """app=~\\?"([^"\\]+)\\?"""".r
      .findAllMatchIn(RepoFile.read(path))
      .map(_.group(1))
      .toSeq
      .distinct

  /** PromQL regex matchers are fully anchored, which is what String.matches does. */
  private def matched(matcher: String, apps: Seq[String]): Seq[String] =
    apps.filter(_.matches(matcher))

  private def assertCountryBlind(path: String, role: String, apps: Seq[String]): Unit = {
    apps.size should be > 1 // otherwise the check is vacuous and we'd never catch drift
    regexMatchers(path).foreach { matcher =>
      val hit = matched(matcher, apps)
      withClue(
        s"""$path: app=~"$matcher" matches only ${hit.mkString(", ")} of the $role apps """ +
          s"""(${apps.mkString(", ")}) — that hides ${apps.diff(hit).mkString(", ")}. """ +
          "Match the naming convention (kinowo.*|showtimes-.* for both roles, " +
          "kinowo|showtimes-.* for web-only, kinowo-worker.* for worker-only) " +
          "instead of enumerating today's apps. "
      ) {
        (hit.isEmpty || hit.size == apps.size) shouldBe true
      }
    }
  }

  "the Fly-health dashboard" should "scope every panel country-blind across the web apps" in {
    assertCountryBlind(FlyOverview, "web", webApps)
  }

  it should "scope every panel country-blind across the worker apps" in {
    assertCountryBlind(FlyOverview, "worker", workerApps)
  }

  "the fleet alert rules" should "fire country-blind across the web apps" in {
    assertCountryBlind(AlertRules, "web", webApps)
  }

  it should "fire country-blind across the worker apps" in {
    assertCountryBlind(AlertRules, "worker", workerApps)
  }

  "the derived app set" should "cover every country the repo deploys" in {
    // Sanity-check the fly*.toml derivation itself: if this ever comes back
    // empty or PL-only, the four checks above would pass vacuously.
    webApps should contain allOf ("kinowo", "showtimes-uk", "showtimes-de")
    workerApps should contain allOf ("kinowo-worker", "kinowo-worker-uk", "kinowo-worker-de")
  }
}
