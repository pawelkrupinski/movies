package deploy

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.RateLimitedHttpFetch

import scala.concurrent.duration.*

/**
 * Locks the PER-COUNTRY scrape cadence, which lives only in each worker app's
 * `[env]` block. `Freshness.defaultScrapeTtl` reads
 * `KINOWO_SCRAPE_FRESHNESS_MINUTES` (default 60) and `WorkerWiring` captures it
 * once into the shared `DueWindow`, so the sweep rate a country actually runs at
 * is decided by its fly toml and nothing else — no `Country` field, no code path
 * a running-JVM test can reach. That makes an accidental edit here silent: the
 * worker just scrapes at the wrong rate until someone reads a graph.
 *
 * DE is deliberately the slow one. Its roster is ~1,533 cinemas across 158
 * Filmstarts regions, ~5x PL, on the box with the least CPU credit and heap
 * headroom in the fleet (see the JVM sizing note in `fly.worker.de.toml`).
 *
 * The mechanism itself — env var overriding the TTL — is covered by
 * `FreshnessStoreSpec`; this spec covers the deployed VALUES.
 */
class WorkerScrapeCadenceConfigSpec extends AnyFlatSpec with Matchers {
  private def cadenceOf(toml: String): Option[String] =
    RepoFile
      .read(toml)
      .linesIterator
      .map(_.trim)
      .filterNot(_.startsWith("#"))
      .collectFirst { case s"KINOWO_SCRAPE_FRESHNESS_MINUTES$rest" => rest.dropWhile(_ != '\'').filter(_.isDigit) }

  "the DE worker" should "scrape on a 3-hour cadence, not the fleet's hourly default" in {
    // 180, coupled to the 1000ms Filmstarts pace — a ~179min sweep needs a budget
    // that fits it. See the invariant test below and fly.worker.de.toml.
    cadenceOf("fly.worker.de.toml") shouldBe Some("180")
  }

  it should "pace Filmstarts slowly enough to stop the 429s, yet still sweep inside that cadence" in {
    // These two numbers are coupled and live in different files, so a change to
    // either alone silently breaks DE: the outbound pace (RealHttpFetch's
    // HostPolicies) decides how long a full sweep takes, and the cadence (the
    // toml above) decides how long it may take. Tightening the pace to fix 429s
    // lengthens the sweep; shortening the cadence shrinks the budget. Assert the
    // invariant rather than the arithmetic, so either can move as long as the
    // sweep still fits.
    val pace     = RateLimitedHttpFetch.configuredInterval("https://www.filmstarts.de/kinoprogramm/kino/A0006/")
    val cadence  = cadenceOf("fly.worker.de.toml").map(_.toInt).map(_.minutes)
    // One request per day-page per venue; WebediaShowtimesClient fetches 7 days.
    val requests = Country.Germany.cities.flatMap(_.cinemas).distinct.size * 7

    withClue("Filmstarts must stay paced — unpaced fan-out is what drew the 429s: ") {
      pace should not be empty
    }
    val sweep = (requests * pace.get.toMillis).millis
    withClue(s"$requests requests at ${pace.get.toMillis}ms = ${sweep.toMinutes}min sweep vs ${cadence.get.toMinutes}min cadence: ") {
      sweep should be <= cadence.get
    }
  }

  "the PL worker" should "stay on the hourly cadence" in {
    // Guards against a fleet-wide sweep of a slower value: PL's roster drains hourly.
    cadenceOf("fly.worker.toml") shouldBe Some("60")
  }

  "the UK worker" should "scrape on a slow cadence its chunked full-horizon sweep can drain within" in {
    // UK is even slower than DE (5h vs 3h) for a different reason. Its 843 Flicks
    // venues each fan out one ScrapeChunk per advertised day of their full booking
    // horizon (up to MaxHorizonDays=210, ~90-150 populated) — ~100k+ chunk tasks per
    // cycle. Flicks is UNPACED (no HostPolicies row), so the DE-style pace invariant
    // below doesn't bind here: the constraint is queue DRAIN, not 429s. The 4-worker
    // pool needs ~260min to clear one cycle, so at the old 60min work enqueued ~4x
    // faster than it drained and the task queue grew unboundedly (2026-07-20). 300min
    // clears a sweep with headroom. Only real drain throughput could tighten this
    // further, which a static test can't measure — so we lock the deployed value.
    cadenceOf("fly.worker.uk.toml") shouldBe Some("300")
  }

  "every worker toml" should "set the cadence explicitly rather than inheriting the code default" in {
    val workerTomls = RepoFile
      .flyTomls()
      .map(_.getName)
      .filter(_.startsWith("fly.worker"))

    workerTomls should not be empty
    workerTomls.foreach { toml =>
      withClue(s"$toml is missing KINOWO_SCRAPE_FRESHNESS_MINUTES: ") {
        cadenceOf(toml) should not be empty
      }
    }
  }
}
