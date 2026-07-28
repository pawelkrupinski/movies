package deploy

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.ChainFlicksFallback
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

  /** Filmstarts requests one German venue costs per sweep: the venue page
   *  `planChunks` reads `data-showtimes-dates` off, plus one day-page per day
   *  that attribute advertises.
   *
   *  It used to be modelled as a flat 7 — the fixed day grid the client fetched
   *  before it became `data-showtimes-dates`-driven. That constant now BOTH
   *  overstates the cost (a venue advertises far fewer populated days than a
   *  week) and understates the ceiling (`MaxHorizonDays` is 34), so it measured
   *  nothing real. 5 is the production figure rounded up: over 24h the DE worker
   *  ran 14,862 venue sweeps and 51,973 day-chunks — ~3.5 days plus the one
   *  listing fetch per venue — leaving ~11% headroom for a busier week. */
  private val RequestsPerGermanVenue = 5

  /** UK venues whose scrape actually reaches the PACED flicks.co.uk origin on the
   *  happy path. The chain venues (Cineworld, Vue, Odeon, Everyman, Showcase) went
   *  own-site-primary on 2026-07-27; `SourceFallbackScraper` only calls the flicks
   *  fallback after the primary has been failing for 6h, so in steady state they
   *  cost the paced origin nothing. Derived rather than written out, so wiring a
   *  venue to a chain (or back to flicks) moves this number automatically.
   *  (`ChainFlicksFallback.slugs` is what `CinemaScraperCatalog.flicksFallbackSlugs`
   *  exposes; the catalog itself is a class needing live deps, so read the source.) */
  private def FlicksPrimaryVenues =
    Country.UnitedKingdom.cities.flatMap(_.cinemas).distinct.size - ChainFlicksFallback.slugs.size

  /** flicks.co.uk requests one UK venue costs per sweep: the programme page
   *  `planChunks` reads its `data-date` list off, plus one day-page per advertised
   *  day. Measured on prod 2026-07-28 — over 24h the UK worker ran 2,377 chunked
   *  venue sweeps and 82,939 day-chunks, i.e. ~34.9 days per venue — rounded up to
   *  36 for the listing fetch. NOT `ScrapeHorizon.MaxDays` (730): that is a sanity
   *  valve against a garbage far date, not the number of days a venue advertises. */
  private val RequestsPerFlicksVenue = 36

  private def cadenceOf(toml: String): Option[String] =
    RepoFile
      .read(toml)
      .linesIterator
      .map(_.trim)
      .filterNot(_.startsWith("#"))
      .collectFirst { case s"KINOWO_SCRAPE_FRESHNESS_MINUTES$rest" => rest.dropWhile(_ != '\'').filter(_.isDigit) }

  "the DE worker" should "scrape on a 3-hour cadence, not the fleet's hourly default" in {
    // 180, coupled to the 1400ms Filmstarts pace — a ~179min sweep needs a budget
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
    val requests = Country.Germany.cities.flatMap(_.cinemas).distinct.size * RequestsPerGermanVenue

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

  "the UK worker" should "scrape on a cadence its paced Flicks sweep can drain within" in {
    // Was a hard-coded 420. That number was sized when all 843 UK venues fanned out
    // onto flicks.co.uk; since 2026-07-27 the 343 chain venues are own-site-primary
    // and only reach flicks after 6h of a failing primary, so the paced origin
    // carries 500. Assert the INVARIANT the way DE's test does rather than the
    // arithmetic, so the pace and the cadence can each move as long as the sweep
    // still fits — a literal is exactly what let the old value outlive its premise.
    val pace    = RateLimitedHttpFetch.configuredInterval("https://www.flicks.co.uk/cinema/sessions/x/2026-07-31/")
    val cadence = cadenceOf("fly.worker.uk.toml").map(_.toInt).map(_.minutes)

    withClue("Flicks must stay paced — unpaced fan-out is what drew the 429s: ") {
      pace should not be empty
    }
    val requests = FlicksPrimaryVenues * RequestsPerFlicksVenue
    val sweep    = (requests * pace.get.toMillis).millis
    withClue(s"$requests requests at ${pace.get.toMillis}ms = ${sweep.toMinutes}min sweep vs ${cadence.get.toMinutes}min cadence: ") {
      sweep should be <= cadence.get
    }

    // HEADROOM, reported not asserted. At 500 venues x 36 requests x 200ms the sweep
    // is 60min against a 60min cadence: it fits with 0% to spare, so the paced origin
    // is busy 100% of every window and the worker never idles on it. DE deliberately
    // keeps ~11%. This prints the margin so a run that "passes" at the knife edge
    // still says so out loud — the invariant above is the hard floor, not a comfort
    // zone, and a venue advertising one more day than measured breaks it.
    val headroom = 1.0 - sweep.toMillis.toDouble / cadence.get.toMillis
    info(f"Flicks sweep uses ${100 * (1 - headroom)}%.1f%% of the ${cadence.get.toMinutes}min window (headroom ${100 * headroom}%+.1f%%)")
  }

  it should "pace Flicks so the fan-out stops tripping its 429 limiter" in {
    // The fact that changed: Flicks earned a HostPolicies pace row, coupling the
    // cadence above to it exactly as DE's is to Filmstarts'. A dropped pace row
    // would silently reopen the 429 bursts (panel-14) and the venue-day drops.
    RateLimitedHttpFetch.configuredInterval("https://www.flicks.co.uk/cinema/sessions/x/2026-07-31/") should not be empty
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
