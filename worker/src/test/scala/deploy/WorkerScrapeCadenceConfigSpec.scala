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
 * DE is deliberately the slow one. Its roster is ~1,529 cinemas across 158
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
   *  Was 5, measured back when `MaxHorizonDays` was 34 — a venue advertised ~3.5
   *  days plus its listing fetch. `ScrapeHorizon.MaxDays` went to 730 on
   *  2026-07-27, and a German venue now advertises far more of its programme: the
   *  DE worker ran 13.4 day-chunks per venue sweep over 6h on 2026-07-29 (range
   *  11.2-15.5). 14 is that rounded up.
   *
   *  The stale 5 is why this guard stayed green through a cadence that had become
   *  unreachable: at 5 the sweep computes to 3.0h and fits the old 180min window,
   *  while the real sweep is 1529 x 13.4 x 1400ms = 7.9h. DE's oldest cinema sat
   *  at ~12.8h and climbing, with the guard reporting everything fine. A constant
   *  measured against one horizon does not survive the horizon changing — the
   *  invariant below is only as honest as this number. */
  private val RequestsPerGermanVenue = 14

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

  "the DE worker" should "scrape on a cadence its paced Filmstarts sweep can drain within" in {
    // Was 180, on the old 5-requests-per-venue figure. At the real 13.4 the paced
    // sweep is 7.9h, so 180min was unreachable and DE's roster simply aged — the
    // invariant below now fails at 180 instead of passing on a stale constant.
    // 600 clears the ~8.4h sweep with ~17% headroom, matching the margin DE was
    // originally sized for. See the invariant test below and fly.worker.de.toml.
    cadenceOf("fly.worker.de.toml") shouldBe Some("600")
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
    // UK is the fleet's slowest (7h vs DE's 3h). This used to assert the literal 420,
    // which is how the number outlived its premise: it was sized when all 843 UK
    // venues fanned out onto flicks.co.uk, and since 2026-07-27 the 343 chain venues
    // are own-site-primary (only reaching flicks after 6h of a failing primary), so
    // the paced origin carries 500. Assert the INVARIANT the way DE's test does, so
    // the pace and the cadence can each move as long as the sweep still fits.
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

    // HEADROOM, reported not asserted. The invariant above is a hard floor, not a
    // comfort zone: `sweep <= cadence` still passes at exact equality, which means a
    // pacer at a 100% duty cycle and a worker that never idles. That is not
    // hypothetical — the hourly cadence tried on 2026-07-28 landed exactly there
    // (500 x 36 x 200ms = 60min against a 60min window) and was reverted the same
    // day. DE deliberately keeps ~11%. Print the margin so a run that passes at the
    // knife edge still says so out loud.
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

  /** The oldest-cinema-scrape panel tells the reader what "healthy" looks like by
   *  naming each country's scrape window — and that is the ONE number on it that
   *  cannot be derived from the series it draws. It was wrong within an hour of
   *  being written (DE stated as 2h against a deployed 180min), because the panel
   *  text and the toml that decides the value sit in different files and nothing
   *  connected them. A panel that misstates the healthy band is worse than one
   *  that says nothing: it turns a real 3x overrun into a reassuring "just over
   *  the line". So the sentence is generated here from the same tomls the spec
   *  already reads, and the panel has to spell it exactly. */
  "the oldest-cinema-scrape panel" should "state the same scrape windows the worker apps actually deploy" in {
    def hoursOf(toml: String): Int = {
      val minutes = cadenceOf(toml).map(_.toInt).getOrElse(fail(s"$toml has no cadence"))
      withClue(s"$toml's cadence ${minutes}min is not a whole number of hours, so the panel sentence needs rewording: ") {
        minutes % 60 shouldBe 0
      }
      minutes / 60
    }

    // One clause per country, never a grouping. The first version of this guard said
    // "Xh for pl and uk" because those two happened to share a value for a few hours
    // on 2026-07-28; the moment UK went back to 7h the assertion failed on the
    // GROUPING rather than on any real drift, which is a guard failing for the wrong
    // reason. Spelling each country out has no such coupling.
    val (pl, uk, de) = (hoursOf("fly.worker.toml"), hoursOf("fly.worker.uk.toml"), hoursOf("fly.worker.de.toml"))

    val dashboard = RepoFile.read("fly/grafana/provisioning/dashboards/fly-overview.json")
    withClue(s"the oldest-scrape panel must say '${pl}h for pl, ${uk}h for uk, ${de}h for de': ") {
      dashboard should include(s"${pl}h for pl, ${uk}h for uk, ${de}h for de")
    }
  }
}
