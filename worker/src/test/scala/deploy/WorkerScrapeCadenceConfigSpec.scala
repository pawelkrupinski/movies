package deploy

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.ChainFlicksFallback
import services.cinemas.us.UsChainVenues
import tools.{RateLimitedHttpFetch, RealHttpFetch}

import scala.concurrent.duration.*

/**
 * Locks the PER-COUNTRY scrape cadence, which lives only in each worker's DEPLOY
 * CONFIG — its k3s overlay's ConfigMap, or the `[env]` block of the fly toml it
 * was onboarded with. `Freshness.defaultScrapeTtl` reads
 * `KINOWO_SCRAPE_FRESHNESS_MINUTES` (default 60) and `WorkerWiring` captures it
 * once into the shared `DueWindow`, so the sweep rate a country actually runs at
 * is decided by that config and nothing else — no `Country` field, no code path
 * a running-JVM test can reach. That makes an accidental edit here silent: the
 * worker just scrapes at the wrong rate until someone reads a graph.
 *
 * US is the slow one, at 14h. Its roster is ~5,000 Flicks venues across 55 states
 * and territories — ~10x the UK's paced set — against an origin whose 200ms pace
 * is a measured ceiling rather than a choice, so the cadence is the only lever
 * its ~10h sweep leaves. DE is next at 10h: ~1,529 cinemas across 158 Filmstarts
 * regions, on the least heap headroom in the fleet (see `fly.worker.de.toml`).
 *
 * NOT EVERY COUNTRY HAS A FLY TOML. The US was onboarded straight onto k3s, so
 * its cadence exists only in `infra/kubernetes/worker/overlays/us/patch.yaml` —
 * which is the general case now that every main.yml Fly leg is disabled, and
 * why the reader below is syntax-agnostic.
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

  /** SensaCine requests one SPANISH venue costs per sweep — the same client and
   *  the same shape as Germany's (one venue page for `data-showtimes-dates`,
   *  then one day-page per advertised day), but NOT the same number, which is
   *  the point of measuring it per market rather than inheriting it.
   *
   *  Measured 2026-09-01 over 30 venues drawn from both large and small
   *  provinces: mean 7.8 requests per venue (median 11, max 19), with 8 of the
   *  30 dark — an empty `data-showtimes-dates`, costing just the listing fetch.
   *  A Spanish venue advertises about half what a German one does.
   *
   *  12 is that mean rounded UP with room for a busier season, and rounding up
   *  is the safe direction: this number only makes the invariant below stricter.
   *  Germany's sat at a stale 5 through a horizon change and reported a
   *  comfortable 3h sweep while the real one had grown to 7.9h. */
  private val RequestsPerSpanishVenue = 12

  /** UK venues whose scrape actually reaches the PACED flicks.co.uk origin on the
   *  happy path. The chain venues (Cineworld, Vue, Odeon, Everyman, Showcase) went
   *  own-site-primary on 2026-07-27; `SourceFallbackScraper` only calls the flicks
   *  fallback after the primary has been failing for 6h, so in steady state they
   *  cost the paced origin nothing. Derived rather than written out, so wiring a
   *  venue to a chain (or back to flicks) moves this number automatically.
   *  (`ChainFlicksFallback.ukSlugs` is the UK half of what
   *  `CinemaScraperCatalog.flicksFallbackSlugs` exposes — `slugs` now also carries
   *  Regal's US venues, which are no business of a UK count; the catalog itself is
   *  a class needing live deps, so read the source.) */
  private def FlicksPrimaryVenues =
    Country.UnitedKingdom.cities.flatMap(_.cinemas).distinct.size - ChainFlicksFallback.ukSlugs.size

  /** US venues whose scrape actually reaches the PACED flicks.us origin on the happy
   *  path — the US counterpart of the UK figure above. 79 of them (Alamo 40,
   *  Landmark 26, Showcase US 13) went own-site-primary and now cost flicks.us
   *  nothing in steady state, reaching it only after the 6h fallback grace.
   *  Derived from `UsChainVenues`, so wiring a venue to a chain (or back to Flicks)
   *  moves this number automatically. */
  private def UsFlicksPrimaryVenues =
    Country.UnitedStates.cities.flatMap(_.cinemas).distinct
      .count(c => !UsChainVenues.all.contains(c.displayName))

  /** flicks.co.uk requests one UK venue costs per sweep: the programme page
   *  `planChunks` reads its `data-date` list off, plus one day-page per advertised
   *  day. Measured on prod 2026-07-28 — over 24h the UK worker ran 2,377 chunked
   *  venue sweeps and 82,939 day-chunks, i.e. ~34.9 days per venue — rounded up to
   *  36 for the listing fetch. NOT `ScrapeHorizon.MaxDays` (730): that is a sanity
   *  valve against a garbage far date, not the number of days a venue advertises. */
  private val RequestsPerFlicksVenue = 36

  /** `KINOWO_SCRAPE_FRESHNESS_MINUTES` out of a deploy config, whichever syntax it
   *  is written in — the parsing itself lives in `RepoFile` because
   *  `ScrapeCadenceSustainabilitySpec` reads the same values to size the shared
   *  enqueue caps, and two copies of it would drift. */
  private def cadenceOf(config: String): Option[String] =
    RepoFile.freshnessMinutesIn(RepoFile.read(config)).map(_.toString)

  /** The k3s overlay that actually deploys a country's worker. THE FLY TOMLS ARE
   *  RETIRED for the three countries that have one (every WORKER leg in main.yml
   *  is `enabled: false`; the pods on k3s-worker-1 are what run) and the US never had
   *  one — so for it this file is the only place its cadence exists. */
  private def workerOverlay(cc: String) = s"infra/kubernetes/worker/overlays/$cc/patch.yaml"

  /** One `KINOWO_*` value out of a worker overlay's ConfigMap. */
  private def overlayEnv(cc: String, key: String): Option[String] =
    RepoFile
      .read(workerOverlay(cc))
      .linesIterator
      .map(_.trim)
      .filterNot(_.startsWith("#"))
      .collectFirst { case line if line.startsWith(s"$key:") => line.drop(key.length + 1).filter(_.isDigit) }

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

  "the US worker" should "scrape on a cadence its paced Flicks sweep can drain within" in {
    // THE SAME INVARIANT AS UK AND DE, against six times the UK's roster. The US is not
    // a UK with more venues: `flicks.us` earns its OWN HostPolicies row (rows match by
    // host SUFFIX, so `flicks.co.uk` does not cover it), and 200ms is a measured
    // CEILING there rather than a tuning choice — the origin throttles per zone by
    // stalling connections and plateaus at ~3-5 req/s whatever the concurrency, so a
    // shorter interval buys latency, not throughput. The only lever left is the
    // cadence, which is why this country's is 840min and not the UK's 420.
    val pace    = RateLimitedHttpFetch.configuredInterval("https://www.flicks.us/cinema/sessions/x/2026-08-30/")
    val cadence = cadenceOf(workerOverlay("us")).map(_.toInt).map(_.minutes)

    withClue("flicks.us must stay paced — an UNPACED host is the condition that drew the UK's 429 storm, here at 6x the venue count: ") {
      pace should not be empty
    }

    val requests = UsFlicksPrimaryVenues * RequestsPerFlicksVenue
    val sweep    = (requests * pace.get.toMillis).millis
    withClue(s"$requests requests at ${pace.get.toMillis}ms = ${sweep.toMinutes}min sweep vs ${cadence.get.toMinutes}min cadence: ") {
      sweep should be <= cadence.get
    }

    // Reported, not asserted — same as UK. `sweep <= cadence` passes at exact equality,
    // which is a pacer at 100% duty against one third-party origin 24/7.
    val headroom = 1.0 - sweep.toMillis.toDouble / cadence.get.toMillis
    info(f"US Flicks sweep uses ${100 * (1 - headroom)}%.1f%% of the ${cadence.get.toMinutes}min window (headroom ${100 * headroom}%+.1f%%)")
  }

  it should "charge the scrape queue the same per-venue cost the sweep above is computed from" in {
    // Two numbers for one fact, in two files. `KINOWO_SCRAPE_TASKS_PER_VENUE` is what
    // the reaper ADMITS a venue against, and RequestsPerFlicksVenue is what the sweep
    // arithmetic above SPENDS for it. They are both "a Flicks venue costs one
    // programme page plus one day-chunk per advertised day". If they drift, the reaper
    // admits at one rate while the cadence is sized for another — the queue-by-COUNT
    // burst that floors CPU, with a guard still reporting the sweep fits.
    overlayEnv("us", "KINOWO_SCRAPE_TASKS_PER_VENUE") shouldBe Some(RequestsPerFlicksVenue.toString)
  }

  "the Spain worker" should "scrape on a cadence its paced SensaCine sweep can drain within" in {
    // THE SAME INVARIANT, and the same trap Germany and the US each hit from a
    // different direction: Spain reaches the SAME client Germany does
    // (`WebediaShowtimesClient`) on a DIFFERENT host, and `HostPolicies` rows match
    // by host SUFFIX — so `filmstarts.de` does not cover `www.sensacine.com` and a
    // Spanish sweep would run entirely UNPACED while every dashboard, parser and
    // spec it shares with Germany looked fine. The pace assertion below is the one
    // that catches that.
    //
    // 1400ms is Filmstarts' converged number adopted for a sibling market rather
    // than a measured Spanish ceiling. Germany spent three retunes
    // (250 -> 500 -> 1000 -> 1400ms) finding what a Webedia origin tolerates
    // sustained; starting Spain there costs sweep length and risks nothing, and
    // KINOWO_SENSACINE_PACE_MS retunes it live.
    val pace = RateLimitedHttpFetch.configuredInterval(
      "https://www.sensacine.com/_/showtimes/theater-E0291/d-2026-09-02/p-1/")
    val cadence = cadenceOf(workerOverlay("es")).map(_.toInt).map(_.minutes)

    withClue("sensacine.com must have its OWN pace row — it does not inherit filmstarts.de's: ") {
      pace should not be empty
    }

    val requests = Country.Spain.cities.flatMap(_.cinemas).distinct.size * RequestsPerSpanishVenue
    val sweep    = (requests * pace.get.toMillis).millis
    withClue(s"$requests requests at ${pace.get.toMillis}ms = ${sweep.toMinutes}min sweep vs ${cadence.get.toMinutes}min cadence: ") {
      sweep should be <= cadence.get
    }

    // Reported, not asserted — same as UK and US. `sweep <= cadence` passes at exact
    // equality, which is a pacer at 100% duty against one third-party origin 24/7.
    val headroom = 1.0 - sweep.toMillis.toDouble / cadence.get.toMillis
    info(f"SensaCine sweep uses ${100 * (1 - headroom)}%.1f%% of the ${cadence.get.toMinutes}min window (headroom ${100 * headroom}%+.1f%%)")
  }

  it should "charge the scrape queue the same per-venue cost the sweep above is computed from" in {
    // Same coupling the US test spells out: the reaper ADMITS a venue against
    // KINOWO_SCRAPE_TASKS_PER_VENUE while the cadence is sized from
    // RequestsPerSpanishVenue. Drift, and the queue bursts by COUNT with this guard
    // still reporting the sweep fits.
    overlayEnv("es", "KINOWO_SCRAPE_TASKS_PER_VENUE") shouldBe Some(RequestsPerSpanishVenue.toString)
  }

  it should "keep sensacine.com paced independently of its German sibling" in {
    // The two markets share a client, a parser and a set of dashboards, so the ONE
    // thing keeping their request budgets apart is that the pace gate and the 429
    // back-off both bucket by full hostname. Assert they resolve to separate rows
    // rather than one matching both.
    val es = RateLimitedHttpFetch.configuredInterval("https://www.sensacine.com/cines/cine/E0291/")
    val de = RateLimitedHttpFetch.configuredInterval("https://www.filmstarts.de/kinoprogramm/kino/A0263/")
    es should not be empty
    de should not be empty
    // Equal TODAY (Spain adopted its sibling's number), but they are separate knobs:
    // KINOWO_SENSACINE_PACE_MS moves one without the other.
    RealHttpFetch.HostPolicies.count(_.hostSuffixes.contains("sensacine.com")) shouldBe 1
    RealHttpFetch.HostPolicies.count(_.hostSuffixes.contains("filmstarts.de")) shouldBe 1
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

  "every k3s worker overlay" should "set the cadence explicitly rather than inheriting the code default" in {
    // The overlays are the layer that actually deploys now. A country onboarded onto
    // k3s without a fly toml — the US is the first — has no other place to say this,
    // and inheriting `Freshness.defaultScrapeTtl`'s 60min would put a 8.5h sweep on an
    // hourly window.
    val overlays = Option(new java.io.File("infra/kubernetes/worker/overlays").listFiles())
      .getOrElse(Array.empty[java.io.File])
      .filter(_.isDirectory)
      .map(_.getName)
      .sorted

    overlays should not be empty
    overlays.foreach { cc =>
      withClue(s"infra/kubernetes/worker/overlays/$cc is missing KINOWO_SCRAPE_FRESHNESS_MINUTES: ") {
        cadenceOf(workerOverlay(cc)) should not be empty
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
    def hoursOf(config: String): Int = {
      val minutes = cadenceOf(config).map(_.toInt).getOrElse(fail(s"$config has no cadence"))
      withClue(s"$config's cadence ${minutes}min is not a whole number of hours, so the panel sentence needs rewording: ") {
        minutes % 60 shouldBe 0
      }
      minutes / 60
    }

    // One clause per country, never a grouping. The first version of this guard said
    // "Xh for pl and uk" because those two happened to share a value for a few hours
    // on 2026-07-28; the moment UK went back to 7h the assertion failed on the
    // GROUPING rather than on any real drift, which is a guard failing for the wrong
    // reason. Spelling each country out has no such coupling.
    //
    // The US clause is read from its k3s OVERLAY rather than a toml, because it has no
    // toml — which is the general case now, not an exception: the tomls are retired and
    // the overlays are what deploy. A country left out of the sentence entirely is the
    // same failure in a milder form — the panel draws its line and states no band for it.
    val pl = hoursOf("fly.worker.toml")
    val uk = hoursOf("fly.worker.uk.toml")
    val de = hoursOf("fly.worker.de.toml")
    val us = hoursOf(workerOverlay("us"))
    val es = hoursOf(workerOverlay("es"))

    val sentence = s"${pl}h for pl, ${uk}h for uk, ${de}h for de, ${us}h for us, ${es}h for es"
    // BOTH copies, because there are two and only one of them is read by anybody. The
    // live dashboard is the one on monitoring-1; the fly/ copy is the frozen rollback
    // for the stopped Fly Grafana. Guarding only the frozen one is how the live panel
    // would be free to misstate the band.
    Seq(
      "fly/grafana/provisioning/dashboards/fly-overview.json",
      "infra/nix/files/monitoring/grafana/dashboards/apps/fly-overview.json"
    ).foreach { dashboard =>
      withClue(s"$dashboard's oldest-scrape panel must say '$sentence': ") {
        RepoFile.read(dashboard) should include(sentence)
      }
    }
  }
}
