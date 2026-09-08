package modules.wiring

import models.Cinema
import modules.WorkerWiring
import services.MongoCachingDetailFetch
import services.cinemas.{ChainFlicksFallback, CinemaScraperCatalog}
import services.cinemas.common.{AdaptiveTimeoutScraper, ChunkedCinemaScraper, CinemaClientMarkers, CinemaScrapeRunner, CinemaScraper, FallbackEligibility, FlicksClient, RetryingCinemaScraper, SourceFallbackScraper, UptimeRecordingScraper}
import services.cinemas.pl.{FilmwebCinemaIdResolver, FilmwebShowtimesClient}
import services.alerts.FallbackAlert
import services.fallback.{FallbackEvent, FallbackState, FallbackStore, MongoFallbackStore}
import services.scrapes.{MongoScrapeArchiveRepository, ScrapeArchiveRepository}
import services.tasks.{ScrapeCadence, ScrapeCinemaHandler, ScrapeFreshnessPolicy, ScrapeReaper}
import tools.{DaemonExecutors, Env, HostScrapeStats, ScrapeCities}

import java.util.concurrent.ExecutorService
import scala.concurrent.duration.DurationLong

/** The per-city scraper graph, the wraps every production scrape runs under
 *  (retry, adaptive timeout, uptime recording, Filmweb/Flicks fallback), and the
 *  plain-scrape task path: handler + the reaper that enqueues it. The chunked
 *  (map-reduce) path is [[ChunkScrapeWiring]]. */
trait ScrapeWiring { self: WorkerWiring =>

  // ── Cinema scrapers ───────────────────────────────────────────────────────
  // The per-city scraper graph lives in CinemaScraperCatalog (Mongo-free, so a
  // diagnostic like tools.FilmwebDiff can build the real scrapers without the
  // worker's write machinery). WorkerWiring supplies the seams it varies —
  // `httoFetch`, the Zyte-routed `multikinoFetch` / `biletynaFetch`, and Helios's REST date — and
  // wraps each raw scraper in RetryingCinemaScraper (retry) + UptimeRecordingScraper
  // (record the outcome) for production ticks.
  lazy val cinemaScraperCatalog = new CinemaScraperCatalog(
    httoFetch, multikinoFetch, biletynaFetch, heliosToday,
    // Mongo-backed chain detail cache so Helios / Cinema City detail is deduped
    // across worker servers, not just within one process.
    // One collection PER CHAIN: the TTL index is a property of the collection, so two
    // chains sharing it means one of their expiries silently loses (see the class doc).
    (chain, h, ttl) => new MongoCachingDetailFetch(h, mongoConnection.database, ttl, s"detailCache-$chain"),
    // Kino Kryterium (bilety.ck105.koszalin.pl) times out our Fly egress IP AND
    // every Decodo proxy IP at the TCP layer, so a direct scrape came back empty
    // → a permanent white /uptime bar. Only Zyte's true-residential network
    // reaches it, so route it straight through Zyte (the Decodo proxy can't help).
    zyteFetch = zyteFetch,
    flicksFetch = flicksFetch,
    vueFetch = vueFetch,
    odeonFetch = odeonFetch,
    odeonAuthToken = odeonAuthHarvester.token,
    titles = titleNormalizer)

  // This country's own cities — the default scrape set. A country wiring only
  // ever scrapes its own cities (never another country's), so the default is
  // `country.cities`, not the global `City.all` union. KINOWO_SCRAPE_CITIES only
  // NARROWS within that set (e.g. to shed load if the worker throttles/OOMs).
  protected def scrapeCitiesDefault: Set[String] = country.cities.map(_.slug).toSet
  // `protected def` so test wirings can pin the set independently.
  protected def scrapeCities: Set[String] =
    ScrapeCities.enabled(Env.get("KINOWO_SCRAPE_CITIES"), default = scrapeCitiesDefault)

  // The date Helios bakes into its REST URLs. Production uses the real Warsaw
  // date; fixture-replay test wirings override with the fixture's capture date.
  // TODO(multi-country): Helios is a Poland-only chain, so its REST date is
  // inherently Europe/Warsaw. There's no single country-level zone to key this
  // off (a country can span cities in different zones — `City.zoneId` carries the
  // per-city zone), and no non-PL date-baked chain exists yet, so this stays
  // Warsaw rather than being widened here. When a second country grows a
  // date-baked chain, lift a primary zone onto `Country` and read it here.
  protected def heliosToday: java.time.LocalDate =
    java.time.LocalDate.now(java.time.ZoneId.of("Europe/Warsaw"))

  // Upper bound on how many times a cinema scrape is attempted before giving up.
  // Each scraper declares its own `maxFetchAttempts` (default 3; a flaky upstream
  // like GCF raises it) and the wrap below takes the smaller of that and this
  // ceiling. Production leaves the ceiling generous so a cinema's own value wins;
  // fixture-replay test wirings drop it to 1 — a missing fixture is a PERMANENT
  // miss, so retrying just multiplies the fixture-server boot time (backoff per
  // fixture-less cinema, which with the full 40+-city catalogue blows past the CI
  // port-file ceiling).
  protected def scrapeAttemptCeiling: Int = 6

  // ── Filmweb fallback ────────────────────────────────────────────────────────
  // Each non-chain venue whose own scraper throws or comes back empty is served
  // from Filmweb instead (SourceFallbackScraper), and the swap is recorded for
  // the /uptime/fallback page. Each cinema's Filmweb id is resolved once (one GET
  // per Filmweb-listed city), guarded so a network/resolver failure yields no
  // fallback rather than a boot failure; cinemas Filmweb doesn't list simply have
  // no fallback available. Test wirings pin this empty so fixture replay never
  // resolves or fetches Filmweb live (see TestWiring). Also empty for a country
  // whose Filmweb path is off — an empty id map makes every SourceFallbackScraper
  // short-circuit to the primary's real outcome (identical to no fallback).
  protected lazy val filmwebFallbackIds: Map[Cinema, Int] =
    if (!filmwebEnabled) Map.empty
    else scala.util.Try(new FilmwebCinemaIdResolver(httoFetch).resolveAll())
      .toOption.getOrElse(Nil)
      .collect { case r if r.resolved => r.cinema -> r.filmwebId.get }
      .toMap

  protected def filmwebFallbackFor(cinema: Cinema): Option[CinemaScraper] =
    filmwebFallbackIds.get(cinema).map(id =>
      new FilmwebShowtimesClient(httoFetch, id, cinema, today = heliosToday))

  lazy val filmwebFallbackStore: FallbackStore =
    new MongoFallbackStore(mongoConnection.database)

  // The per-cinema scraper-client marker ("shared:<Client>" / "custom:<Client>"),
  // derived once from the catalog. Shared by the boot reconcile and the per-event
  // retag so the FtFW tag is layered on top of — never instead of — the marker.
  protected lazy val clientMarkers: Map[String, String] =
    CinemaClientMarkers.markers(cinemaScraperCatalog.all)

  // The per-cinema public source-page URL ("url:<https…>"), derived once from
  // the catalog alongside the client marker so the /uptime page can link each
  // cinema name to the page we scrape. Rides the same tag channel. Filmweb-backed
  // venues are upgraded from their `/cinema/-<id>` fallback to the canonical,
  // browser-renderable `/showtimes/<City>/<Name>-<id>` page, resolved once from
  // Filmweb's /info at boot (city + name aren't in our model); tolerant, so a
  // venue whose resolve fails keeps the fallback.
  protected lazy val sourceUrls: Map[String, String] = {
    val base           = CinemaClientMarkers.sourceUrls(cinemaScraperCatalog.all)
    val filmwebClients = cinemaScraperCatalog.all.collect { case f: FilmwebShowtimesClient => f }
    base ++ FilmwebShowtimesClient.resolveAll(filmwebClients)
  }

  // Fired on each ENTER / PROBE_FAILED / RECOVERED transition: alerts on the
  // page-worthy ones (FallbackAlert filters PROBE_FAILED out) when Telegram is
  // configured, and (re)writes the cinema's /uptime tags so the FtFW chip appears
  // on ENTER and clears on RECOVER. `state.active` is the post-transition truth
  // (put() runs before onEvent), so no store round-trip is needed.
  protected def filmwebFallbackOnEvent: (FallbackState, FallbackEvent) => Unit =
    (state, event) => {
      FallbackAlert.messageFor(state, event).foreach(message => fallbackTelegramNotifier.foreach(_.send(message)))
      uptimeMonitor.tagService(state.cinema, CinemaClientMarkers.tagsFor(clientMarkers.get(state.cinema), sourceUrls.get(state.cinema), state.active))
    }

  // Cinemas whose ONLY scraper is a FilmwebShowtimesClient — served by Filmweb by
  // design, not as a fallback. Feeds the FilmwebDropAlerter (a Filmweb-only venue
  // going empty means migrate it to an own-site scraper).
  lazy val filmwebOnlyCinemas: Set[String] =
    cinemaScraperCatalog.all.groupBy(_.cinema)
      .collect { case (c, scrapers) if scrapers.nonEmpty && scrapers.forall(_.isInstanceOf[FilmwebShowtimesClient]) =>
        c.displayName }
      .toSet

  lazy val cinemaScrapers: Seq[CinemaScraper] =
    country.cities
      .filter(c => scrapeCities(c.slug))
      .flatMap(c => cinemaScraperCatalog.byCity.getOrElse(c.slug, Nil))
      .map { raw =>
        val retried = new RetryingCinemaScraper(raw, maxAttempts = math.min(raw.maxFetchAttempts, scrapeAttemptCeiling))
        // A chunked cinema is scraped via the task fan-out (ScrapeCinemaHandler
        // routes it to ChunkScrapePlanner) and its outcome is recorded at the
        // reduce step, so it skips the per-scrape AdaptiveTimeout — each chunk is
        // already its own bounded task. Everything else is bounded here: the whole
        // scrape (retries included) to an adaptive per-host budget, OUTSIDE retry
        // but INSIDE the uptime recorder so a cut surfaces as a normal failure.
        val inner: CinemaScraper =
          if (raw.isInstanceOf[ChunkedCinemaScraper]) retried
          else new AdaptiveTimeoutScraper(retried, hostScrapeStats, adaptiveTimeoutExecutor)
        recordingScraper(inner, FallbackEligibility.eligible(raw))
      }

  /** Cinema → its flicks.co.uk slug, for UK chain venues whose own-site scraper is
   *  the primary and flicks is the aggregator FALLBACK (the mirror of the Polish
   *  own-site→Filmweb arrangement). Sourced from the catalogue so the same slug that
   *  used to be a flicks *catalogue* entry now feeds the fallback. Keyed by cinema so
   *  it applies whether the primary is a plain or a chunked chain scraper. */
  protected def flicksFallbackSlugs: Map[Cinema, ChainFlicksFallback.FlicksFallback] =
    cinemaScraperCatalog.flicksFallbackSlugs

  /** Wrap a scrape source with the outcome recorder + its fallback source:
   *   - a chain venue → Flicks as the aggregator fallback, on the market its
   *     catalogue entry names (flicks.co.uk for the UK chains, flicks.us for AMC);
   *   - else an eligible single venue → Filmweb;
   *   - else the plain uptime recorder.
   *  One source-neutral [[SourceFallbackScraper]] serves both feeds; `fallbackName`
   *  drives the /uptime label + Telegram text. Extracted so the chunked reduce step
   *  (`publishScrape`) records uptime + falls back exactly like a live scrape. */
  private[wiring] def recordingScraper(inner: CinemaScraper, eligible: Boolean): CinemaScraper =
    flicksFallbackSlugs.get(inner.cinema) match {
      // The market comes from the map, not a constant: a Regal venue's fallback
      // lives on flicks.us, and looking it up on flicks.co.uk would just 404.
      case Some(ChainFlicksFallback.FlicksFallback(market, slug)) =>
        new SourceFallbackScraper(inner,
          fallback     = () => Some(new FlicksClient(flicksFetch, slug, inner.cinema, market)),
          fallbackName = "Flicks",
          fallbackRef  = () => Some(slug),
          uptimeMonitor, filmwebFallbackStore, onEvent = filmwebFallbackOnEvent)
      case None if eligible =>
        new SourceFallbackScraper(inner,
          fallback     = () => filmwebFallbackFor(inner.cinema),
          fallbackName = "Filmweb",
          fallbackRef  = () => filmwebFallbackIds.get(inner.cinema).map(_.toString),
          uptimeMonitor, filmwebFallbackStore, onEvent = filmwebFallbackOnEvent)
      case None =>
        new UptimeRecordingScraper(inner, uptimeMonitor, scrapeOutcomeListener)
    }

  /** Rolling per-host scrape-duration stats backing the adaptive scrape timeout.
   *  In-memory by design — it adds no Mongo write load (the throttle this guards
   *  against IS write/CPU pressure) and rebuilds within a few refresh ticks. */
  lazy val hostScrapeStats: HostScrapeStats = new HostScrapeStats()

  /** Runs each scrape so [[AdaptiveTimeoutScraper]] can time it out and interrupt
   *  it. Virtual threads (cheap, daemon) in production; the test harness
   *  overrides this with a caller-runs executor to stay deterministic. */
  protected lazy val adaptiveTimeoutExecutor: ExecutorService =
    DaemonExecutors.virtualThreadEC("adaptive-timeout")

  // ── Task queue (scrape scheduling) ──────────────────────────────────────────
  // Hold the first scrape back from boot so the cold-boot scrape burst doesn't
  // pile onto the cache hydrate and drain the shared-CPU credit balance to zero.
  // The ScrapeReaper's first tick enqueues every stale cinema (all of them on a
  // cold boot) for the TaskWorker to drain at once.
  def initialScrapeDelaySeconds: Long = Env.positiveLong("KINOWO_SCRAPE_INITIAL_DELAY_SECONDS", 45L)

  // Cap on stale cinemas enqueued per reaper tick. A cold boot (or a long backlog)
  // would otherwise queue every cinema at once and let the TaskWorker pool drain
  // flat-out for minutes, exhausting the shared-CPU credit balance — the boot-storm
  // throttle spike. ~25/min drains inside the 1-min tick, leaving idle gaps for
  // credit to recover; the backlog clears over a handful of ticks. Tune down if a
  // restart still throttles, up once Mongo/CPU have headroom. Default sized in
  // ScrapeCadence (≥1.5× the steady-state due rate at the freshness window).
  def maxScrapeEnqueuePerTick: Int =
    Env.positiveInt("KINOWO_SCRAPE_MAX_ENQUEUE_PER_TICK", ScrapeCadence.MaxEnqueuePerTick)

  // Cinema scraping is driven by a durable Mongo task queue: the ScrapeReaper
  // enqueues each cinema at most once per freshness window, and the TaskWorker
  // scrapes it (skipping if a concurrent run already refreshed it). Detail and
  // rating enrichment are governed by KINOWO_DEFERRED_DETAIL and
  // KINOWO_QUEUE_ENRICHMENT independently.

  // The shared scrape core: record + decide-trigger, injected into
  // ScrapeCinemaHandler. Detail enqueue is event-driven (DetailTaskEnqueuer off
  // CinemaMovieAdded) plus the DetailReaper backstop; the runner publishes
  // MovieDetailsComplete only for rows that don't await deferred detail.
  lazy val cinemaScrapeRunner = new CinemaScrapeRunner(movieCache, eventBus, deferredDetailCinemas, scrapeArchive)

  /** Every cinema's last consolidated scrape, kept for replay/repopulate. One row
   *  per cinema in THIS country's database, replaced on each successful scrape. */
  lazy val scrapeArchive: ScrapeArchiveRepository = new MongoScrapeArchiveRepository(mongoConnection.database)

  // ONE policy across every scrape path (plain, chunked plan, chunked reduce) so a
  // venue's failure streak is counted once and every terminal outcome advances the
  // due schedule by the same rule. See ScrapeFreshnessPolicy for why a broken venue
  // MUST eventually be stamped: un-stamped venues sort first in the reaper's
  // oldest-first order and otherwise camp on the whole per-tick budget forever.
  lazy val scrapeFreshnessPolicy    = new ScrapeFreshnessPolicy(freshnessStore, venueCadence = Some(venueCadenceStore))

  // ONE shared due schedule (`scrapeDueWindow`, an eager member of the root) backs
  // both the scrape reaper (enqueue) and the scrape handler (pickup re-gate), so
  // they agree on what's due and a cinema's scrapes spread across the freshness
  // window instead of falling due in a lockstep wave.
  lazy val scrapeCinemaHandler = new ScrapeCinemaHandler(
    cinemaScrapers.map(s => ScrapeCinemaHandler.scraperKey(s.cinema) -> s).toMap,
    cinemaScrapeRunner, freshnessStore, scrapeDueWindow,
    chunkPlanner = Some(chunkScrapePlanner), scrapeFreshness = Some(scrapeFreshnessPolicy),
    // The same archive the runner writes: it is what says whether a venue is
    // merely failing or has 404'd for over a day.
    scrapeArchive = scrapeArchive
  )

  // Post-boot enqueue ramp window: after a restart, ramp the per-tick scrape cap up
  // over this long instead of enqueuing the full `maxScrapeEnqueuePerTick` from the
  // first tick, so the whole-corpus backlog drains gradually (pool idles → the just-
  // reset CPU-credit balance rebuilds) rather than pinning the pool flat-out and
  // re-draining credit — the residual boot-storm spike. See ScrapeReaper.bootRamp.
  def scrapeBootRampMinutes: Long = Env.positiveLong("KINOWO_SCRAPE_BOOT_RAMP_MINUTES", 5L)
  // How many staggered sub-slices each healthy reaper tick spreads its due batch
  // over the 1-min interval, so the batch's scrape parses don't clump into a single
  // CPU spike that floors the CPU-credit balance (the parse-wave burst). Same total
  // work and freshness; only the enqueue timing is staggered. Sized in ScrapeCadence;
  // 1 disables the spread. See ScrapeReaper.enqueueSpread.
  def scrapeEnqueueSpreadSlices: Int =
    Env.positiveInt("KINOWO_SCRAPE_ENQUEUE_SPREAD_SLICES", ScrapeCadence.EnqueueSpreadSlices)
  // Ceiling on outstanding scrape TASKS, the bound that survives the healthy path —
  // the per-tick caps above count VENUES, which on a chunked country understates the
  // work by the fan-out factor. Sized in ScrapeCadence. See ScrapeReaper's parameter.
  def maxOutstandingScrapeTasks: Int =
    Env.positiveInt("KINOWO_SCRAPE_MAX_OUTSTANDING_TASKS",
      ScrapeCadence.MaxOutstandingScrapeTasks)
  // What one venue costs in scrape tasks, so the budget above can be spent in the unit
  // it is written in. Per country because the fan-out is a property of that country's
  // scrapers — see ScrapeReaper's `tasksPerVenue`. Default 1 (unchunked).
  def scrapeTasksPerVenue: Int = Env.positiveInt("KINOWO_SCRAPE_TASKS_PER_VENUE", 1)
  lazy val scrapeReaper =
    new ScrapeReaper(cinemaScrapers, taskQueue, freshnessStore, dueWindow = scrapeDueWindow,
      initialDelay = initialScrapeDelaySeconds.seconds,
      maxEnqueuePerTick = maxScrapeEnqueuePerTick, bootRamp = scrapeBootRampMinutes.minutes,
      maxOutstandingScrapeTasks = maxOutstandingScrapeTasks, tasksPerVenue = scrapeTasksPerVenue,
      chunkSpread = ScrapeCadence.ChunkEnqueueSpread,
      inFlight = chunkRunInFlight,
      enqueueSpread = scrapeEnqueueSpreadSlices, runStore = scheduledRunStore)
}
