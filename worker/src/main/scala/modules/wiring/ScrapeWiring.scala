package modules.wiring

import settings.{ScrapeBootRamp, ScrapeEnqueueSpreadSlices, ScrapeInitialDelay, ScrapeMaxEnqueuePerTick, ScrapeMaxOutstandingTasks, ScrapeTasksPerVenue}

import models.Cinema
import modules.WorkerWiring
import services.MongoCachingDetailFetch
import services.cinemas.{ChainFlicksFallback, CinemaScraperCatalog}
import services.cinemas.common.{AdaptiveTimeoutScraper, ChunkedCinemaScraper, CinemaClientMarkers, CinemaScrapeRunner, CinemaScraper, FallbackAfter, FallbackEligibility, FlicksClient, KinoprogrammClient, RetryingCinemaScraper, SourceFallbackScraper, UptimeRecordingScraper}
import services.cinemas.pl.{FilmwebCinemaIdResolver, FilmwebShowtimesClient}
import services.alerts.{FallbackAlert, GoneVenueAlertingArchive}
import services.observations.ObservingScrapeArchive
import services.fallback.{FallbackEvent, FallbackState, FallbackStore, MongoFallbackStore}
import services.scrapes.{MongoScrapeArchiveRepository, ScrapeArchiveRepository}
import services.tasks.{ScrapeCadence, ScrapeCinemaHandler, ScrapeFreshnessPolicy, ScrapeReaper}
import tools.{DaemonExecutors, HostScrapeStats}

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
    (chain, h, ttl) => new MongoCachingDetailFetch(h, mongoConnection.database, ttl, s"detailCache-$chain",
      workerMetrics.ttlIndexMismatches),
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

  /** THIS country's slice of the global catalog, every city included — what every
   *  consumer below walks. `cinemaScraperCatalog.all` spans every country; a wiring
   *  that iterates it builds, tags or fetches for venues it does not serve (the
   *  Cineworld detail enricher in every country's worker, f4c7ac583; every country's
   *  venues tagged into each country's /uptime). Narrowed further by
   *  [[scrapeCities]] where only the scraped subset matters. */
  lazy val countryScrapers: Seq[CinemaScraper] =
    country.cities.flatMap(c => cinemaScraperCatalog.byCity.getOrElse(c.slug, Nil))

  // This country's own cities — the default scrape set. A country wiring only
  // ever scrapes its own cities (never another country's), so the default is
  // `country.cities`, not the global `City.all` union. KINOWO_SCRAPE_CITIES only
  // NARROWS within that set (e.g. to shed load if the worker throttles/OOMs).
  protected def scrapeCitiesDefault: Set[String] = country.cities.map(_.slug).toSet
  // `protected def` so test wirings can pin the set independently.
  protected def scrapeCities: Set[String] =
    configuration.scrapeCitySlugs.fold(scrapeCitiesDefault)(_.value)

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
  // resolves or fetches Filmweb live (see TestWiring). A country whose Filmweb path
  // is off gets no Filmweb wrapper at all (`recordingScraper`), not merely an empty map.
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
  // derived once from this country's slice of the catalog — the whole catalog tagged
  // every country's venues (thousands of US/UK/DE/ES rows) into each country's
  // /uptime tag store at boot. Shared by the boot reconcile and the per-event
  // retag so the FtFW tag is layered on top of — never instead of — the marker.
  protected lazy val clientMarkers: Map[String, String] =
    CinemaClientMarkers.markers(countryScrapers)

  // The per-cinema public source-page URL ("url:<https…>"), derived once from
  // this country's slice alongside the client marker (off the whole catalog, every
  // non-Polish worker resolved Poland's Filmweb links over HTTP at boot) so the /uptime page can link each
  // cinema name to the page we scrape. Rides the same tag channel. Filmweb-backed
  // venues are upgraded from their `/cinema/-<id>` fallback to the canonical,
  // browser-renderable `/showtimes/<City>/<Name>-<id>` page, resolved once from
  // Filmweb's /info at boot (city + name aren't in our model); tolerant, so a
  // venue whose resolve fails keeps the fallback.
  protected lazy val sourceUrls: Map[String, String] = {
    val base           = CinemaClientMarkers.sourceUrls(countryScrapers)
    val filmwebClients = countryScrapers.collect { case f: FilmwebShowtimesClient => f }
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

  // This country's cinemas whose ONLY scraper is a FilmwebShowtimesClient — served by Filmweb by
  // design, not as a fallback. Feeds the FilmwebDropAlerter (a Filmweb-only venue
  // going empty means migrate it to an own-site scraper).
  lazy val filmwebOnlyCinemas: Set[String] =
    countryScrapers.groupBy(_.cinema)
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

  /** Cinema → its kinoprogramm.com page, for German venues whose Filmstarts scrape
   *  has kinoprogramm as its FALLBACK. */
  protected def kinoprogrammFallbackPaths: Map[Cinema, String] =
    cinemaScraperCatalog.kinoprogrammFallbackPaths

  /** A venue's fallback: the feed that serves it once its own scrape keeps failing. */
  private final case class FallbackPlan(
    name:   String,                        // "Flicks", "Kinoprogramm", "Filmweb" — the /uptime label + Telegram text
    ref:    () => Option[String],          // the feed's per-venue handle, for the status page's link
    client: () => Option[CinemaScraper],
    after:  FallbackAfter
  )

  /** Which fallback a venue gets, if any — the ONE decision both [[recordingScraper]]
   *  and [[venuesPagedElsewhere]] read, so they cannot disagree about who is covered:
   *   - a chain venue → Flicks, on the market its catalogue entry names (flicks.co.uk
   *     for the UK chains, flicks.us for the US ones);
   *   - a German venue kinoprogramm.com lists → Kinoprogramm, once its Filmstarts
   *     scrape has failed [[KinoprogrammFailedRuns]] SEPARATE runs: German venues are
   *     scraped ~10-hourly, so a 6h window would hand over on the second failure;
   *   - an eligible single venue in a Filmweb country (Poland) → Filmweb. Outside
   *     Poland Filmweb lists nothing, so a wrapper there could only page "Filmweb has
   *     nothing to serve" for a venue it never could have covered. */
  private def fallbackFor(cinema: Cinema, eligible: Boolean): Option[FallbackPlan] = {
    val sixHours = FallbackAfter.FailingFor(SourceFallbackScraper.DefaultFallbackAfter)
    flicksFallbackSlugs.get(cinema).map { case ChainFlicksFallback.FlicksFallback(market, slug) =>
      // The market comes from the map, not a constant: a US chain venue's fallback
      // lives on flicks.us, and looking it up on flicks.co.uk would just 404.
      FallbackPlan("Flicks", () => Some(slug), () => Some(new FlicksClient(flicksFetch, slug, cinema, market)), sixHours)
    }.orElse(kinoprogrammFallbackPaths.get(cinema).map { path =>
      FallbackPlan("Kinoprogramm", () => Some(path),
        () => Some(new KinoprogrammClient(httoFetch, path, cinema,
          today = Some(java.time.LocalDate.now(clock.withZone(KinoprogrammClient.Zone))))),
        FallbackAfter.FailedRuns(KinoprogrammFailedRuns))
    }).orElse(Option.when(eligible && filmwebEnabled)(
      FallbackPlan("Filmweb", () => filmwebFallbackIds.get(cinema).map(_.toString), () => filmwebFallbackFor(cinema), sixHours)))
  }

  /** Separate failed Filmstarts runs before a German venue hands over to kinoprogramm.com:
   *  several, so one bad scrape — or two — rides out on the last good listing. */
  private val KinoprogrammFailedRuns = 3

  /** Wrap a scrape source with the outcome recorder + its fallback source
   *  ([[fallbackFor]]), or the plain uptime recorder when it has none. One
   *  source-neutral [[SourceFallbackScraper]] serves every feed. Extracted so the
   *  chunked reduce step (`publishScrape`) records uptime + falls back exactly like a
   *  live scrape. Both wrappers run on the wiring's [[clock]], the one `uptimeMonitor`
   *  stamps buckets with, so the fixture harness's pinned day judges its own corpus. */
  private[wiring] def recordingScraper(inner: CinemaScraper, eligible: Boolean): CinemaScraper =
    fallbackFor(inner.cinema, eligible).fold[CinemaScraper](
      new UptimeRecordingScraper(inner, uptimeMonitor, scrapeOutcomeListener, clock)
    )(plan =>
      new SourceFallbackScraper(inner,
        fallback = plan.client, fallbackName = plan.name, fallbackRef = plan.ref,
        uptimeMonitor, filmwebFallbackStore, now = () => clock.instant(),
        fallbackAfter = plan.after, onEvent = filmwebFallbackOnEvent))

  /** The venues another alert already pages for when their page is gone: those with
   *  a fallback (`SourceFallbackScraper` pages UNCOVERED) and the Filmweb-only ones
   *  (`FilmwebDropAlerter`). [[GoneVenueAlertingArchive]] pages for the rest. */
  lazy val venuesPagedElsewhere: Set[String] =
    countryScrapers
      .filter(s => fallbackFor(s.cinema, FallbackEligibility.eligible(s)).isDefined)
      .map(_.cinema.displayName).toSet ++ filmwebOnlyCinemas

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
  def initialScrapeDelay: ScrapeInitialDelay = configuration.scrapeInitialDelay(ScrapeInitialDelay(45.seconds))

  // Cap on stale cinemas enqueued per reaper tick. A cold boot (or a long backlog)
  // would otherwise queue every cinema at once and let the TaskWorker pool drain
  // flat-out for minutes, exhausting the shared-CPU credit balance — the boot-storm
  // throttle spike. ~25/min drains inside the 1-min tick, leaving idle gaps for
  // credit to recover; the backlog clears over a handful of ticks. Tune down if a
  // restart still throttles, up once Mongo/CPU have headroom. Default sized in
  // ScrapeCadence (≥1.5× the steady-state due rate at the freshness window).
  def maxScrapeEnqueuePerTick: ScrapeMaxEnqueuePerTick =
    configuration.scrapeMaxEnqueuePerTick(ScrapeMaxEnqueuePerTick(ScrapeCadence.MaxEnqueuePerTick))

  // Cinema scraping is driven by a durable Mongo task queue: the ScrapeReaper
  // enqueues each cinema at most once per freshness window, and the TaskWorker
  // scrapes it (skipping if a concurrent run already refreshed it). Detail and
  // rating enrichment are governed by KINOWO_DEFERRED_DETAIL and
  // KINOWO_QUEUE_ENRICHMENT independently.

  // The shared scrape core: record + decide-trigger, injected into
  // ScrapeCinemaHandler. Detail enqueue is event-driven (DetailTaskEnqueuer off
  // CinemaMovieAdded) plus the DetailReaper backstop; the runner publishes
  // MovieDetailsComplete only for rows that don't await deferred detail.
  // The runner archives through the observing archive when the identity program's shadow
  // capture is on (`observationStore`): every scraped listing becomes an observation too.
  // Wrapped once more, outermost, to page for a gone venue nothing else pages for.
  lazy val cinemaScrapeRunner = new CinemaScrapeRunner(movieCache, eventBus, deferredDetailCinemas,
    new GoneVenueAlertingArchive(
      observationStore.fold(scrapeArchive)(new ObservingScrapeArchive(scrapeArchive, _)),
      venuesPagedElsewhere,
      message => fallbackTelegramNotifier.foreach(_.send(message))))

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
  def scrapeBootRamp: ScrapeBootRamp = configuration.scrapeBootRamp(ScrapeBootRamp(5.minutes))
  // How many staggered sub-slices each healthy reaper tick spreads its due batch
  // over the 1-min interval, so the batch's scrape parses don't clump into a single
  // CPU spike that floors the CPU-credit balance (the parse-wave burst). Same total
  // work and freshness; only the enqueue timing is staggered. Sized in ScrapeCadence;
  // 1 disables the spread. See ScrapeReaper.enqueueSpread.
  def scrapeEnqueueSpreadSlices: ScrapeEnqueueSpreadSlices =
    configuration.scrapeEnqueueSpreadSlices(ScrapeEnqueueSpreadSlices(ScrapeCadence.EnqueueSpreadSlices))
  // Ceiling on outstanding scrape TASKS, the bound that survives the healthy path —
  // the per-tick caps above count VENUES, which on a chunked country understates the
  // work by the fan-out factor. Sized in ScrapeCadence. See ScrapeReaper's parameter.
  def maxOutstandingScrapeTasks: ScrapeMaxOutstandingTasks =
    configuration.scrapeMaxOutstandingTasks(ScrapeMaxOutstandingTasks(ScrapeCadence.MaxOutstandingScrapeTasks))
  // What one venue costs in scrape tasks, so the budget above can be spent in the unit
  // it is written in. Per country because the fan-out is a property of that country's
  // scrapers — see ScrapeReaper's `tasksPerVenue`. Default 1 (unchunked).
  def scrapeTasksPerVenue: ScrapeTasksPerVenue = configuration.scrapeTasksPerVenue(ScrapeTasksPerVenue(1))
  lazy val scrapeReaper =
    new ScrapeReaper(cinemaScrapers, taskQueue, freshnessStore, dueWindow = scrapeDueWindow,
      initialDelay = initialScrapeDelay,
      maxEnqueuePerTick = maxScrapeEnqueuePerTick, bootRamp = scrapeBootRamp,
      maxOutstandingScrapeTasks = maxOutstandingScrapeTasks, tasksPerVenue = scrapeTasksPerVenue,
      chunkSpread = settings.ScrapeChunkSpread(ScrapeCadence.ChunkEnqueueSpread),
      inFlight = chunkRunInFlight,
      enqueueSpread = scrapeEnqueueSpreadSlices, runStore = scheduledRunStore)
}
