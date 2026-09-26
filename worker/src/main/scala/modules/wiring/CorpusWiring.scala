package modules.wiring

import scala.concurrent.duration.DurationInt
import settings.{BootHydrateRetryInterval, CacheRehydrateInterval}

import modules.WorkerWiring
import services.movies.{CaffeineMovieCache, MongoMovieRepository, MongoScreeningsRepository, MongoSlotsRepository, MovieRepository, RetiredVenueRows, ScreeningTokens, VenueRoster, ScreeningsRepository, SlotsRepository, StrandedSideRowsCleanup, TitleNormalizer, UnscreenedCleanup}

/** ── MovieRecord cache (write-through) ───────────────────────────────────────
 *  The `movies` corpus and its side collections, the write-through cache every
 *  scrape and enrichment writes through, and the per-country title vocabulary
 *  they all key by. */
trait CorpusWiring { self: WorkerWiring =>

  // Showtimes split: per-cinema showtimes live in the separate `screenings`
  // collection, not embedded in the (formerly 1-2MB) movies doc the change stream
  // re-decodes on every write. Wiring the screenings repo turns the split on — movies
  // is written without showtimes, reads stitch them from `screenings`. `start()` runs
  // the one-shot backfill before the cache hydrates.
  lazy val screeningsRepository: ScreeningsRepository =
    // Persist the screenings stream's resume token too (like `movies`): a showtime change
    // writes only `screenings`, so without this a restart drops showtime edits made while
    // down and only the full reproject catches them — the gap that kept it non-redundant.
    new MongoScreeningsRepository(mongoConnection.database, persistResumeToken = true,
      metrics = taskMetrics, roster = VenueRoster.of(country), writeMetrics = taskMetrics, decodeFailures = taskMetrics)
  // Slot split: the per-cinema SourceData lives in `movie_slots`, one row per slot,
  // for the same reason showtimes moved to `screenings` — an UPDATE_LOOKUP change event
  // otherwise carries the whole film document, and those documents queue up on an
  // unbounded executor (one poster URL was resident 10,848 times in the 2026-07-27 UK
  // OOM dump).
  lazy val slotsRepository: SlotsRepository =
    // Its own persisted resume token and its own counters, like `screenings`: a venue's slot
    // lands without a `movies` write whenever the film document is unchanged, so this is the
    // third cursor on the projector and a restart must replay it too.
    new MongoSlotsRepository(mongoConnection.database, persistResumeToken = true,
      metrics = taskMetrics.slotsChangeMetrics, roster = VenueRoster.of(country), writeMetrics = taskMetrics, decodeFailures = taskMetrics)
  /** This wiring's country's title rules — the ONE instance every component below
   *  keys through, so a worker running several countries cannot fold one country's
   *  titles with another's. Every component takes its normalizer as a required
   *  argument, so there is no environment-resolved fallback to inherit. */
  lazy val titleNormalizer: TitleNormalizer =
    TitleNormalizer.forCountry(country)

  lazy val movieRepository: MovieRepository = new MongoMovieRepository(
    mongoConnection.database, changeStreamMetrics = taskMetrics.movieChangeMetrics,
    screeningsMetrics = taskMetrics, slotsMetrics = taskMetrics.slotsChangeMetrics,
    writeMetrics = taskMetrics, decodeFailures = taskMetrics,
    normalizer = titleNormalizer,
    screenings = Some(screeningsRepository),
    slots = Some(slotsRepository),
    // The worker is the durable read-model/cache mirror: persist the change-stream resume
    // token so a restart replays events missed while down instead of leaning on the backstop.
    persistResumeToken = true)

  // Staging-ingest: a genuinely-new film incubates in `pending_movies`
  // (resolve-then-fold) instead of landing straight in `movies`; a film already
  // known to `movies` keeps the direct path. The `staging` sink is wired into the
  // cache, the promoter scheduled and the fold subscribed (in the root)
  // unconditionally.
  /** The merge counter the cache reports to — production's task metrics. A seam, so a harness
   *  that counts merges by reason overrides THIS rather than rebuilding the cache, which
   *  silently drops every argument it forgets (the scrape-guard ledger, the clock, the
   *  screening tokens — see `CountryConvergenceBehaviour`). */
  protected def cacheMergeMetrics: services.movies.MergeMetrics = taskMetrics

  lazy val movieCache: CaffeineMovieCache =
    new CaffeineMovieCache(movieRepository, eventBus, staging = Some(stagingRepository),
      retrigger = enrichmentRetrigger, mergeMetrics = cacheMergeMetrics, cacheMetrics = taskMetrics,
      // The composition root's clock, not the cache's own default: the scrape guards judge a
      // tick by the showtimes still UPCOMING, and "upcoming" must mean the same instant for
      // the cache as for everything else the root wires. Production's is the system clock
      // either way; a harness that moves its clock a day on moved nothing here, so the depth
      // guard measured the next day's listing against showtimes the day had already passed
      // and discarded the tick.
      clock = clock,
      enrichmentLanguage = country.language, screeningTokens = screeningTokens, normalizer = titleNormalizer,
      scrapeLandingMetrics = taskMetrics,
      // Durable, so the guards' grace and each venue's recorded source survive a
      // rollout — held in memory they reset on every pod change.
      scrapeGuardLedger = scrapeGuardLedger,
      // The process's one intern pool, shared with every other country's cache.
      stringPool = workerMetrics.stringPool,
      bootHydrateMaxAttempts = configuration.bootHydrateMaxAttempts,
      bootHydrateRetry       = configuration.bootHydrateRetryInterval(BootHydrateRetryInterval(1.second)),
      maxConsecutiveGuardRejections =
        services.movies.ScrapeHealth.maxRejectionsFor(scrapeFreshness),
      rehydrateInterval = configuration.cacheRehydrateInterval(CacheRehydrateInterval(6.hours)))

  /** Where the scrape guards keep each venue's state — the landing's, or a cut-over country's
   *  listing intake's: one ledger, so a country switched between paths keeps one count. */
  lazy val scrapeGuardLedger: services.movies.ScrapeGuardLedger = new services.scrapes.MongoScrapeGuardLedger(mongoConnection.database)

  // This deployment's badge vocabulary. One instance, shared by every path that
  // writes a `Showtime.format`, so the cache and the two detail-merge paths
  // cannot disagree about how the country spells a voice-over.
  lazy val screeningTokens: ScreeningTokens = ScreeningTokens.of(country)

  // After a merge changes an enrichment's input fields, re-kick that enrichment
  // (per case) as a worker task — clearing its freshness stamp so the tmdbId-keyed
  // dedup doesn't skip the re-fetch. See QueueEnrichmentRetrigger / MergeRetrigger.
  lazy val enrichmentRetrigger = new services.tasks.QueueEnrichmentRetrigger(taskQueue, freshnessStore, country, titleNormalizer)

  lazy val unscreenedCleanup = new UnscreenedCleanup(movieCache, movieRepository)

  // The other daily sweep: side-collection rows whose film left the corpus before
  // deletes and merges carried their rows with them — and, on the same tick, rows filed
  // under a venue this country's roster no longer lists.
  lazy val strandedSideRowsCleanup = new StrandedSideRowsCleanup(movieRepository,
    retiredVenues = () => RetiredVenueRows.sweep(Some(screeningsRepository), Some(slotsRepository),
      VenueRoster.venuesOf(country), now = clock.instant()),
    afterSweeps = () => retiredVenueCensus.sample())
}
