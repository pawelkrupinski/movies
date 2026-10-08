package modules.wiring

import scala.concurrent.duration.DurationInt
import settings.{BootHydrateRetryInterval, CacheRehydrateInterval}

import modules.WorkerWiring
import services.movies.{CaffeineMovieCache, MongoMovieRepository, MovieChangeStream, MongoScreeningsRepository, MongoSlotsRepository, MovieRepository, RetiredVenueRows, ScreeningTokens, VenueRoster, ScreeningsRepository, SlotsRepository, StrandedSideRowsCleanup, TitleNormalizer, UnscreenedCleanup}

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
    mongoConnection.database, clock, changeStreamMetrics = taskMetrics.movieChangeMetrics,
    screeningsMetrics = taskMetrics, slotsMetrics = taskMetrics.slotsChangeMetrics,
    writeMetrics = taskMetrics, decodeFailures = taskMetrics,
    normalizer = titleNormalizer,
    screenings = Some(screeningsRepository),
    slots = Some(slotsRepository),
    changeDebounce = MovieChangeStream.Debounce.forCountry(country),
    // The worker is the durable read-model/cache mirror: persist the change-stream resume
    // token so a restart replays events missed while down instead of leaning on the backstop.
    persistResumeToken = true)

  lazy val movieCache: CaffeineMovieCache = managedResources.stopping(
    new CaffeineMovieCache(movieRepository,
      retrigger = enrichmentRetrigger, cacheMetrics = taskMetrics,
      normalizer = titleNormalizer, clock = clock,
      listingIntakeMetrics = taskMetrics,
      stringPool = workerMetrics.stringPool,
      bootHydrateMaxAttempts = configuration.bootHydrateMaxAttempts,
      bootHydrateRetry       = configuration.bootHydrateRetryInterval(BootHydrateRetryInterval(1.second)),
      rehydrateInterval = configuration.cacheRehydrateInterval(CacheRehydrateInterval(6.hours)),
      // The boot's other whole-corpus readers take the hydrate's read rather than read it again.
      bootReaders = Seq(readModelBootStudy)))

  /** Where the scrape guards keep each venue's state (the listing intake's). */
  lazy val scrapeGuardLedger: services.movies.ScrapeGuardLedger = new services.scrapes.MongoScrapeGuardLedger(mongoConnection.database)

  // This deployment's badge vocabulary. One instance, shared by every path that
  // writes a `Showtime.format`, so the cache and the two detail-merge paths
  // cannot disagree about how the country spells a voice-over.
  lazy val screeningTokens: ScreeningTokens = ScreeningTokens.of(country)

  // After a merge changes an enrichment's input fields, re-kick that enrichment
  // (per case) as a worker task — clearing its freshness stamp so the tmdbId-keyed
  // dedup doesn't skip the re-fetch. See QueueEnrichmentRetrigger / MergeRetrigger.
  lazy val enrichmentRetrigger = new services.tasks.QueueEnrichmentRetrigger(taskQueue, freshnessStore, country, titleNormalizer, clock)

  lazy val unscreenedCleanup = managedResources.stopping(new UnscreenedCleanup(movieCache, movieRepository))

  // Daily: side-collection rows filed under a venue this country's roster no longer lists. Weekly, on
  // the same tick: the backstop for rows whose film is gone, which a film's delete carries with it.
  lazy val strandedSideRowsCleanup = managedResources.stopping(new StrandedSideRowsCleanup(movieRepository,
    retiredVenues = () => RetiredVenueRows.sweep(Some(screeningsRepository), Some(slotsRepository),
      VenueRoster.venuesOf(country), now = clock.instant()),
    afterSweeps = () => retiredVenueCensus.sample(),
    strandedDue = StrandedSideRowsCleanup.weekly(scheduledRunStore, clock)))
}
