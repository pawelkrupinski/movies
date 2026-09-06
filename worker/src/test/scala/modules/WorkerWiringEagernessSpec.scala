package modules

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import tools.TestWiring

/** Pins what CONSTRUCTING the worker composition root leaves unbuilt, so the
 *  split of `WorkerWiring` into per-subsystem `modules.wiring.*` traits cannot
 *  quietly turn a lazy subsystem eager. Construction runs the root's eager
 *  members — the bus subscriptions and the three `DueWindow`s — which reach the
 *  TMDB stage, the IMDb-id resolver, the detail enqueuers (and through them the
 *  scraper catalogue), the staging reaper and the chunk coordinator. Every
 *  reaper, the task worker, the censuses, the read-model projector, the rating
 *  and operator handlers and the alerters stay unbuilt until `start()`.
 *
 *  The probe overrides one or more representative members per subsystem with a
 *  sentinel that names itself and throws, so a construction that forces any of
 *  them fails by name. Each entry was observed unforced on the pre-split root;
 *  a member moved into a trait's eager initialiser, or a new eager
 *  cross-reference, surfaces here as that member's name. */
class WorkerWiringEagernessSpec extends AnyFlatSpec with Matchers {

  final case class ForcedAtConstruction(member: String)
    extends RuntimeException(s"`$member` was forced while constructing WorkerWiring")

  class SentinelWiring extends TestWiring {
    private def sentinel[A](name: String): A = throw ForcedAtConstruction(name)

    // Cinema scraping
    override lazy val cinemaScrapers          = sentinel("cinemaScrapers")
    override lazy val cinemaScrapeRunner      = sentinel("cinemaScrapeRunner")
    override lazy val scrapeArchive           = sentinel("scrapeArchive")
    override lazy val scrapeCinemaHandler     = sentinel("scrapeCinemaHandler")
    override lazy val scrapeReaper            = sentinel("scrapeReaper")
    override lazy val filmwebFallbackStore    = sentinel("filmwebFallbackStore")
    override lazy val hostScrapeStats         = sentinel("hostScrapeStats")
    // Chunked scrape machinery
    override lazy val chunkScrapePlanner      = sentinel("chunkScrapePlanner")
    override lazy val chunkScrapeReaper       = sentinel("chunkScrapeReaper")
    override lazy val scrapeChunkHandler      = sentinel("scrapeChunkHandler")
    // Deferred detail
    override lazy val enrichDetailsHandler    = sentinel("enrichDetailsHandler")
    override lazy val detailReaper            = sentinel("detailReaper")
    // Movies corpus
    override lazy val unscreenedCleanup       = sentinel("unscreenedCleanup")
    // Resolution
    override lazy val crewConfirmation        = sentinel("crewConfirmation")
    override lazy val unresolvedTmdbReaper    = sentinel("unresolvedTmdbReaper")
    override lazy val settleReaper            = sentinel("settleReaper")
    // Ratings
    override lazy val imdbRatings             = sentinel("imdbRatings")
    override lazy val ratingCadenceStore      = sentinel("ratingCadenceStore")
    override lazy val ratingHandlers          = sentinel("ratingHandlers")
    override lazy val ratingEnqueuer          = sentinel("ratingEnqueuer")
    override lazy val enrichmentReaper        = sentinel("enrichmentReaper")
    override lazy val omdbBackfillReaper      = sentinel("omdbBackfillReaper")
    // Read model
    override lazy val readModelRepository     = sentinel("readModelRepository")
    override lazy val readModelProjector      = sentinel("readModelProjector")
    // Metrics
    override lazy val corpusScan              = sentinel("corpusScan")
    override lazy val cinemaScrapeCensus      = sentinel("cinemaScrapeCensus")
    override lazy val cinemaContentCensus     = sentinel("cinemaContentCensus")
    // Task queue + worker
    override lazy val taskWorker              = sentinel("taskWorker")
    override lazy val workerHeartbeat         = sentinel("workerHeartbeat")
    override lazy val livenessWatchdog        = sentinel("livenessWatchdog")
    // Staging
    override lazy val stagingHandlers         = sentinel("stagingHandlers")
    override lazy val stagingFolder           = sentinel("stagingFolder")
    // Alerting
    override protected lazy val stagingStuckAlerter   = sentinel("stagingStuckAlerter")
    override protected lazy val filmwebDropAlerter    = sentinel("filmwebDropAlerter")
    override protected lazy val scrapeOutcomeListener = sentinel("scrapeOutcomeListener")
    // Operator
    override lazy val envConfigService        = sentinel("envConfigService")
    override lazy val operatorHandlers        = sentinel("operatorHandlers")
    override lazy val bulkTaskResultStore     = sentinel("bulkTaskResultStore")
  }

  "Constructing WorkerWiring" should "build no reaper, handler, census, projector, worker or alerter before start()" in {
    try new SentinelWiring
    catch { case ForcedAtConstruction(member) => fail(s"construction forced `$member`") }
  }
}
