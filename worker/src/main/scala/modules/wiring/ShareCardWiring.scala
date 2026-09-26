package modules.wiring

import settings.{PosterDecodeMemoryCap, ShareCardBackfillBatch, ShareCardBackfillMaxBacklog, ShareCardStorageBudget}

import modules.WorkerWiring
import services.readmodel.ShareCardLedger
import services.sharecards.*
import services.tasks.{ClaimedEnqueueReaper, TaskHandler, TaskType}

import scala.concurrent.duration.*

/** ── Film share cards ─────────────────────────────────────────────────────────
 *  The worker renders each film's Open Graph card into this country's share-card directory
 *  (`KINOWO_SHARE_CARD_DIR`, default `/share-cards` — the pod's mount of the node's
 *  `/var/lib/kinowo/share-cards/<cc>`; a process running several countries sets it to
 *  `/share-cards/{cc}`), and the projection records each card's path and version on `web_movies`.
 *  Everything runs on the task queue: renders, the backfill, the prune and budget passes, the end
 *  of a first-publish hold and the Facebook re-scrape. Without a writable directory the whole
 *  pipeline is off and the projection runs with [[ShareCardLedger.none]]. */
trait ShareCardWiring { self: WorkerWiring =>

  lazy val shareCardStore: ShareCardStore =
    new ShareCardStore(configuration.shareCardDirectory.forCountry(country))

  lazy val shareCardsEnabled: Boolean = shareCardStore.usable

  lazy val shareCardMetrics: ShareCardMetrics = workerMetrics.shareCardSeries.forCountry(country.code)

  lazy val shareCardBudget: ShareCardStorageBudget = configuration.shareCardStorageBudget(ShareCardStorageBudget(1024L * 1024 * 1024))

  /** Posters download directly, except a Cloudflare-blocked site's, which go through the egress
   *  its scrapes use: Multikino 403s the worker's IP on every poster as on its pages. That route
   *  is PAID (the proxy, Zyte behind it), so a poster that fails on it is remembered rather than
   *  asked for again by every render and every daily backfill. */
  private lazy val posterDownload: PosterDownload = PosterDownload.routed(new HttpPosterDownload(tls = tlsContext), Map(
    java.net.URI.create(services.cinemas.pl.MultikinoClient.HomeUrl).getHost ->
      new RememberedFailurePosterDownload(new EgressPosterDownload(multikinoPosterFetch), shareCardStore.failedPosters, clock)))

  /** Shrinks each poster to the card's slot — through the process's one gate, shared with every
   *  other country's renders. */
  lazy val posterShrinker: VipsPosterShrinker =
    new VipsPosterShrinker(
      binary      = VipsPosterShrinker.locate(configuration.executableSearchPath),
      memoryCapMb = configuration.posterDecodeMemoryCap(PosterDecodeMemoryCap(PosterPipeline.DefaultDecodeMemoryCapMb)).megabytes,
      gate        = posterShrinkGate)

  lazy val shareCardService: ShareCardService = new ShareCardService(
    country, shareCardStore,
    new ShareCardPosters(shareCardStore, posterDownload, posterShrinker, shareCardMetrics),
    taskQueue, shareCardMetrics, clock)

  /** What the projection asks about share cards. */
  lazy val shareCardLedger: ShareCardLedger = if (shareCardsEnabled) shareCardService else ShareCardLedger.none

  lazy val shareCardJanitor: ShareCardJanitor = new ShareCardJanitor(
    shareCardStore, readModelRepository, shareCardBudget.bytes, shareCardMetrics, clock,
    refresh = readModelProjector.refreshShareCard)

  lazy val shareCardBackfill: ShareCardBackfill =
    new ShareCardBackfill(shareCardService, readModelRepository, taskQueue, shareCardMetrics, clock,
      batch      = configuration.shareCardBackfillBatch(ShareCardBackfillBatch(ShareCardBackfill.DefaultBatch)).value,
      maxBacklog = configuration.shareCardBackfillMaxBacklog(ShareCardBackfillMaxBacklog(ShareCardBackfill.DefaultMaxBacklog)).value)

  lazy val shareCardFollowUp: ShareCardFollowUp =
    new ShareCardFollowUp(shareCardStore, shareCardService.superseded, readModelProjector.refreshShareCard, readModelProjector.releaseShareCardHold)

  lazy val shareCardHandlers: Seq[TaskHandler] =
    if (!shareCardsEnabled) Nil
    else Seq(
      new RenderShareCardHandler(shareCardService),
      new ShareCardBackfillHandler(shareCardBackfill),
      new PruneShareCardsHandler(shareCardJanitor),
      new ReleaseShareCardHoldHandler(() => readModelProjector.releaseExpiredHolds()),
      new RescrapeShareCardHandler(new ShareCardRescraper(FacebookGraph.fromConfiguration(configuration, tlsContext), readModelRepository, country, shareCardMetrics, clock)))

  /** The recurring enqueues: a backfill tick every minute (first three minutes after boot), the
   *  budget pass every ten, the full prune daily (first five minutes after boot). Each window is
   *  claimed, so one replica enqueues it. */
  lazy val shareCardReapers: Seq[ClaimedEnqueueReaper] =
    if (!shareCardsEnabled) Nil
    else {
      def enqueue(taskType: TaskType, key: String, payload: Map[String, String] = Map.empty): () => Unit =
        () => { taskQueue.enqueue(taskType, key, payload, submittedAt = clock.instant()); () }
      Seq(
        new ClaimedEnqueueReaper("share-card-backfill", enqueue(TaskType.ShareCardBackfill, "share-card-backfill"),
          1.minute, 3.minutes, scheduledRunStore, clock),
        new ClaimedEnqueueReaper("share-card-budget",
          enqueue(TaskType.PruneShareCards, "share-card-budget", Map(PruneShareCardsHandler.ModeKey -> PruneShareCardsHandler.Budget)),
          10.minutes, 4.minutes, scheduledRunStore, clock),
        new ClaimedEnqueueReaper("share-card-prune",
          enqueue(TaskType.PruneShareCards, "share-card-prune", Map(PruneShareCardsHandler.ModeKey -> PruneShareCardsHandler.Daily)),
          24.hours, 5.minutes, scheduledRunStore, clock))
    }
}
