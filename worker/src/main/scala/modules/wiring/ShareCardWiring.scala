package modules.wiring

import settings.{PosterDecodeMemoryCap, ShareCardBackfillBatch, ShareCardBackfillMaxBacklog, ShareCardStorageBudget}

import modules.WorkerWiring
import services.readmodel.ShareCardLedger
import services.sharecards.*
import services.{MongoConnection, MongoRequirement, MongoTuning}
import services.tasks.{ClaimedEnqueueReaper, TaskHandler, TaskType}

import scala.concurrent.duration.*

/** ── Film share cards ─────────────────────────────────────────────────────────
 *  The worker renders each film's Open Graph card into this country's share-card directory
 *  (`KINOWO_SHARE_CARD_DIR`, default `/share-cards` — the pod's mount of the node's
 *  `/var/lib/kinowo/share-cards/<cc>`; a process running several countries sets it to
 *  `/share-cards/{cc}`), and the projection records each card's path and version on `web_movies`.
 *  Everything runs on the task queue — renders, the backfill, the prune and budget passes, the end
 *  of a first-publish hold — except the Facebook re-scrapes, which the whole fleet queues in its
 *  shared database and drains on one quota, each country on its own thread
 *  ([[FacebookRescrapeDrain]]). Without a writable directory the whole
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
      memoryCap   = configuration.posterDecodeMemoryCap(PosterDecodeMemoryCap(PosterPipeline.DefaultDecodeMemoryCapMb)),
      gate        = posterShrinkGate)

  lazy val shareCardService: ShareCardService = new ShareCardService(
    country, shareCardStore,
    new ShareCardPosters(shareCardStore, posterDownload, posterShrinker, shareCardMetrics),
    taskQueue, shareCardRescrapes, shareCardMetrics, clock)

  /** The fleet database — one for every country's worker, on the same cluster (and, in a
   *  multi-country process, the same client) as the country's own. Optional: unreachable or
   *  refused, it is only the Facebook re-scrapes that stop. */
  lazy val fleetMongoConnection: MongoConnection =
    MongoConnection.forDatabase(mongoAddress.uri, ShareCardWiring.FleetDatabase, MongoRequirement.Optional,
      MongoTuning.from(configuration), sharedMongoClient)

  private lazy val facebookGraph: Option[FacebookGraph] = FacebookGraph.fromConfiguration(configuration, tlsContext)

  /** The fleet's re-scrape queue, when there is anything to send with and anywhere to queue. */
  private lazy val facebookRescrapeStore: Option[FacebookRescrapeStore] =
    facebookGraph.flatMap(_ => fleetMongoConnection.database).map(db =>
      new MongoFacebookRescrapeStore(db.getCollection(MongoFacebookRescrapeStore.Collection)))

  lazy val shareCardRescrapes: ShareCardRescrapes = facebookRescrapeStore match {
    case Some(store) => new FacebookRescrapeQueue(store, country.code)
    case None =>
      if (facebookGraph.isDefined) logger.error(s"share card: Facebook re-scrapes are OFF for ${country.code} — " +
        s"the fleet database ${ShareCardWiring.FleetDatabase.value} could not be reached or used")
      ShareCardRescrapes.disabled(shareCardMetrics)
  }

  /** This country's side of draining the fleet's re-scrape queue, on its own thread. */
  lazy val facebookRescrapeDrain: Option[FacebookRescrapeDrain] =
    for { store <- facebookRescrapeStore; graph <- facebookGraph if shareCardsEnabled }
    yield new FacebookRescrapeDrain(store, graph, new FilmPageUrls(readModelRepository, country, clock), country.code, shareCardMetrics, clock)

  def startFacebookRescrapes(): Unit = facebookRescrapeDrain.foreach(_.start())

  /** Stops the drain and closes the fleet database — only when this wiring opened it. */
  def stopFacebookRescrapes(): Unit = {
    facebookRescrapeDrain.foreach(_.stop())
    facebookRescrapeStore.foreach(_ => fleetMongoConnection.close())
  }

  /** What the projection asks about share cards. */
  lazy val shareCardLedger: ShareCardLedger = if (shareCardsEnabled) shareCardService else ShareCardLedger.none

  lazy val shareCardJanitor: ShareCardJanitor = new ShareCardJanitor(
    shareCardStore, readModelRepository, shareCardBudget, shareCardMetrics, clock,
    refresh = readModelProjector.refreshShareCard)

  lazy val shareCardBackfill: ShareCardBackfill =
    new ShareCardBackfill(shareCardService, readModelRepository, taskQueue, shareCardMetrics, clock,
      batch      = configuration.shareCardBackfillBatch(ShareCardBackfillBatch(ShareCardBackfill.DefaultBatch)),
      maxBacklog = configuration.shareCardBackfillMaxBacklog(ShareCardBackfillMaxBacklog(ShareCardBackfill.DefaultMaxBacklog)))

  lazy val shareCardFollowUp: ShareCardFollowUp =
    new ShareCardFollowUp(shareCardStore, shareCardService.superseded, readModelProjector.refreshShareCard, readModelProjector.releaseShareCardHold)

  lazy val shareCardHandlers: Seq[TaskHandler] =
    if (!shareCardsEnabled) Nil
    else Seq(
      new RenderShareCardHandler(shareCardService),
      new ShareCardBackfillHandler(shareCardBackfill),
      new PruneShareCardsHandler(shareCardJanitor),
      new ReleaseShareCardHoldHandler(() => readModelProjector.releaseExpiredHolds()),
      new RescrapeShareCardHandler(shareCardRescrapes, clock))

  /** The recurring enqueues: a backfill tick every minute (first three minutes after boot), the
   *  budget pass every ten, the full prune daily at 03:00 UTC (or five minutes after a boot that
   *  finds that day's prune never ran). Each window is claimed, so one replica enqueues it. */
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
          24.hours, 5.minutes, scheduledRunStore, clock, ClaimedEnqueueReaper.Timing.Aligned(ShareCardWiring.DailyPruneAt)))
    }
}

object ShareCardWiring {
  /** When the daily share-card prune runs, past midnight UTC: a fixed time, so its burst of
   *  deletions lands at one place on the dashboard however the day's deploys fall. */
  val DailyPruneAt: FiniteDuration = 3.hours

  /** The database every country's worker shares: the fleet's Facebook re-scrape queue and quota.
   *
   *  POLAND'S DATABASE, NOT A DEDICATED ONE, for the reason the web's shared users database is
   *  (`MONGODB_USERS_DB: kinowo`, see the web manifest): the deployments' Mongo user is scoped to
   *  the country databases, so a new database fails every write `Unauthorized` until the server
   *  grants it. `kinowo` is one every worker already holds rights on; the queue's one collection
   *  sits beside the Polish corpus and nothing that prunes the corpus reads it. */
  val FleetDatabase: settings.MongoDatabaseName = settings.MongoDatabaseName(models.Country.Poland.mongoDb)
}
