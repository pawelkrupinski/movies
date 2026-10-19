package modules.wiring

import settings.{PosterDecodeMemoryCap, ShareCardBackfillMaxBacklog, ShareCardMinimumFree, ShareCardStorageBudget}

import modules.WorkerWiring
import services.readmodel.ShareCardLedger
import services.sharecards.*
import services.{MongoConnection, MongoRequirement, MongoTuning}
import services.tasks.{ClaimedEnqueueReaper, TaskHandler, TaskType}

import scala.concurrent.duration.*

/** ── Film and city share cards ──────────────────────────────────────────────
 *  The worker renders each film's and each city's Open Graph card into this country's share-card directory
 *  (`KINOWO_SHARE_CARD_DIR`, default `/share-cards` — the pod's mount of the node's
 *  `/var/lib/kinowo/share-cards/<cc>`; a process running several countries sets it to
 *  `/share-cards/{cc}`), and the projection records each card's path and version on `web_movies`.
 *  Everything runs on the task queue — renders, the prune and budget passes, the end of a
 *  first-publish hold, with the backfill riding their completions and the projection's — except
 *  the Facebook re-scrapes, which the whole fleet queues in its shared database and drains on one
 *  quota, each country on its own thread ([[FacebookRescrapeDrain]]). Without a writable directory the whole
 *  pipeline is off and the projection runs with [[ShareCardLedger.none]]. */
trait ShareCardWiring { self: WorkerWiring =>

  lazy val shareCardStore: ShareCardStore = {
    val directory = configuration.shareCardDirectory.forCountry(country)
    new ShareCardStore(directory, ShareCardRoom.onVolumeOf(directory,
      configuration.shareCardMinimumFree(ShareCardMinimumFree(ShareCardWiring.DefaultMinimumFreeMb * 1024 * 1024)), shareCardMetrics.refused))
  }

  lazy val shareCardsEnabled: Boolean = shareCardStore.usable

  lazy val shareCardMetrics: ShareCardMetrics = workerMetrics.shareCardSeries.forCountry(country.code)

  lazy val shareCardBudget: ShareCardStorageBudget = configuration.shareCardStorageBudget(ShareCardStorageBudget(ShareCardWiring.DefaultBudgetMb * 1024 * 1024))

  /** Posters download directly, except those of a site that blocks the worker's IP, which go through the
   *  egress its scrapes use: Multikino 403s the worker's IP on every poster as on its pages (the proxy), biletyna.pl
   *  refuses it (the proxy), Kino Kryterium's portal times it out (Zyte). Those routes are PAID, so a poster that fails on
   *  one is remembered rather than asked for again by every render and every daily backfill. */
  private lazy val posterDownload: PosterDownload = PosterDownload.routed(new HttpPosterDownload(tls = tlsContext), posterEgressRoutes)

  /** The poster hosts that block the worker's IP, each by the egress its scrapes use — for every poster download: every
   *  host the scraper catalog reads through an egress ([[services.cinemas.CinemaScraperCatalog.egressByHost]]), and
   *  Multikino's on a poster route of its own (its own breaker, off the scrapes' /uptime row). */
  lazy val posterEgressRoutes: Map[String, PosterDownload] =
    cinemaScraperCatalog.egressByHost.map { case (host, egress) => host -> remembered(egress) } +
      (java.net.URI.create(services.cinemas.pl.MultikinoClient.HomeUrl).getHost -> remembered(multikinoPosterFetch))

  /** A PAID poster route, a failure on it remembered rather than asked again by every render and backfill. */
  private def remembered(egress: tools.HttpFetch): PosterDownload =
    new RememberedFailurePosterDownload(new EgressPosterDownload(egress), shareCardStore.failedPosters, clock)

  /** Shrinks each poster to the card's slot — through the process's one gate, shared with every
   *  other country's renders. */
  lazy val posterShrinker: VipsPosterShrinker =
    new VipsPosterShrinker(
      binary      = VipsPosterShrinker.locate(configuration.executableSearchPath),
      memoryCap   = configuration.posterDecodeMemoryCap(PosterDecodeMemoryCap(PosterPipeline.DefaultDecodeMemoryCapMb)),
      gate        = posterShrinkGate)

  lazy val shareCardService: ShareCardService = new ShareCardService(
    country, shareCardStore,
    new ShareCardPosters(shareCardStore, posterDownload, posterShrinker, shareCardMetrics, stopwatch),
    taskQueue, shareCardRescrapes, shareCardMetrics, clock)

  /** The fleet database — one for every country's worker, on the same cluster (and, in a
   *  multi-country process, the same client) as the country's own. Optional: unreachable or
   *  refused, it is only the Facebook re-scrapes that stop. */
  lazy val fleetMongoConnection: MongoConnection =
    MongoConnection.forDatabase(mongoAddress.uri, services.FleetDatabase.Name, MongoRequirement.Optional,
      MongoTuning.from(configuration), sharedMongoClient)

  private lazy val facebookGraph: Option[FacebookGraph] = FacebookGraph.fromConfiguration(configuration, tlsContext)

  /** The fleet's re-scrape queue, when there is anything to send with and anywhere to queue. */
  private lazy val facebookRescrapeStore: Option[FacebookRescrapeStore] =
    facebookGraph.flatMap(_ => fleetMongoConnection.database).map(db =>
      new MongoFacebookRescrapeStore(db.getCollection(services.facebook.MongoFacebookSlots.Collection)))

  /** The film pages Facebook has fetched, as the web records them: the only pages worth a
   *  re-scrape. Present whenever the queue is — the same database. */
  private lazy val facebookFetches: Option[services.facebook.MongoFacebookFetches] =
    facebookRescrapeStore.flatMap(_ => fleetMongoConnection.database).map { db =>
      val fetches = new services.facebook.MongoFacebookFetches(db.getCollection(services.facebook.FacebookFetches.Collection))
      fetches.ensureExpiry(workerMetrics.ttlIndexMismatches)
      fetches
    }

  lazy val shareCardRescrapes: ShareCardRescrapes = facebookRescrapeStore match {
    case Some(store) => new FacebookRescrapeQueue(store, country.code)
    case None =>
      if (facebookGraph.isDefined) logger.error(s"share card: Facebook re-scrapes are OFF for ${country.code} — " +
        s"the fleet database ${services.FleetDatabase.Name.value} could not be reached or used")
      ShareCardRescrapes.disabled(shareCardMetrics)
  }

  /** This country's side of draining the fleet's re-scrape queue, on its own thread. */
  lazy val facebookRescrapeDrain: Option[FacebookRescrapeDrain] = managedResources.stoppingEach(
    for { store <- facebookRescrapeStore; graph <- facebookGraph if shareCardsEnabled; fetches <- facebookFetches }
    yield new FacebookRescrapeDrain(store, graph, new FilmPageUrls(readModelRepository, country, clock), fetches, country.code, shareCardMetrics, clock, stopwatch))

  def startFacebookRescrapes(): Unit = facebookRescrapeDrain.foreach(_.start())

  /** Closes the fleet database — only when this wiring opened it; the drain itself is a managed resource. */
  def closeFleetConnection(): Unit = facebookRescrapeStore.foreach(_ => fleetMongoConnection.close())

  /** What the projection asks about share cards — and tells the backfill which films are on screen. */
  lazy val shareCardLedger: ShareCardLedger =
    if (shareCardsEnabled) new BackfilledShareCardLedger(shareCardService, shareCardBackfill) else ShareCardLedger.none

  lazy val shareCardJanitor: ShareCardJanitor = new ShareCardJanitor(
    shareCardStore, readModelRepository, shareCardBudget, shareCardMetrics, clock,
    refresh = readModelProjector.refreshShareCard, cities = country.cities.iterator.map(_.slug).toSet)

  /** The country's city cards, each re-rendered weekly at its own phase. */
  lazy val cityCardService: CityCardService =
    new CityCardService(country, shareCardStore, readModelRepository, taskQueue, shareCardMetrics, clock)

  lazy val shareCardBackfill: ShareCardBackfill =
    new ShareCardBackfill(shareCardService, readModelRepository, taskQueue, shareCardMetrics, clock,
      maxBacklog = configuration.shareCardBackfillMaxBacklog(ShareCardBackfillMaxBacklog(ShareCardBackfill.DefaultMaxBacklog)))

  lazy val shareCardFollowUp: ShareCardFollowUp =
    new ShareCardFollowUp(shareCardStore, shareCardService.superseded, readModelProjector.refreshShareCard, readModelProjector.releaseShareCardHold)

  lazy val shareCardHandlers: Seq[TaskHandler] =
    if (!shareCardsEnabled) Nil
    else Seq(
      new RenderShareCardHandler(shareCardService),
      new PruneShareCardsHandler(shareCardJanitor),
      new ReleaseShareCardHoldHandler(() => readModelProjector.releaseExpiredHolds()),
      new RenderCityCardHandler(cityCardService))

  /** The recurring enqueues: the budget pass every ten minutes (first four after boot), the full
   *  prune daily at 03:00 UTC (or five minutes after a boot that finds that day's prune never ran). Each window is claimed, so one replica enqueues it. */
  lazy val shareCardReapers: Seq[ClaimedEnqueueReaper] = managedResources.stoppingEach(
    if (!shareCardsEnabled) Nil
    else {
      def enqueue(taskType: TaskType, key: String, payload: Map[String, String] = Map.empty): () => Unit =
        () => { taskQueue.enqueue(taskType, key, payload, submittedAt = clock.instant()); () }
      Seq(
        new ClaimedEnqueueReaper("share-card-budget",
          enqueue(TaskType.PruneShareCards, "share-card-budget", Map(PruneShareCardsHandler.ModeKey -> PruneShareCardsHandler.Budget)),
          10.minutes, 4.minutes, scheduledRunStore, clock),
        new ClaimedEnqueueReaper("share-card-prune",
          enqueue(TaskType.PruneShareCards, "share-card-prune", Map(PruneShareCardsHandler.ModeKey -> PruneShareCardsHandler.Daily)),
          24.hours, 5.minutes, scheduledRunStore, clock, ClaimedEnqueueReaper.Timing.Aligned(ShareCardWiring.DailyPruneAt)),
        // The city cards' weekly spread, ticked every minute like the scrape reaper.
        new ClaimedEnqueueReaper("city-card", () => { cityCardService.enqueueDue(); () }, 1.minute, 3.minutes, scheduledRunStore, clock))
    })
}

object ShareCardWiring {
  /** When the daily share-card prune runs, past midnight UTC: a fixed time, so its burst of
   *  deletions lands at one place on the dashboard however the day's deploys fall. */
  val DailyPruneAt: FiniteDuration = 3.hours

  /** Each country's share-card budget, in MB (`KINOWO_SHARE_CARD_BUDGET_MB`): 6 GiB, raised from 3 when
   *  Germany's new venues took its current cards past 2.2 GB in a night (ShareCardBudgetSpec). Five
   *  countries at it (30 GiB, plus the floor below) are under a quarter of k3s-worker-1's root; the
   *  free-space floor, not this, guards the node. A `KINOWO_SHARE_CARD_BUDGET_MB` in the gitops
   *  worker base overrides this default for every country. */
  val DefaultBudgetMb: Long = 6144

  /** The free space the cards leave on their volume, in MB (`KINOWO_SHARE_CARD_MIN_FREE_MB`): 4 GiB,
   *  under FilesystemSpaceCritical's 7% of k3s-worker-1's 160 GB root, so the node's own alerts fire
   *  first and this floor only ever stops the cards being what fills it. */
  val DefaultMinimumFreeMb: Long = 4096
}
