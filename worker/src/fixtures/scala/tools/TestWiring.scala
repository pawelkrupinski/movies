package tools

import clients.TmdbClient
import models.Cinema
import modules.WorkerWiring
import services.{Drainable, MongoConnection}
import services.events.{DomainEvent, EventBus}
import services.freshness.{FreshnessStore, InMemoryFreshnessStore}
import services.resolution.{ResolutionCache, UnresolvedPolicy}
import services.tasks.{ChunkScrapeStore, EnrichDetailsHandler, InMemoryChunkScrapeStore, InMemoryTaskQueue, TaskQueue, TaskType, TaskWorker}

import scala.concurrent.duration._

/** Test seam over the worker's [[WorkerWiring]] composition root: pins a
 *  disabled Mongo, a stub TMDB key, and the full city catalogue so fixture
 *  replay and the coverage spec see every cinema. The serving-side seams
 *  (controllerComponents, materializer, environmentMode) are gone — the worker
 *  is a plain `def main` app, not Play, so they no longer exist to override. */
trait TestWiring extends WorkerWiring {
  /** Pinned: a no-match `TmdbAttempt` carries the time it was stamped, and the
   *  determinism specs compare whole records across arrival orders. */
  override lazy val clock: java.time.Clock = java.time.Clock.fixed(TestWiring.FixedInstant, java.time.ZoneOffset.UTC)

  // Scrape every city in tests, independent of any KINOWO_SCRAPE_CITIES the
  // local/CI env might set, so the recorded fixtures and the coverage spec
  // always see the full catalogue. (Production already defaults to every city;
  // this pin just makes the test set immune to a narrowing override.)
  override def scrapeCities: Set[String] = ScrapeCities.allCities

  // Pin a DISABLED Mongo connection. Tests get their movie data from
  // `InMemoryMovieRepository` / fixtures and don't exercise the user repos, so a real
  // Mongo is never needed — but the production `fromEnv` would still CONNECT to
  // whatever `MONGODB_URI` (`.env.local`) is reachable. With a developer's
  // `flyctl proxy 27017` tunnel up, that hydrated real PRODUCTION enrichment
  // into otherwise-hermetic end-to-end specs, so a row's ratings differed
  // depending on whether the tunnel happened to be open — a flake that never
  // reproduced on CI (no tunnel there). Disabling it here makes every test
  // wiring deterministic regardless of the local environment.
  override lazy val mongoConnection: MongoConnection =
    new MongoConnection(uri = None, dbName = settings.MongoDatabaseName("kinowo"), required = services.MongoRequirement.Optional)

  // NO PAID EGRESS, whatever environment the wiring was handed. A test wiring answers from
  // fixtures; an integration or e2e spec hands it the process's Env for its MONGODB_URI and
  // TMDB key, and that Env carries CI's secrets or a developer's `.env.local` — which is how a
  // replay could reach Zyte (billed per request) or the Decodo proxy without anyone asking.
  // No key means no Zyte leg on any route and no Odeon token; no shards means no proxy leg —
  // so every cinema-egress route collapses onto the wiring's own `httpFetch`.
  override protected def zyteApiKey: Option[settings.ZyteApiKey] = None
  override protected def residentialProxyShards: Option[IndexedSeq[HttpFetch]] = None

  // Run the adaptive-timeout scrape inline on the calling thread, so the
  // order-determinism specs see no scrape thread to race. Fixture scrapes are
  // instant, so the timeout never fires here regardless — this just removes the
  // virtual thread production uses to make a real timeout interruptible.
  override protected lazy val adaptiveTimeoutExecutor: java.util.concurrent.ExecutorService =
    DaemonExecutors.directExecutor()

  // In-memory chunked-scrape coordination/store: Mongo is disabled in tests, and
  // the deterministic harness drives plan→chunk→reduce through this (and the
  // task-path specs need real coordination, not Mongo no-ops).
  override lazy val chunkScrapeStore: ChunkScrapeStore = new InMemoryChunkScrapeStore()

  // Passthrough resolution caches: the fixture harness proves the pipeline is a
  // pure function of the corpus, and a shared stateful cache (whose value for a
  // hint key is fixed by whichever row populates it first) would make a shuffled
  // re-enrich sweep order-dependent — exactly what `ScrapeOrderDeterminismSpec`
  // guards against. The caches' own behaviour is covered by their unit specs.
  override protected def resolutionCache(collection: String, unresolved: UnresolvedPolicy): ResolutionCache = ResolutionCache.passthrough

  // In-memory task queue + freshness store so the queue-driven wiring boots
  // without Mongo: the reapers and the detail enqueuers (when deferred detail is
  // on) write here harmlessly. The harness never runs the TaskWorker — it drives
  // enrichment synchronously (see `enrichRatingsSync` / `converge`) — so these
  // stay drained. METERED, as production's is, and counting re-asks: a fixpoint pass
  // (`FixpointPass`) sees a re-dispatch through `kinowo_worker_tasks_enqueued` and a
  // re-fetch of something already fresh through `reaskCountingQueue` — an unmetered queue
  // would make every enqueue invisible to it. A wiring with a real queue swaps only the
  // store underneath (`queueStore`), so it keeps both counts.
  protected def queueStore: TaskQueue = new InMemoryTaskQueue
  lazy val reaskCountingQueue = new ReaskCountingTaskQueue(queueStore, freshnessStore, {
    case TaskType.EnrichDetails                                    => Some(detailDueWindow)
    case rating if ReaskCountingTaskQueue.StampedTypes(rating)     => Some(ratingDueWindow)
    case _                                                         => None
  }, now = () => clock.instant())
  override lazy val taskQueue: TaskQueue = new services.metrics.MeteredTaskQueue(reaskCountingQueue, taskMetrics)
  override lazy val freshnessStore: FreshnessStore = new InMemoryFreshnessStore

  // The harness's detail handler announces its page reads (`VenueDetailRead`) to THIS buffer rather
  // than straight to the bus, and each detail pass flushes it only after EVERY detail in the pass has
  // merged, so no re-ask races the pass's other merges. Production publishes inline (no buffer).
  private val detailEventBuffer = scala.collection.mutable.ListBuffer.empty[DomainEvent]
  private val detailCaptureBus: EventBus = new EventBus {
    def subscribe(handler: PartialFunction[DomainEvent, Unit]): Unit = ()
    def publish(event: DomainEvent): Unit = { detailEventBuffer += event; () }
  }
  override lazy val enrichDetailsHandler = new EnrichDetailsHandler(
    detailEnrichers.map(de => de.detailGroup -> de).toMap, movieCache,
    freshnessStore, uptimeMonitor, detailCaptureBus,
    detailDueWindow, screeningTokens = screeningTokens, pages = venuePageStore
  , clock = clock, enrichmentLanguage = country.language)
  // The fixture pipeline drives ONE `detailReaper.tick()` per pass and expects it
  // to enqueue the whole deferred-detail corpus (the prod per-tick cap would
  // truncate the snapshot). The cap is a prod burst-shedding lever, not a
  // correctness gate, so the fixture runs it uncapped.
  override def maxDetailEnqueuePerTick: settings.DetailMaxEnqueuePerTick = settings.DetailMaxEnqueuePerTick(Int.MaxValue)

  // No Filmweb fallback in tests: pin the id map empty so fixture replay never
  // resolves (one GET per Filmweb city) or fetches Filmweb live. Eligible scrapers
  // are still wrapped in SourceFallbackScraper, but with no id the fallback
  // short-circuits to the primary's real outcome — identical to pre-fallback
  // behaviour, so fixture snapshots are unaffected.
  override lazy val filmwebFallbackIds: Map[Cinema, Int] = Map.empty
  // Nor Filmweb's venue programmes, for the same reason: the agreement stage reads none (what corrects a model
  // take by them, `agreement.Correction`, is the whole-corpus dump's to measure).
  override lazy val filmwebProgrammes: Option[services.cinemas.pl.FilmwebProgrammes] = None

  // Inject a stub TMDB API key so the test doesn't depend on a `TMDB_API_KEY`
  // env var. `TmdbClient.search` short-circuits to `None` when the key is
  // absent — without an override, every CI runner (and any local box without
  // `.env.local`) sees no TMDB resolution and no downstream enrichment at all.
  // The fixture replay doesn't need a real key (the URL's
  // `api_key` query parameter is stripped from the fixture fingerprint via
  // `RecordingHttpFetch.stableQueryFingerprint`), so any non-empty string works.
  override def tmdbClientOver(http: tools.HttpFetch): TmdbClient =
    new TmdbClient(http, apiKey = Some(settings.TmdbApiKey("test-api-key")), bodies = tmdbJsonBodies)

  // Don't retry cinema scrapes in fixture replay: a missing fixture is permanent,
  // so backoff per fixture-less cinema just multiplies fixture-server boot time
  // (FixtureServerMain scrapes the whole 40+-city catalogue; the retry churn was
  // pushing boot past CI's 300s port-file ceiling → iOS/Android LocalServer
  // "never wrote a port file"). The ceiling clamps EVERY cinema's own
  // `maxFetchAttempts` down to a single no-retry attempt.
  override def scrapeAttemptCeiling: Int = 1

  /** Synchronously force one title all the way through the enrichment cascade:
   *  IMDb id recovery → the four `*Ratings.refreshOneSync` URL
   *  discovery + rating scrapes. Idempotent — safe to call after the bus-driven
   *  path; already-resolved rows re-hit the same URLs (so `RecordingHttpFetch`
   *  overwrites each fixture with byte-identical content). Test/tooling-only:
   *  the fixture recorder uses it as a belt-and-braces pass so no row is left
   *  half-enriched by an async retry that outlived the drain. */
  def fullySyncOne(title: String, year: Option[Int]): Unit = {
    for {
      row <- movieService.get(title, year)
      _   <- row.tmdbId if row.imdbId.isEmpty
    } imdbIdResolver.resolveSync(title, year, row.originalTitle.getOrElse(title))
    imdbRatings.refreshOneSync(title, year)
    rottenTomatoesRatings.refreshOneSync(title, year)
    metascoreRatings.refreshOneSync(title, year)
    // `auditOneSync`, not `refreshOneSync`: Filmweb resolves by a fuzzy
    // title/director SEARCH, so the bus-driven async pass — which runs against a
    // partially-merged row — can land (or miss) a different URL run-to-run.
    // `refreshOneSync` would then take the cheap rating-only path and PRESERVE
    // that order-dependent URL; the audit instead RE-resolves against the now
    // settled row and overwrites/drops it, so Filmweb is a pure function of the
    // final row like every IMDb-id-keyed source already is.
    filmwebRatings.auditOneSync(title, year)
  }

  /** The web's read seam over whatever read model this harness wired. Lives here
   *  rather than on one harness because every end-to-end shape — fixture replay
   *  and archive replay alike — has to be able to render through the SAME seam
   *  the web app serves from, not through the raw worker cache. */
  lazy val webReadModel = new services.readmodel.WebReadModel(readModelRepository, clock = clock)

  /** Every venue whose scrape THREW inside [[cutoverTick]], as `venue: exception`,
   *  over the wiring's life. The tick carries on past a throwing venue, as production's
   *  scheduler does, so a venue that no longer lands at all reads exactly like one that
   *  landed and changed nothing. A harness whose corpus should land whole (the convergence
   *  legs) requires it empty. */
  val scrapeFailures = new java.util.concurrent.ConcurrentLinkedQueue[String]()

  /** The venues that threw in the most recent tick, and how many venues' OUTCOME has
   *  flipped between consecutive ticks (landed → threw or threw → landed). A venue with no
   *  recorded fixture throws on every tick, identically — that is the corpus, not churn —
   *  so `FixpointPass.ledger` counts the flips, never the repeats. */
  @volatile private var lastTickThrew: Option[Set[String]] = None
  @volatile private var lastTickFlips: Set[String] = Set.empty
  val scrapeOutcomeFlips = new java.util.concurrent.atomic.AtomicLong()
  def scrapeOutcomeFlipped: Set[String] = lastTickFlips

  /** A country's boot: every venue scraped into the listing intake through the production runner,
   *  then one identity projection and the enrichment it announces. */
  def bootCutover(): services.identity.ProjectionTick = cutoverTick()

  /** One production tick of a CUT-OVER country — the boot is its first: every venue scraped into the
   *  listing intake through the production runner, then one identity projection and its enrichment. */
  def cutoverTick(): services.identity.ProjectionTick = {
    val threw = java.util.concurrent.ConcurrentHashMap.newKeySet[String]()
    landCutover(cinemaScrapers) { failure =>
      scrapeFailures.add(failure); threw.add(failure.takeWhile(_ != ':')); ()
    }
    val now = { import scala.jdk.CollectionConverters._; threw.asScala.toSet }
    lastTickFlips = lastTickThrew.fold(Set.empty[String])(before => (before diff now) ++ (now diff before))
    scrapeOutcomeFlips.addAndGet(lastTickFlips.size.toLong)
    lastTickThrew = Some(now)
    projectIdentity()
  }

  /** Land each of `scrapers`' listings through the production runner into a CUT-OVER country's
   *  intake, `CutoverLandingThreads` venues at a time, as production's scrape pool lands them side by
   *  side; a venue whose scrape throws is handed to `failed` (`"<venue>: <exception>"`). A venue's
   *  landing reads and writes only that venue's intake state, and the projection after it reads the
   *  whole set, so the order they land in decides nothing — serially, a US walk was 4,462 venues of
   *  round-trips one after another, ~47 s of every tick. Submitted in `scrapers`' order. */
  def landCutover(scrapers: Seq[services.cinemas.common.CinemaScraper])(failed: String => Unit): Unit =
    BoundedParallel.foreach(s"cutover-landing-${country.code}", scrapers, TestWiring.CutoverLandingThreads) { scraper =>
      try { cinemaScrapeRunner.run(scraper); () }
      catch { case e: Exception => failed(s"${scraper.cinema.displayName}: $e") }
    }

  /** One identity projection of a cut-over country (of the corpus `whole`, or of what moved), then the enrichment it kicked (venue detail pages,
   *  IMDb-id recovery, ratings) worked to quiescence, as the detail reaper and the TaskWorker would. */
  def projectIdentity(whole: Boolean = false): services.identity.ProjectionTick = {
    val projection = identityProjection
    // Each stage timed: a cut-over replay is this one call, and its log said only
    // how long the whole of it took, not whether the projection or the enrichment after it spent it.
    val scope = country.code
    val tick  = PhaseTimer.timed(scope, "  identityTick")(projection.tick(whole))
    enrichAfterProjection(scope)
    // Production projects again whatever that enrichment moved — an IMDb year completing a yearless film's key — as
    // the cache tells its trigger (`MovieCache.onChanged`). Here it is done at once, never on a timer beside this one.
    Iterator.continually { val moved = movedByEnrichment.getAndSet(false); moved }.take(TestWiring.EnrichmentReprojections)
      .takeWhile(identity).foreach { _ =>
        PhaseTimer.timed(scope, "  identityTick")(projection.tick())
        enrichAfterProjection(scope)
      }
    tick
  }

  private def enrichAfterProjection(scope: String): Unit = {
    movedByEnrichment.set(false)
    // The venue pages of the films it wrote, enriched as a cut-over worker's detail reaper does: the
    // model reads them from the slots and re-asks each page the enrichment announces, so the next
    // projection decides with them.
    PhaseTimer.timed(scope, "  identityDetails")(enrichDetailsUntilQuiet())
    PhaseTimer.timed(scope, "  identityDrainServices")(drainServices())
    PhaseTimer.timed(scope, "  identityRatings")(enrichRatingsSync())
  }

  /** Whether a write other than the projection's moved a stored film since the last projection's enrichment began. */
  private lazy val movedByEnrichment = {
    val moved = new java.util.concurrent.atomic.AtomicBoolean(false)
    movieCache.onChanged(_ => moved.set(true))
    moved
  }

  // The harness projects by hand (`projectIdentity`): the trigger production runs on a timer never runs here.
  override protected lazy val identityProjectionTriggerScheduler: java.util.concurrent.ScheduledExecutorService =
    new tools.ManualScheduler(new tools.MutableClock(TestWiring.FixedInstant))

  /** The enrichment reaper's per-tick cap is a burst-shedding lever in production;
   *  a harness that drives ONE sweep to quiescence wants the whole corpus offered,
   *  exactly as `maxDetailEnqueuePerTick` already does for detail. */
  override def maxEnrichmentEnqueuePerTick: settings.EnrichmentMaxEnqueuePerTick = settings.EnrichmentMaxEnqueuePerTick(Int.MaxValue)

  /**
   * Refresh every film's ratings by DRIVING PRODUCTION'S OWN PATH: the enrichment
   * reaper enqueues, the real `RatingHandler`s work the queue.
   *
   * This used to walk the corpus itself and call each refresher directly, and every
   * rule it restated along the way drifted from the one production actually uses:
   *
   *  - eligibility. It gated all four sources on `tmdbId`, while `RatingSources` makes
   *    IMDb eligible on an `imdbId` alone and Filmweb on `tmdbId OR filmwebUrl` —
   *    the latter deliberately, "because it can RESOLVE its tmdbId via
   *    Filmweb→Wikidata". The tmdbId-less row that route exists for was the one row
   *    the sweep never walked, and 21 of the 25 films production identifies and the
   *    replay does not carry a Filmweb slot.
   *  - the country gate. Filmweb ran for Germany and the UK, which production holds no
   *    Filmweb data for at all, inventing 972 and 1293 ratings.
   *  - failure isolation. One `try` wrapped all four, so the first to throw skipped
   *    every source after it and coverage depended on a source's POSITION in a list.
   *
   * None of those can be got wrong here, because none of them is stated here. The
   * reaper owns eligibility (through `RatingEnqueuer` → `RatingSources`), the handler
   * list owns the country gate (Filmweb is only in `ratingHandlers` where
   * `filmwebEnabled`), and one task per source owns the isolation. The harness supplies
   * only what production gets from its clock: repetition until quiescent.
   */
  def enrichRatingsSync(): Unit = try {
    UntilQuiet(s"[${country.code}] the rating phase", UntilQuiet.MaxRatingRounds) { round =>
      // `enrichmentReaper.tick` is `private[tasks]`, so the harness supplies the walk
      // the reaper would have done and hands each row to the SAME enqueuer the reaper
      // uses. Eligibility, the due window and the dedup keys all stay where production
      // keeps them — this only decides WHEN, which is the one thing a harness with no
      // clock has to.
      movieCache.snapshot()
        .sortBy(row => (row.title, row.year.map(_.toString).getOrElse("")))
        .foreach(row => movieService.enqueueRatingsFor(row.title, row.year))
      val done = drainRatingQueueOnce()
      if (done > 0) println(s"[${country.code}] rating round $round: $done task(s)")
      done
    }
    ()
  } catch {
    // WHICH tasks keep coming back, and what their handler made of them: "297 unit(s) of work" alone
    // could not say whether the enqueuer or the handler was wrong about a film being due.
    case e: IllegalStateException =>
      val round   = { import scala.jdk.CollectionConverters._; lastRatingRound.asScala.toSeq }
      val tallied = round.groupMapReduce { case (task, outcome) => s"${task.taskType} → $outcome" }(_ => 1)(_ + _)
        .toSeq.sortBy(-_._2).map { case (kind, n) => s"  $n × $kind" }
      val sample  = round.take(10).map { case (task, outcome) => s"  ${task.taskType} ${task.dedupKey} → $outcome" }
      throw new IllegalStateException((Seq(e.getMessage, "the last round's tasks, by handler outcome:") ++ tallied ++
        Seq("e.g.") ++ sample).mkString("\n"), e)
  }

  /** The last rating round's tasks and what their handler returned (or threw), for the report a
   *  rating phase that never goes quiet fails with. */
  private val lastRatingRound = new java.util.concurrent.ConcurrentLinkedQueue[(services.tasks.Task, String)]()

  private lazy val ratingHandlerByType = ratingHandlers.map(h => h.taskType -> h).toMap

  /** How many claimants the queue drains run — production's own pool size, read off
   *  the SAME `backgroundBudget` that caps every other background consumer.
   *
   *  Derived rather than declared, so it cannot drift from the lever callers already
   *  use. The convergence suite's order-independence passes and the determinism specs
   *  swap in a `SameThreadExecutionBudget` to leave their seeded shuffle as the only
   *  nondeterminism; that budget reports 1, so those drains stay strictly serial with
   *  nothing extra to remember. Everything else gets the real budget's cap. An unbounded budget (`<= 0`) means "no cap", which is not a
   *  usable claimant count, so the pool default stands in. */
  private[tools] def drainClaimants: Int =
    backgroundBudget.maxConcurrent match {
      case bounded if bounded > 0 => bounded
      case _                      => TaskWorker.DefaultPoolSize
    }

  /**
   * Work every claimable rating task through the REAL handlers — the synchronous
   * stand-in for the prod `TaskWorker`. A non-rating task is completed and dropped so
   * it cannot spin this drain.
   *
   * A POOL, exactly as `TaskWorker` runs one. This claimed a single task at a time for
   * its whole life, which made the harness strictly slower than the thing it stands in
   * for, and it dominated the convergence legs: Poland's `enrichRatings` phase was
   * 1,615s of a 2,201s boot, while 85% of its enrichment calls were free fixture
   * replays — so nearly all of it was a serial tail of network round-trips production
   * would have overlapped four ways.
   *
   * Concurrency is safe here for the same reason it is safe in production: `claim` is
   * atomic per task, so N claimants partition the queue rather than racing for a row,
   * and the handlers are the same ones four prod threads already run side by side.
   *
   * A claimant that finds the queue momentarily empty simply stops; the round loop in
   * [[enrichRatingsSync]] re-offers the corpus and drains again, so work another
   * claimant enqueued mid-round is picked up on the next one rather than lost.
   */
  private def drainRatingQueueOnce(): Int = {
    lastRatingRound.clear()
    val handled   = new java.util.concurrent.atomic.AtomicInteger(0)
    val claimants = (0 until drainClaimants).map { i =>
      val workerId = s"rating-sync-$i"
      val thread   = new Thread(
        () =>
          Iterator.continually(taskQueue.claim(workerId, 5.minutes))
            .takeWhile(_.isDefined).flatten
            .foreach { task =>
              ratingHandlerByType.get(task.taskType).foreach { h =>
                try { lastRatingRound.add(task -> h.handle(task).toString); handled.incrementAndGet() }
                catch { case e: Exception => lastRatingRound.add(task -> s"threw ${e.getClass.getSimpleName}: ${e.getMessage}"); () }
              }
              taskQueue.complete(task.id, workerId)
            },
        workerId)
      thread.start()
      thread
    }
    claimants.foreach(_.join())
    handled.get()
  }

  /** One detail pass — the reaper's tick (capped at its `maxEnqueuePerTick`) and the tasks it
   *  enqueued worked — answering how many it enqueued. */
  private def enrichDetailsOnce(): Int = {
    val enqueued = detailReaper.tick()
    val workerId = "detail-sync"
    // A handler that asks for its task again (`Reschedule`: a page venue_pages did not take) has it queued for the
    // next pass, as production's TaskWorker returns it to waiting — never worked again within this pass.
    val again = Iterator.continually(taskQueue.claim(workerId, 5.minutes))
      .takeWhile(_.isDefined).flatten
      .flatMap { task =>
        // The page reads a cut-over model asked for its waiting listings run beside the film rows'.
        val outcome =
          if (task.taskType == TaskType.EnrichDetails) scala.util.Try(enrichDetailsHandler.handle(task)).toOption
          else if (task.taskType == TaskType.ReadVenuePage) scala.util.Try(readVenuePageHandler.handle(task)).toOption
          else None
        taskQueue.complete(task.id, workerId)
        outcome.collect { case _: services.tasks.HandlerOutcome.Reschedule => task }
      }.toList
    again.foreach(task => taskQueue.enqueue(task.taskType, task.dedupKey, task.payload, submittedAt = clock.instant()))
    // Every detail has merged; now announce the pages read, against a fully-settled cache.
    val ready = detailEventBuffer.toList
    detailEventBuffer.clear()
    ready.foreach(eventBus.publish)
    enqueued + again.size
  }

  /** Detail passes until they stop making progress — what production's reaper reaches over its
   *  ticks, as each is capped. A page whose fetch FAILED is left unstamped and due again (production
   *  retries it every tick), so the passes settle on those, not on zero: the last pass is the one
   *  that enqueued no fewer than the one before it. Bounded like the rating phase. */
  def enrichDetailsUntilQuiet(): Unit = {
    var previous = Int.MaxValue
    UntilQuiet(s"[${country.code}] the detail phase", UntilQuiet.MaxRatingRounds) { _ =>
      val enqueued = enrichDetailsOnce()
      val progress = if (enqueued < previous) enqueued else 0
      previous = enqueued
      progress
    }
    ()
  }

  def quiesce(drainables: Drainable*): Unit =
    drainables.foreach(_.drain())

  /** Drain the enrichment worker pools so every `ImdbIdMissing` published during
   *  the scrape (and the id write-backs it drives) is processed end to end, in the
   *  wiring's `cascadeDrainOrder` (producer→consumer). Production shutdown no longer
   *  reads it: `managedResources` stops every service newest first.
   *
   *  DRAIN, not `stop()`. This is called once per projection — and `stop()` shuts
   *  the executors down for good. So the very first call ended the id-recovery
   *  pool, and every `ImdbIdMissing` published afterwards (`announceResolvedNewMovie`,
   *  which is where a `tmdbNoMatch` newcomer asks for an id) was submitted to a
   *  dead pool and dropped. Poland's
   *  leg logged zero event-driven recoveries as a result, and the bare-title long
   *  tail prod identifies through IMDb — "Stop Making Sense", "Złoto", "La La
   *  Land", 42 films in all — came out unresolved.
 */
  def drainServices(): Unit =
    quiesce(cascadeDrainOrder*)
}

object TestWiring {
  /** The instant every harness clock starts at. */
  val FixedInstant: java.time.Instant = java.time.Instant.parse("2026-06-08T12:00:00Z")

  /** The most projections one `projectIdentity` runs again after its enrichment moved stored films: an enrichment of what
   *  a projection wrote moves less each round, and settles in one or two. */
  val EnrichmentReprojections = 4

  /** How many venues a cut-over scrape walk lands at once ([[TestWiring.landCutover]]): each landing
   *  waits on a few Mongo round-trips, so the walk is bound by how many are in flight, not by a
   *  runner's four cores. */
  val CutoverLandingThreads = 8
}
