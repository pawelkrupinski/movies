package modules.webwiring

import controllers.{CorpusListing, DebugController, DebugCountries, DebugSnapshot, DebugStack, DebugStreamController, ReadModelDump, RefreshingSnapshot, FileSnapshotStore, SnapshotStore}
import modules.Wiring
import play.api.Mode
import services.{MongoConnection, UptimeMonitor}
import services.movies.MongoMovieRepository
import services.readmodel.MongoReadModelRepository
import services.tasks.MongoTaskQueue

/** ── /debug ────────────────────────────────────────────────────────────────
 *  The dev-only corpus inspector: one `DebugStack` of read-only views per
 *  country, the boot country's on the serving connection (or its local mirror)
 *  and, in Dev, one per switchable country on a shared client. */
trait DebugWiring { self: Wiring =>

  // Read-only view of the worker-written `rating_cadence` collection for the
  // dev-only /debug/cadence page. Read from the MIRROR alongside `movies`: both
  // this and the attempt log below are read per /debug row-expand, so leaving
  // them on the prod tunnel would keep two ~110ms round-trips on a page whose
  // corpus read is already a LAN hop. They're mirrored collections, so this is
  // the same data, locally.
  lazy val ratingCadenceReader: services.cadence.RatingCadenceReader =
    new services.cadence.MongoRatingCadenceReader(movieMirrorConnection.database)
  // Whether the /debug stacks below are reading a COPY. Gates the navbar's
  // mirror-age badge: with no mirror configured every page reads the source, so
  // there is nothing that could be behind and nothing to render.
  private lazy val readingThroughMirror: Boolean = processConfiguration.mirrorMongoUri.isDefined
  // How far behind that copy is. A sync that stops serves a page which renders,
  // times itself `now`, and is silently hours old — so the pages say their own
  // age (services.MirrorFreshness).
  private def mirrorFreshnessOf(connection: MongoConnection): services.MirrorFreshness =
    if (readingThroughMirror) new services.MongoMirrorFreshness(connection.database)
    else services.MirrorFreshness.notMirrored
  // Read-only view of the worker-written `enrichment_attempts` collection — the
  // last fetch per (source, film) behind the /debug row's expand section.
  lazy val enrichmentAttemptReader: services.attempts.EnrichmentAttemptReader =
    new services.attempts.MongoEnrichmentAttemptReader(movieMirrorConnection.database)

  // ── Dev-only per-country /debug data ─────────────────────────────────────────
  // The /debug pages read ONE country's Mongo db. In prod that's this
  // deployment's country (`bootDebugStack`). Locally in Dev the navbar's country
  // switch stays SAME-ORIGIN (`?country=xx`) and selects a per-country stack here,
  // instead of navigating to the other country's PROD host (which serves prod
  // mode and 404s every /debug route). Each extra country reads its OWN database
  // (`country.mongoDb`, NOT the MONGODB_DB override — that would pin every country
  // to one db) off ONE shared MongoClient, so N countries add N database views,
  // not N connection pools. When the read-mirror is configured those views come
  // from the MIRROR (which holds every country's db, not just the boot one), so
  // `?country=uk` is as fast as the boot country instead of paying the tunnel's
  // ~110ms per round-trip; unset → the MAIN Mongo, as before.
  //
  // The mirror is read UNCONDITIONALLY once configured, so a collection its sync
  // doesn't carry reads as permanently EMPTY — a blank page, no error. POINTING A
  // NEW READER HERE MEANS ADDING ITS COLLECTION TO `services.DebugMirror`, which
  // `MongoConnectionSpec` diffs against the sync's own list.
  //
  // Each stack's whole-collection reads (the corpus listing; for a switched-to
  // country also its read-model dump) are `RefreshingSnapshot`s: re-reading them
  // per request made every country switch 4–10 s (US: 105k slots, 100k read-model
  // screenings, measured off the local mirror). In Dev a ticker keeps them warm —
  // from boot, then every `DebugSnapshotRefresh` — so a switch is a map lookup and
  // what it shows is at most about a minute behind the mirror, an age the navbar
  // badge states. In Dev each snapshot is also kept on disk (`target/`), so a dev
  // reload or a server restart serves the last one at once instead of making the
  // first load of every country wait on a cold read.
  private val DebugSnapshotRefresh = scala.concurrent.duration.DurationInt(60).seconds
  private lazy val debugSnapshotPool = managedResources.executor("debug snapshots")(tools.DaemonExecutors.virtualThreadEC("debug-snapshots"))
  private lazy val debugSnapshotStore: SnapshotStore =
    if (environmentMode == Mode.Dev) new FileSnapshotStore(java.nio.file.Paths.get("target", "debug-snapshots"))
    else SnapshotStore.none
  private def debugSnapshot[A](label: String, freshness: services.MirrorFreshness)(read: => A): RefreshingSnapshot[A] =
    new RefreshingSnapshot(label, () => read, freshness, refreshAfter = scala.concurrent.duration.DurationInt(15).seconds, clock,
      debugSnapshotStore)(using debugSnapshotPool)

  private lazy val bootDebugListing = debugSnapshot(s"/debug listing ${country.code}", mirrorFreshnessOf(movieMirrorConnection))(
    CorpusListing.read(movieRepository))
  private lazy val bootDebugCadence = debugSnapshot(s"/debug/cadence ${country.code}", mirrorFreshnessOf(movieMirrorConnection))(
    ratingCadenceReader.all())
  private lazy val bootDebugStack: DebugStack = new DebugStack(
    country, movieRepository, taskQueue, ratingCadenceReader, enrichmentAttemptReader,
    // The WARM in-memory model the app actually serves from, read per request so
    // `/debug/readmodel` shows exactly what a request would resolve against.
    readModel              = DebugSnapshot.readNow(mirrorFreshnessOf(movieMirrorConnection))(ReadModelDump.of(webReadModel)),
    readModelScreeningsFor = ReadModelDump.screeningsOf(webReadModel),
    mirrorFreshness        = mirrorFreshnessOf(movieMirrorConnection),
    corpusListing          = Some(() => bootDebugListing.get()),
    ratingCadenceSnapshot  = Some(() => bootDebugCadence.get()))
  // One shared client for the extra countries: None in prod, when only one country
  // is deployed, or when MONGODB_URI is unset — then there are no extras and the
  // debug switch stays off. The root's `stop()` closes it.
  protected lazy val debugExtraClient: Option[org.mongodb.scala.MongoClient] =
    if (environmentMode == Mode.Prod || models.Country.switchable.sizeIs <= 1) None
    else processConfiguration.mirrorMongoUri
      .map(mirror => MongoConnection.sharedClientFor(mirror.asMongoUri, Some(MongoConnection.ServerSelectionTimeout(MongoConnection.LocalMirrorTimeout)), mongoTuning.maxPoolSize))
      .orElse(MongoConnection.sharedClientAt(mongoAddress, mongoTuning))
  private lazy val debugExtraStacks: Seq[(models.Country, MongoConnection, DebugStack, Seq[RefreshingSnapshot[?]])] =
    debugExtraClient.toSeq.flatMap { client =>
      models.Country.switchable.filterNot(_ == country).map { country =>
        val conn       = Wiring.debugMirrorConnection(
          processConfiguration.mirrorMongoUri,
          MongoConnection.mirrorForDb(_, country.mongoDb, sharedClient = Some(client)),
          MongoConnection.forDatabase(mongoAddress.uri, settings.MongoDatabaseName(country.mongoDb), required = services.MongoRequirement.Optional, mongoTuning, sharedClient = Some(client)))
        val screenings = new services.movies.MongoScreeningsRepository(conn.database)
        val slots      = new services.movies.MongoSlotsRepository(conn.database)
        val reader     = new MongoReadModelRepository(conn.database)
        // THIS stack's country, not the serving one: /debug reads another
        // country's database, and folding its titles with the serving country's
        // rules would key rows the way no worker ever wrote them.
        val normalizer = services.movies.TitleNormalizer.forCountry(country)
        val repository = new MongoMovieRepository(conn.database,
          screenings = Some(screenings), slots = Some(slots), normalizer = normalizer)
        val freshness  = mirrorFreshnessOf(conn)
        val listing    = debugSnapshot(s"/debug listing ${country.code}", freshness)(CorpusListing.read(repository))
        val cadence    = debugSnapshot(s"/debug/cadence ${country.code}", freshness)(
          new services.cadence.MongoRatingCadenceReader(conn.database).all())
        // No warm model for a switched-to country: its `web_movies` / `web_screenings`
        // straight from Mongo, `now` for the mtime. A partial read THROWS rather than
        // listing as a smaller read model.
        val readModel  = debugSnapshot(s"/debug/readmodel ${country.code}", freshness) {
          val movies = reader.findAllMoviesChecked().required
          ReadModelDump.of(movies, f => {
            if (!reader.foreachScreening(f).isComplete)
              throw new IllegalStateException(s"${country.code} read model read incomplete")
          }, clock.instant())
        }
        val stack = new DebugStack(country, repository,
          new MongoTaskQueue(conn.database),
          new services.cadence.MongoRatingCadenceReader(conn.database),
          new services.attempts.MongoEnrichmentAttemptReader(conn.database),
          readModel              = () => readModel.get(),
          ratingCadenceSnapshot  = Some(() => cadence.get()),
          readModelScreeningsFor = id => reader.findCard(id).map(_.screenings),
          mirrorFreshness        = freshness,
          corpusListing          = Some(() => listing.get()))
        (country, conn, stack, Seq(listing, readModel, cadence))
      }
    }
  lazy val debugCountries: DebugCountries =
    DebugCountries.of(bootDebugStack,
      debugExtraStacks.map { case (country, _, stack, _) => country -> stack }.toMap,
      devMode = environmentMode != Mode.Prod)

  // The warm-up ticker: Dev only — prod 404s every /debug route, and a spec's
  // wiring must not start reading Mongo behind its back. `stop()` shuts it down (`managedResources`), so
  // a dev reload doesn't leave the previous app's ticker reading.
  protected lazy val debugSnapshotTicker: Option[java.util.concurrent.ScheduledExecutorService] =
    Option.when(environmentMode == Mode.Dev) {
      val snapshots = Seq(bootDebugListing, bootDebugCadence) ++ debugExtraStacks.flatMap(_._4)
      val ticker    = managedResources.executor("debug snapshot ticker")(tools.DaemonExecutors.scheduler("debug-snapshot-ticker"))
      // ONE re-read at a time, boot country first: all of them at once (14 reads)
      // contended so hard right after boot that each took 8–60 s instead of ~1 s.
      // A page asking for a cold country doesn't queue behind this — its own read
      // starts immediately.
      ticker.scheduleWithFixedDelay(() => snapshots.foreach(snapshot =>
          scala.concurrent.Await.ready(snapshot.refreshIfOlderThan(DebugSnapshotRefresh), scala.concurrent.duration.DurationInt(2).minutes)),
        0, DebugSnapshotRefresh.toSeconds, java.util.concurrent.TimeUnit.SECONDS)
      ticker
    }

  lazy val debugController  = { debugSnapshotTicker; new DebugController(controllerComponents, debugCountries, webReadModel, adminAction, environmentMode,
    cinemaSourceUrls = () => UptimeMonitor.cinemaUrls(uptimeMonitor.serviceTagsSnapshot()),
    servingCountry = country, clock = clock, normalizer = titleNormalizer) }
  // Dev-only SSE feed for the /debug live view; watches the SELECTED country's
  // `movies` via the same per-country stacks the /debug page
  // renders from. The live row's details cell ships empty (lazily fetched on
  // expand), so no cinema-URL snapshot is needed.
  lazy val debugStreamController = new DebugStreamController(controllerComponents, debugCountries, environmentMode)(using materializer)
}
