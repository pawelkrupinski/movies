package modules.webwiring

import controllers.{DebugController, DebugCountries, DebugStack, DebugStreamController}
import modules.Wiring
import play.api.Mode
import services.{MongoConnection, UptimeMonitor}
import services.movies.MongoMovieRepository
import services.readmodel.MongoReadModelRepository
import services.tasks.MongoTaskQueue
import tools.Env

/** ── /debug ────────────────────────────────────────────────────────────────
 *  The dev-only corpus inspector: one `DebugStack` of read-only views per
 *  country, the boot country's on the serving connection (or its local mirror)
 *  and, in Dev, one per switchable country on a shared client. */
trait DebugWiring { self: Wiring =>

  // The /debug "pending enrichment (staging)" table reads + live-watches this.
  lazy val stagingRepository: services.staging.StagingRepository = new services.staging.MongoStagingRepository(mongoConnection.database, titleNormalizer)
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
  private lazy val readingThroughMirror: Boolean = Env.get("MONGODB_MOVIES_MIRROR_URI").isDefined
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
  private lazy val bootDebugStack: DebugStack = new DebugStack(
    models.Country.fromEnv, movieRepository, stagingRepository, taskQueue, ratingCadenceReader, enrichmentAttemptReader,
    readModelMovies       = () => webReadModel.allMovies(),
    readModelScreenings   = () => webReadModel.allScreenings(),
    readModelLastModified = () => webReadModel.lastModified,
    mirrorFreshness       = mirrorFreshnessOf(movieMirrorConnection))
  // One shared client for the extra countries: None in prod, when only one country
  // is deployed, or when MONGODB_URI is unset — then there are no extras and the
  // debug switch stays off. The root's `stop()` closes it.
  protected lazy val debugExtraClient: Option[org.mongodb.scala.MongoClient] =
    if (environmentMode == Mode.Prod || models.Country.switchable.sizeIs <= 1) None
    else Env.get("MONGODB_MOVIES_MIRROR_URI")
      .map(MongoConnection.sharedClientFor(_, Some(MongoConnection.LocalMirrorTimeout)))
      .orElse(MongoConnection.sharedClientFromEnv())
  private lazy val debugExtraStacks: Seq[(models.Country, MongoConnection, DebugStack)] =
    debugExtraClient.toSeq.flatMap { client =>
      models.Country.switchable.filterNot(_ == models.Country.fromEnv).map { country =>
        val conn       = Wiring.debugMirrorConnection(
          Env.get("MONGODB_MOVIES_MIRROR_URI"),
          MongoConnection.mirrorForDb(_, country.mongoDb, sharedClient = Some(client)),
          MongoConnection.fromEnvForDb(country.mongoDb, required = false, sharedClient = Some(client)))
        val screenings = new services.movies.MongoScreeningsRepository(conn.database)
        val slots      = new services.movies.MongoSlotsRepository(conn.database)
        val reader     = new MongoReadModelRepository(conn.database)
        val stack = new DebugStack(country,
          // THIS stack's country, not the serving one: /debug reads another
          // country's database, and folding its titles with the serving country's
          // rules would key rows the way no worker ever wrote them.
          new MongoMovieRepository(conn.database, fallbackToOwnInit = false,
            screenings = Some(screenings), slots = Some(slots),
            normalizer = services.movies.TitleNormalizer.forCountry(country)),
          new services.staging.MongoStagingRepository(conn.database,
            normalizer = services.movies.TitleNormalizer.forCountry(country)),
          new MongoTaskQueue(conn.database),
          new services.cadence.MongoRatingCadenceReader(conn.database),
          new services.attempts.MongoEnrichmentAttemptReader(conn.database),
          readModelMovies       = () => reader.findAllMovies(),
          readModelScreenings   = () => reader.findAllScreenings(),
          readModelLastModified = () => java.time.Instant.now(),
          mirrorFreshness       = mirrorFreshnessOf(conn))
        (country, conn, stack)
      }
    }
  lazy val debugCountries: DebugCountries =
    DebugCountries.of(bootDebugStack,
      debugExtraStacks.map { case (country, _, stack) => country -> stack }.toMap,
      devMode = environmentMode != Mode.Prod)

  lazy val debugController  = new DebugController(controllerComponents, debugCountries, webReadModel, adminAction, environmentMode,
    cinemaSourceUrls = () => UptimeMonitor.cinemaUrls(uptimeMonitor.serviceTagsSnapshot()))
  // Dev-only SSE feed for the /debug live view; watches the SELECTED country's
  // `movies` + `pending_movies` via the same per-country stacks the /debug page
  // renders from. The live row's details cell ships empty (lazily fetched on
  // expand), so no cinema-URL snapshot is needed.
  lazy val debugStreamController = new DebugStreamController(controllerComponents, debugCountries, environmentMode)(using materializer)
}
