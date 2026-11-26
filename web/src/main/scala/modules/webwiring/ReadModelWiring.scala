package modules.webwiring

import controllers.MovieControllerService
import services.MongoConnection
import services.movies.{MovieChangeFeed, MovieRepository}
import services.readmodel.{MongoReadModelRepository, ReadModelReader, ReadModelStreams, ReadModelUpkeep, WebReadModel}

/** ── Denormalised read model ──────────────────────────────────────────────────
 *  The serving app reads from the worker-maintained `web_movies` /
 *  `web_screenings` collections via `WebReadModel`, kept warm by their change
 *  streams (`ReadModelStreams`) and the backstop (`ReadModelUpkeep`). It deliberately does NOT watch `movies` — a showtime edit there
 *  now reaches the web as one small screening-document delta, not a full-record
 *  re-push. `movieRepository` survives only for the on-demand /debug corpus dump
 *  (a one-off `findAll`) and the /debug live view, whose stream opens only while a viewer watches. */
trait ReadModelWiring { self: Wiring =>

  // Local read-mirror: `/debug`'s `movieRepository.findAll()` is a full `movies`
  // scan. Run locally it goes over the prod ssh tunnel, where 1200+ full
  // documents take 30–60s and intermittently hit findAll's 60s timeout (→ an empty
  // /debug table). When `MONGODB_MOVIES_MIRROR_URI` points at a local Mongo
  // kept synced from prod by `scripts/local-mirror/mirror.sh`, movieRepository reads
  // that LAN mirror (~100ms) instead. movieRepository is read-only in this process
  // (the worker owns `movies` writes), and the task queue stays on the prod
  // connection (AdminWiring), so /debug re-enrich still works end-to-end: ↻ → prod
  // worker → prod `movies` → tailer → local mirror → /debug SSE. Unset (prod +
  // default dev) → reuse the shared prod connection, behaviour identical. Set →
  // ALWAYS read that local mirror and never fall back to the prod tunnel: an
  // unreachable mirror just disables movieRepository (an empty /debug) instead
  // of silently dumping the prod corpus over the slow tunnel.
  // Short timeouts on the mirror connection (`MongoMirror.Timeout`): it's a
  // loopback Mongo that answers in ~ms when healthy, so a few seconds of silence
  // means it's down. Capping the boot probe and the driver's per-request
  // server-selection makes a down/unreachable mirror disable fast (→ empty
  // /debug, per `debugMirrorConnection`'s no-fallback rule) instead of wedging boot
  // and every /debug load on the driver's 30s default.
  lazy val movieMirrorConnection: MongoConnection =
    Wiring.debugMirrorConnection(
      processConfiguration.mirrorMongoUri,
      mirror => new MongoConnection(settings.MongoMirror.fromUri(mirror, mongoAddress.databaseFor(country), mongoTuning), services.MongoRequirement.Optional),
      mongoConnection)
  /** The SERVING country's title rules. Passed explicitly so the web tier keys
   *  the same way the worker that wrote the corpus did; the per-country debug
   *  stacks (DebugWiring) each get their OWN, since they read another country's
   *  database. */
  lazy val titleNormalizer: services.titles.TitleNormalizer =
    services.titles.TitleNormalizer.forCountry(country)
  lazy val movieStoreWiring: MovieStoreWiring = new MovieStoreWiring(clock, stopwatch)
  // Showtimes and slots splits: /debug's movieRepository is read-only, so it only needs the read-stitch from `screenings`
  // and `movie_slots` on the same connection it reads `movies` from. The worker owns the backfill and the writes.
  lazy val movieStores: MovieStores = movieStoreWiring.readOnly(movieMirrorConnection.database, titleNormalizer, webDecodeFailureMetrics)
  lazy val screeningsRepository: services.movies.ScreeningsRepository = movieStores.screenings
  lazy val slotsRepository: services.movies.SlotsRepository = movieStores.slots
  lazy val movieRepository: MovieRepository & MovieChangeFeed = movieStores.repository
  /** The /debug corpus table's read of `movies`, its showtimes stripped server-side. */
  lazy val movieListingReader: services.movies.MovieListingReader = movieStores.listingReader
  lazy val readModelRepository: ReadModelReader = new MongoReadModelRepository(mongoConnection.database, decodeFailures = webDecodeFailureMetrics, stopwatch = stopwatch)
  lazy val webReadModel: WebReadModel = new WebReadModel(readModelRepository, clock = clock, servingCountry = Some(country))
  lazy val readModelReloadInterval    = processConfiguration.readModelReloadInterval(ReadModelUpkeep.DefaultReloadInterval)
  lazy val readModelColdRetryInterval = processConfiguration.readModelColdRetryInterval(ReadModelUpkeep.DefaultColdRetryInterval)
  lazy val readModelStreams: ReadModelStreams = new ReadModelStreams(readModelRepository, webReadModel,
    readModelReloadInterval, readModelColdRetryInterval, webReadModelStreamMetrics, clock)
  lazy val readModelUpkeep: ReadModelUpkeep = managedResources.stopping(new ReadModelUpkeep(webReadModel, readModelStreams,
    readModelReloadInterval, readModelColdRetryInterval, ticks = ticks, threadPools = threadPools, sleep = sleep))

  // Read at scrape time, so the model (forced at boot) is never built by the metric.
  lazy val webReadModelStreamMetrics = new services.metrics.web.WebReadModelStreamMetrics(webJvmMetrics.registry, country.code,
    collection => readModelStreams.streamLive(collection))

  // Reads come straight from the read model; enrichment + projection happen in
  // the worker process.
  lazy val movieControllerService = new MovieControllerService(webReadModel, clock)
}
