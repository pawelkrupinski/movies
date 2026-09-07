package modules.webwiring

import controllers.MovieControllerService
import modules.Wiring
import services.MongoConnection
import services.movies.{MongoMovieRepository, MovieRepository}
import services.readmodel.{MongoReadModelRepository, ReadModelReader, WebReadModel}
import tools.Env

/** ── Denormalised read model ──────────────────────────────────────────────────
 *  The serving app reads from the worker-maintained `web_movies` /
 *  `web_screenings` collections via `WebReadModel`, kept warm by their change
 *  streams. It deliberately does NOT watch `movies` — a showtime edit there
 *  now reaches the web as one small screening-document delta, not a full-record
 *  re-push. `movieRepository` survives only for the on-demand /debug corpus dump
 *  (a one-off `findAll`, no change stream). */
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
  // Short timeouts on the mirror connection (`LocalMirrorTimeout`): it's a
  // loopback Mongo that answers in ~ms when healthy, so a few seconds of silence
  // means it's down. Capping the boot probe and the driver's per-request
  // server-selection makes a down/unreachable mirror disable fast (→ empty
  // /debug, per `debugMirrorConnection`'s no-fallback rule) instead of wedging boot
  // and every /debug load on the driver's 30s default.
  lazy val movieMirrorConnection: MongoConnection =
    Wiring.debugMirrorConnection(
      Env.get("MONGODB_MOVIES_MIRROR_URI"),
      MongoConnection.fromUri(_, required = false,
        probeTimeout           = MongoConnection.LocalMirrorTimeout,
        serverSelectionTimeout = Some(MongoConnection.LocalMirrorTimeout)),
      mongoConnection)
  // Showtimes split: /debug's movieRepository is read-only, so it only needs the
  // read-stitch — re-inject showtimes from `screenings` on the same connection it
  // reads `movies` from. The worker owns the backfill; here we just read.
  lazy val screeningsRepository: services.movies.ScreeningsRepository =
    new services.movies.MongoScreeningsRepository(movieMirrorConnection.database)
  // Same read-stitch seam for the slots split. The worker owns the writes; web only
  // needs the repository so a film whose slots have moved to `movie_slots` still reads
  // complete.
  lazy val slotsRepository: services.movies.SlotsRepository =
    new services.movies.MongoSlotsRepository(movieMirrorConnection.database)
  /** The SERVING country's title rules. Passed explicitly so the web tier keys
   *  the same way the worker that wrote the corpus did; the per-country debug
   *  stacks (DebugWiring) each get their OWN, since they read another country's
   *  database. */
  lazy val titleNormalizer: services.movies.TitleNormalizer =
    services.movies.TitleNormalizer.forCountry(models.Country.fromEnv)

  lazy val movieRepository: MovieRepository = new MongoMovieRepository(
    movieMirrorConnection.database, fallbackToOwnInit = false,
    screenings = Some(screeningsRepository), slots = Some(slotsRepository),
    normalizer = titleNormalizer)
  lazy val readModelRepository: ReadModelReader = new MongoReadModelRepository(mongoConnection.database)
  lazy val webReadModel: WebReadModel = new WebReadModel(readModelRepository)

  // Reads come straight from the read model; enrichment + projection happen in
  // the worker process.
  lazy val movieControllerService = new MovieControllerService(webReadModel)
}
