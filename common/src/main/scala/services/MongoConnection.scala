package services

import org.mongodb.scala.{MongoClient, MongoDatabase, SingleObservableFuture}
import play.api.Logging
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

/**
 * Single Mongo connection shared across every repository. Prior to this,
 * each `Mongo*Repo` built its own `MongoClient` in its lazy init — Movies +
 * Users + UserStates added up to three independent connection pools, three
 * Netty event loops, three replica-set monitor threads, and three Mongo
 * driver memory footprints on the same Atlas cluster. That tipped the JVM
 * RSS past the 512 MB Fly cgroup ceiling (the OOM-restart cycle
 * `b03hleb3x` chased down — bisected to `65c9368` Phase A, which added the
 * second + third clients).
 *
 * Standard Mongo driver usage is one `MongoClient` per process. This class
 * is that single client; repos take its `database` as an
 * `Option[MongoDatabase]` constructor arg, apply their own codec registry
 * via `.withCodecRegistry(...)`, and `getCollection[T]` from the resulting
 * view. The underlying connection pool / IO threads / monitor threads are
 * shared.
 *
 * Config (`uri` / `dbName` / `required`) is passed in, not read from the
 * ambient environment — `MongoConnection.fromEnv` is the production entry
 * point that resolves `MONGODB_URI` / `MONGODB_DB`. Keeping the class a pure
 * function of its inputs is what lets the boot-failure behaviour be tested
 * without poking at process env vars.
 *
 * `required` decides what an absent/unreachable Mongo means:
 *   - `false` (tests, or a local dev who opted out) — disable silently and
 *     degrade: `database` is `None`, pages render with no films, repos no-op.
 *   - `true`  (production + local dev by default) — throw at construction so
 *     the app refuses to start rather than serve a broken, film-less site.
 *
 * The mode → `required` mapping (plus the `MONGODB_OPTIONAL` local opt-out)
 * lives in `Wiring`; `MongoConnection.isRequired` is its pure core.
 *
 * `close()` is idempotent. Wiring constructs one of these and calls
 * `close()` in its shutdown hook; each repo's own `close()` becomes a
 * no-op when it borrowed the database from here.
 */
class MongoConnection(
    uri: Option[String],
    dbName: String,
    required: Boolean,
    probeTimeout: FiniteDuration = MongoConnection.DefaultProbeTimeout) extends Logging {

  // Eager — connecting now (at construction) surfaces wiring / network
  // problems at boot rather than at the first request. `Wiring` touches
  // `database` at the top of `start()` so the chain fires before any
  // background worker tries to read or write. When `required`, a failure
  // here throws straight out of construction and aborts boot.
  private val initResult: (Option[MongoClient], Option[MongoDatabase]) = init()

  /** The shared `MongoDatabase` view — pre-bound to `dbName`. Repos
   *  `.withCodecRegistry(...)` it. `None` only when `required` is false and
   *  Mongo was absent/unreachable (when `required`, init threw instead). */
  def database: Option[MongoDatabase] = initResult._2

  def close(): Unit = initResult._1.foreach(_.close())

  private def init(): (Option[MongoClient], Option[MongoDatabase]) =
    uri match {
      case None =>
        if (required)
          throw new IllegalStateException(
            "MONGODB_URI is not set but a Mongo connection is required — refusing to start.")
        logger.info("MONGODB_URI not set — MongoConnection disabled.")
        (None, None)
      case Some(connectionString) =>
        Try {
          val client = MongoClient(connectionString)
          val db     = client.getDatabase(dbName)
          // Touch the database to surface connectivity errors at boot
          // (same `countDocuments`-against-a-known-collection probe the
          // old per-repo init used). Picking `movies` here because it
          // exists in every environment; an empty collection still
          // round-trips fine. The wait is `probeTimeout` (default 30s) rather
          // than the old hard-coded 10s — see `DefaultProbeTimeout`.
          Await.result(db.getCollection("movies").countDocuments().toFuture(), probeTimeout)
          logger.info(s"MongoConnection connected to $dbName")
          (client, db)
        } match {
          case Success((client, db)) =>
            (Some(client), Some(db))
          case Failure(ex) =>
            val isLocalUri = connectionString.contains("127.0.0.1") || connectionString.contains("localhost")
            val hint = if (isLocalUri)
              " (local URI — start the tunnel with `flyctl proxy 27017:27017 --app kinowo-mongo` and restart, or uncomment the Atlas fallback in .env.local)"
            else ""
            if (required)
              throw new IllegalStateException(
                s"MongoConnection init failed and a Mongo connection is required — refusing to start: ${ex.getMessage}$hint",
                ex)
            logger.error(s"MongoConnection init failed (${ex.getMessage}) — disabled.$hint")
            (None, None)
        }
    }
}

object MongoConnection {
  /** Pure policy core for the boot-failure decision: Mongo is required
   *  everywhere except tests, unless a local dev opted out (e.g. via
   *  `MONGODB_OPTIONAL`). Kept boolean-only — no Play `Mode`, no env reads —
   *  so the services layer doesn't depend on the framework and the rule
   *  stays trivially unit-testable. */
  def isRequired(testMode: Boolean, optedOut: Boolean): Boolean = !testMode && !optedOut

  /** Boot connectivity-probe timeout. Raised from a hard-coded 10s after the
   *  2026-06-06 incident: the self-hosted Mongo went unresponsive under memory
   *  pressure (it OOM-killed minutes later), the 10s `countDocuments` probe
   *  timed out, the web process exited 255 → crash-looped → Fly hit the
   *  max-restart-count and stopped the machine. 30s rides out a momentarily
   *  slow or freshly-restarted Mongo. Override with
   *  `MONGODB_PROBE_TIMEOUT_SECONDS`. */
  val DefaultProbeTimeout: FiniteDuration = 30.seconds

  /** Parse `MONGODB_PROBE_TIMEOUT_SECONDS` into a positive second count,
   *  falling back to `DefaultProbeTimeout` on absent / non-numeric /
   *  non-positive input. Pure (takes the raw string, not the env) so it's
   *  unit-testable without poking at process env vars. */
  private[services] def parseProbeTimeout(raw: Option[String]): FiniteDuration =
    raw.flatMap(_.toIntOption).filter(_ > 0).map(_.seconds).getOrElse(DefaultProbeTimeout)

  /** Build from the ambient environment (`MONGODB_URI` / `MONGODB_DB`,
   *  defaulting the db name to `kinowo`) — the wiring's entry point.
   *  `required = true` turns a missing or unreachable Mongo into a hard boot
   *  failure instead of silent degradation. */
  def fromEnv(required: Boolean): MongoConnection =
    new MongoConnection(
      Env.get("MONGODB_URI"),
      Env.get("MONGODB_DB").getOrElse("kinowo"),
      required,
      parseProbeTimeout(Env.get("MONGODB_PROBE_TIMEOUT_SECONDS")))
}
