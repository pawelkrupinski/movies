package services

import com.mongodb.{ConnectionString, MongoCompressor}
import org.mongodb.scala.{ClientSession, MongoClient, MongoClientSettings, MongoDatabase, SingleObservableFuture}
import play.api.Logging
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

/**
 * Single Mongo connection shared across every repository. Prior to this,
 * each `Mongo*Repository` built its own `MongoClient` in its lazy init — Movies +
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
 * `close()` in its shutdown hook; each repository's own `close()` becomes a
 * no-op when it borrowed the database from here.
 */
class MongoConnection(
    uri: Option[String],
    dbName: String,
    required: Boolean,
    probeTimeout: FiniteDuration = MongoConnection.DefaultProbeTimeout,
    // Caps the driver's per-request server-selection wait. `None` keeps the
    // driver default (30s) — what the prod cluster wants, to ride out a slow /
    // recovering node. The loopback `/debug` mirror passes a short cap so a dead
    // mirror fails fast instead of wedging every read on the 30s default.
    serverSelectionTimeout: Option[FiniteDuration] = None) extends Logging {

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

  /** Start a fresh `ClientSession` for a multi-document transaction (the staging
   *  fold). `None` when Mongo is disabled. Requires a replica set — a standalone
   *  Mongo rejects transactions; the caller degrades to a non-transactional fold.
   *  The caller owns `close()`-ing the returned session. */
  def startSession(): Option[ClientSession] =
    initResult._1.map(c => Await.result(c.startSession().toFuture(), probeTimeout))

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
          val client = MongoClient(MongoConnection.clientSettings(connectionString, serverSelectionTimeout))
          val db     = client.getDatabase(dbName)
          // Touch the database to surface connectivity errors at boot
          // (same `countDocuments`-against-a-known-collection probe the
          // old per-repository init used). Picking `movies` here because it
          // exists in every environment; an empty collection still
          // round-trips fine. The wait is `probeTimeout` (default 30s) rather
          // than the old hard-coded 10s — see `DefaultProbeTimeout`.
          Await.result(db.getCollection("movies").countDocuments().toFuture(), probeTimeout)
          logger.info(s"MongoConnection connected to $dbName")
          (client, db)
        } match {
          case Success((client, db)) =>
            (Some(client), Some(db))
          case Failure(exception) =>
            val isLocalUri = connectionString.contains("127.0.0.1") || connectionString.contains("localhost")
            val hint = if (isLocalUri)
              " (local URI — start the tunnel with `flyctl proxy 27017:27017 --app kinowo-mongo` and restart, or uncomment the Atlas fallback in .env.local)"
            else ""
            if (required)
              throw new IllegalStateException(
                s"MongoConnection init failed and a Mongo connection is required — refusing to start: ${exception.getMessage}$hint",
                exception)
            logger.error(s"MongoConnection init failed (${exception.getMessage}) — disabled.$hint")
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

  /** Boot-probe + server-selection cap for the loopback `/debug` read-mirror.
   *  The mirror is a LAN Mongo that answers in ~ms when healthy, so — unlike the
   *  prod cluster, which keeps the tolerant 30s `DefaultProbeTimeout` — a mirror
   *  that doesn't respond within a few seconds is simply down. The short cap
   *  turns a down/unreachable mirror into a fast fall-back-to-prod at boot AND a
   *  fast-fail (empty, logged) per request, instead of wedging every `/debug`
   *  load on the driver's 30s default server-selection timeout. */
  val LocalMirrorTimeout: FiniteDuration = 3.seconds

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

  /** Build from an explicit URI rather than `MONGODB_URI` — for a second
   *  connection alongside the primary one. The web wiring uses it for the
   *  local `/debug` read-mirror (`MONGODB_MOVIES_MIRROR_URI`): a separate
   *  MongoClient pointed at a local Mongo that's kept synced from prod, so the
   *  full-corpus `movies` read is a LAN hop instead of the prod tunnel. The
   *  database is taken from the URI's own path (see `databaseFromUri`) so the
   *  mirror can live in a different database than the app's working db; probe
   *  timeout shares `fromEnv`'s resolution. `required` is the caller's to decide
   *  (the mirror is a soft optimisation, so the caller passes `false` and
   *  degrades to the primary connection when it's absent). */
  def fromUri(
      uri: String,
      required: Boolean,
      probeTimeout: FiniteDuration = parseProbeTimeout(Env.get("MONGODB_PROBE_TIMEOUT_SECONDS")),
      serverSelectionTimeout: Option[FiniteDuration] = None): MongoConnection =
    new MongoConnection(
      Some(uri),
      databaseFromUri(uri),
      required,
      probeTimeout,
      serverSelectionTimeout)

  /** Database name for an explicit-URI connection: the URI's own path (e.g.
   *  `…/kinowo_prod_mirror`), so the `/debug` mirror lives in a different
   *  database than the app's working db (`MONGODB_DB`, used by the prod
   *  connection). Falls back to `MONGODB_DB` (then `kinowo`) when the URI names
   *  no database. The parse is guarded so a malformed URI flows through to
   *  `MongoConnection`'s own required/optional handling instead of throwing here. */
  private[services] def databaseFromUri(uri: String): String =
    Try(Option(new ConnectionString(uri).getDatabase)).toOption.flatten
      .filter(_.nonEmpty)
      .getOrElse(Env.get("MONGODB_DB").getOrElse("kinowo"))

  /** Driver settings for a connection string. Wire compression (zlib — built into
   *  the JDK, no dependency) is forced ONLY when the host is loopback: the slow
   *  links are the local `flyctl proxy` tunnel and a loopback read-mirror, where
   *  transfer bytes dominate. Measured there: zlib shrinks the whole-`movies` pull
   *  ~6.6x (4.4MB → 0.65MB), a ~36s tunnel hydrate → ~5s.
   *
   *  Over a direct 6PN link (prod worker/web → `*.internal` mongo) the network is a
   *  fast, already-encrypted local datacenter hop, so compressing every message
   *  would only burn CPU on the driver's async-IO threads — the worker's measured
   *  CPU sink — for bandwidth we don't need. So a non-loopback link stays
   *  uncompressed. A URI that names its own `compressors=` always wins either way. */
  private[services] def clientSettings(
      connectionString: String,
      serverSelectionTimeout: Option[FiniteDuration] = None): MongoClientSettings = {
    val cs      = new ConnectionString(connectionString)
    val builder = MongoClientSettings.builder().applyConnectionString(cs)
    if ((cs.getCompressorList == null || cs.getCompressorList.isEmpty) && isLoopbackLink(cs))
      builder.compressorList(java.util.List.of(MongoCompressor.createZlibCompressor()))
    serverSelectionTimeout.foreach { timeout =>
      builder.applyToClusterSettings { b =>
        b.serverSelectionTimeout(timeout.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS); ()
      }
    }
    builder.build()
  }

  /** True when every host in the connection string is loopback — i.e. this is the
   *  slow local `flyctl proxy` tunnel or a loopback mirror, not a direct 6PN link.
   *  Wire compression earns its CPU only on such a link (see `clientSettings`). */
  private[services] def isLoopbackLink(cs: ConnectionString): Boolean = {
    val hosts = Option(cs.getHosts).map(_.asScala.toList).getOrElse(Nil)
    hosts.nonEmpty && hosts.forall { hostAndPort =>
      val host = hostOf(hostAndPort)
      host == "localhost" || host == "::1" || host == "0:0:0:0:0:0:0:1" || host.startsWith("127.")
    }
  }

  /** The host part of a `host:port` (or bracketed `[ipv6]:port`) entry. */
  private def hostOf(hostAndPort: String): String =
    if (hostAndPort.startsWith("[")) {
      val end = hostAndPort.indexOf(']')
      if (end > 1) hostAndPort.substring(1, end) else hostAndPort
    } else hostAndPort.takeWhile(_ != ':')
}
