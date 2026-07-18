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
    serverSelectionTimeout: Option[FiniteDuration] = None,
    // An externally-owned `MongoClient` to bind this database view to, SHARED
    // across several per-country connections on one cluster. When set we do NOT
    // build or `close()` a client — the owner (WorkerMain) does — so N countries
    // reuse ONE connection pool / Netty event loop / replica-set monitor thread
    // set instead of each spinning a full client (the RSS blow-up this class was
    // created to avoid). When `None` we build (and close) our own from `uri`, the
    // single-connection default every existing call site keeps.
    sharedClient: Option[MongoClient] = None) extends Logging {

  // Eager — connecting now (at construction) surfaces wiring / network
  // problems at boot rather than at the first request. `Wiring` touches
  // `database` at the top of `start()` so the chain fires before any
  // background worker tries to read or write. When `required`, a failure
  // here throws straight out of construction and aborts boot.
  // `var` because a `required` connection that found Mongo UNREACHABLE starts
  // degraded and swaps a live client in from the background retry (see
  // `retryInBackground`). `@volatile` so readers on request threads see it.
  @volatile private var initResult: (Option[MongoClient], Option[MongoDatabase]) = init()

  /** The shared `MongoDatabase` view — pre-bound to `dbName`. Repos
   *  `.withCodecRegistry(...)` it. `None` only when `required` is false and
   *  Mongo was absent/unreachable (when `required`, init threw instead). */
  def database: Option[MongoDatabase] = initResult._2

  // Stops the background reconnect. Without it a closed connection keeps probing
  // — and could publish a live client into a connection its owner has already
  // torn down — plus every test that exercises the degraded path would leak a
  // retry thread for the rest of the JVM's life.
  @volatile private var closed = false

  // Only close a client WE built. A `sharedClient` is owned by the caller
  // (WorkerMain closes it once, after every borrowing connection is stopped).
  def close(): Unit = {
    closed = true
    if (sharedClient.isEmpty) initResult._1.foreach(_.close())
  }

  /** Start a fresh `ClientSession` for a multi-document transaction (the staging
   *  fold). `None` when Mongo is disabled. Requires a replica set — a standalone
   *  Mongo rejects transactions; the caller degrades to a non-transactional fold.
   *  The caller owns `close()`-ing the returned session. */
  def startSession(): Option[ClientSession] =
    initResult._1.map(c => Await.result(c.startSession().toFuture(), probeTimeout))

  /** One connect attempt: build (or borrow) the client and PROBE it. Shared by the
   *  boot path and the background retry so both mean exactly the same thing by
   *  "connected" — a probe that actually round-tripped, not a constructed client. */
  private def connect(connectionString: String): Try[(MongoClient, MongoDatabase)] = Try {
    val client = sharedClient.getOrElse(
      MongoClient(MongoConnection.clientSettings(connectionString, serverSelectionTimeout)))
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
  }

  /** Keep trying to reach an UNREACHABLE-at-boot Mongo, and publish the connection
   *  once it answers. Daemon thread so it never holds shutdown open. The backoff
   *  matters as much as the retry: a tight loop of connect probes from every web
   *  machine is the same stampede that kept the cluster down, so back off to a
   *  minute and stay there. Only reached when `required` — an optional connection
   *  that failed stays disabled, as before. */
  private def retryInBackground(connectionString: String): Unit = {
    val thread = new Thread(
      () => {
        var waitSeconds = 5L
        while (!closed && initResult._2.isEmpty) {
          Thread.sleep(waitSeconds * 1000L)
          if (!closed)
            connect(connectionString) match {
              case Success((client, db)) =>
                // Re-check under the close flag: `close()` may have landed while the
                // probe was in flight, and publishing here would hand the owner a
                // client it will never close.
                if (closed) client.close()
                else {
                  initResult = (Some(client), Some(db))
                  logger.info(s"MongoConnection to $dbName RECOVERED — serving from the database again.")
                }
              case Failure(exception) =>
                waitSeconds = math.min(waitSeconds * 2, 60L)
                logger.warn(s"MongoConnection to $dbName still unreachable (${exception.getMessage}) — " +
                  s"retrying in ${waitSeconds}s.")
            }
        }
      },
      s"mongo-reconnect-$dbName")
    thread.setDaemon(true)
    thread.start()
  }

  private def init(): (Option[MongoClient], Option[MongoDatabase]) =
    uri match {
      case None =>
        if (required)
          throw new IllegalStateException(
            "MONGODB_URI is not set but a Mongo connection is required — refusing to start.")
        logger.info("MONGODB_URI not set — MongoConnection disabled.")
        (None, None)
      case Some(connectionString) =>
        connect(connectionString) match {
          case Success((client, db)) =>
            (Some(client), Some(db))
          case Failure(exception) =>
            val isLocalUri = connectionString.contains("127.0.0.1") || connectionString.contains("localhost")
            val hint = if (isLocalUri)
              " (local URI — start the tunnel with `flyctl proxy 27017:27017 --app kinowo-mongo` and restart, or uncomment the Atlas fallback in .env.local)"
            else ""
            // A REACHABILITY failure must not abort boot, even when `required`. Refusing
            // to start means Play never binds port 9000, so Fly's health check can never
            // pass and the machine crash-loops — and every restart re-runs the boot
            // hydrates, keeping an already-overloaded cluster too busy to recover. That
            // is exactly how a slow Mongo became a total outage on 2026-07-18. Start
            // degraded instead (`database` None → repos no-op, pages render film-less),
            // let the health check pass, and reconnect in the background once the
            // cluster answers. A MISCONFIGURATION still aborts: nothing about a bad URI
            // or bad credentials fixes itself, so failing loudly at boot is right.
            if (required && !MongoConnection.isTransient(exception))
              throw new IllegalStateException(
                s"MongoConnection init failed and a Mongo connection is required — refusing to start: ${exception.getMessage}$hint",
                exception)
            if (required) {
              logger.error(s"MongoConnection to $dbName is UNREACHABLE (${exception.getMessage}) — " +
                s"starting DEGRADED rather than crash-looping; retrying in the background.$hint")
              retryInBackground(connectionString)
            } else
              logger.error(s"MongoConnection init failed (${exception.getMessage}) — disabled.$hint")
            (None, None)
        }
    }
}

object MongoConnection extends Logging {
  /** Pure policy core for the boot-failure decision: Mongo is required
   *  everywhere except tests, unless a local dev opted out (e.g. via
   *  `MONGODB_OPTIONAL`). Kept boolean-only — no Play `Mode`, no env reads —
   *  so the services layer doesn't depend on the framework and the rule
   *  stays trivially unit-testable. */
  def isRequired(testMode: Boolean, optedOut: Boolean): Boolean = !testMode && !optedOut

  /** Is this init failure worth waiting out? Pure classifier, and the other half of
   *  the boot-failure decision alongside [[isRequired]].
   *
   *  TRANSIENT — the cluster is there but did not answer in time (server selection
   *  timed out, a socket timed out, the connection dropped). Retrying fixes it, so
   *  a `required` connection starts DEGRADED rather than aborting boot: a process
   *  that refuses to start never binds its port, never passes a health check, and
   *  crash-loops — re-running its boot hydrates each time and keeping an
   *  overloaded cluster from recovering (the 2026-07-18 outage).
   *
   *  PERMANENT — everything else: a malformed connection string, bad credentials,
   *  an unknown database. No amount of retrying helps, so failing loudly at boot
   *  is the honest signal and the existing behaviour is kept.
   *
   *  Deliberately matched on the driver's TIMEOUT/SOCKET family rather than on
   *  message text, which changes between driver versions. `MongoSecurityException`
   *  is checked FIRST because it extends `MongoException` and can wrap an
   *  `IOException` — an auth failure must never read as transient. */
  def isTransient(exception: Throwable): Boolean = exception match {
    case _: com.mongodb.MongoSecurityException      => false
    case _: com.mongodb.MongoTimeoutException       => true
    case _: com.mongodb.MongoSocketException        => true
    case _: com.mongodb.MongoNotPrimaryException    => true
    case _: com.mongodb.MongoNodeIsRecoveringException => true
    case _: java.util.concurrent.TimeoutException   => true
    case _                                          => false
  }

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
   *  defaulting the db name to the country's database via
   *  `Country.resolvedDbName` — `kinowo` for Poland) — the wiring's entry point.
   *  `required = true` turns a missing or unreachable Mongo into a hard boot
   *  failure instead of silent degradation. */
  def fromEnv(required: Boolean): MongoConnection =
    new MongoConnection(
      Env.get("MONGODB_URI"),
      models.Country.resolvedDbName,
      required,
      parseProbeTimeout(Env.get("MONGODB_PROBE_TIMEOUT_SECONDS")))

  /** Like [[fromEnv]] but against an EXPLICIT database name (the caller's
   *  country, via `Country.dbNameFor`) and optionally bound to a `sharedClient`
   *  so several per-country connections reuse ONE pool. The worker builds one of
   *  these per country it runs; `fromEnv` stays the single-db (web) entry point.
   *  `MONGODB_DB` is already folded into `dbName` by the caller, so it isn't
   *  re-read here. */
  def fromEnvForDb(dbName: String, required: Boolean,
      sharedClient: Option[MongoClient] = None): MongoConnection =
    new MongoConnection(
      Env.get("MONGODB_URI"),
      dbName,
      required,
      parseProbeTimeout(Env.get("MONGODB_PROBE_TIMEOUT_SECONDS")),
      sharedClient = sharedClient)

  /** One shared `MongoClient` for the whole process, built from `MONGODB_URI`,
   *  to be bound to per-country database views via [[fromEnvForDb]]'s
   *  `sharedClient`. `None` when `MONGODB_URI` is unset (local opt-out) — each
   *  connection then degrades on its own. The caller OWNS `close()`-ing the
   *  returned client, after every connection that borrowed it is closed. */
  def sharedClientFromEnv(serverSelectionTimeout: Option[FiniteDuration] = None): Option[MongoClient] =
    Env.get("MONGODB_URI").map(cs => MongoClient(clientSettings(cs, serverSelectionTimeout)))

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
   *  connection). Falls back to `Country.resolvedDbName` (explicit `MONGODB_DB`,
   *  else the country's database) when the URI names no database. The parse is
   *  guarded so a malformed URI flows through to
   *  `MongoConnection`'s own required/optional handling instead of throwing here. */
  private[services] def databaseFromUri(uri: String): String =
    Try(Option(new ConnectionString(uri).getDatabase)).toOption.flatten
      .filter(_.nonEmpty)
      .getOrElse(models.Country.resolvedDbName)

  /** Driver settings for a connection string. Wire compression (zlib — built into
   *  the JDK, no dependency) is forced ONLY when the host is loopback: the slow
   *  links are the local `flyctl proxy` tunnel and a loopback read-mirror, where
   *  transfer bytes dominate. Measured there: zlib shrinks the whole-`movies` pull
   *  ~6.6x (4.4MB → 0.65MB), a ~36s tunnel hydrate → ~5s.
   *
   *  Over a direct 6PN link (prod worker/web → `*.internal` mongo) the network is a
   *  fast, already-encrypted local datacenter hop, so compressing every message
   *  would only burn CPU on the driver's I/O event-loop threads — the worker's
   *  measured CPU sink — for bandwidth we don't need. So a non-loopback link stays
   *  uncompressed. A URI that names its own `compressors=` always wins either way. */
  /** Connections per `MongoClient`. Deliberately far below the driver's default of
   *  100: every app holds its own pool against ONE small shared mongod, so the default
   *  lets a handful of clients reserve the whole box. 25 still leaves ample headroom —
   *  the worker's background concurrency and the web's request threads are both well
   *  under it — while capping the fleet's worst case to something the VM can hold.
   *  Tunable without a code change (same pattern as the credit thresholds) because
   *  it is a capacity knob we may need to move under pressure. */
  private[services] val MaxPoolSize: Int = Env.positiveInt("KINOWO_MONGO_MAX_POOL_SIZE", 25)

  private[services] def clientSettings(
      connectionString: String,
      serverSelectionTimeout: Option[FiniteDuration] = None): MongoClientSettings = {
    val cs      = new ConnectionString(connectionString)
    val builder = MongoClientSettings.builder().applyConnectionString(cs)
    // Socket I/O uses the driver's DEFAULT transport (JDK NIO2). We briefly routed it
    // through Netty native-epoll to escape a suspected `sun.nio.ch` selector spin, but
    // that was a red herring: the recurring CPU-credit floor was read-model PROJECTION
    // cost, not an epoll spin. The fix was moving projection OFF the Mongo I/O threads
    // (change-stream apply offload) and making projection cheap — the transport was
    // irrelevant, so it's back to the driver default.
    if ((cs.getCompressorList == null || cs.getCompressorList.isEmpty) && isLoopbackLink(cs))
      builder.compressorList(java.util.List.of(MongoCompressor.createZlibCompressor()))
    // Bound the pool. The driver's default is 100 connections PER CLIENT, and mongod
    // costs roughly a megabyte of RSS per connection — so six-ish clients (web x3,
    // worker x3, doubling while a rolling deploy runs old and new machines at once)
    // can reserve more than the 962 MB the kinowo-mongo VM has. That is the shape of
    // the memory exhaustion measured 2026-07-18: mongo sat 285 min/day under the 8%
    // mem-available alarm and 45 min at 0%, which is NOT the dataset (PL is ~53 MB of
    // data), and web boots hydrate straight off Mongo — so when it thrashed, boot went
    // 20s -> 4m+ and flyctl's health-check wait expired, failing the deploy.
    // A URI that names its own `maxPoolSize=` still wins, same as `compressors=`.
    if (cs.getMaxConnectionPoolSize == null)
      builder.applyToConnectionPoolSettings { b => b.maxSize(MaxPoolSize); () }
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
