package services

import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.{Failure, Success, Try}

import models.Country
import org.mongodb.scala.{MongoClient, MongoDatabase, SingleObservableFuture}
import play.api.Logging
import settings.MongoConnectionSettings
import tools.MongoWriteErrors

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
 * Config ([[MongoConnectionSettings]]: the uri, the database, the tuning) is passed in, not read from the
 * ambient environment — a composition root resolves the process's
 * [[settings.MongoAddress]] (`MONGODB_URI` / `MONGODB_DB`) once and hands it down. Keeping the class a pure
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
 * lives in `Wiring`; `MongoRequirement.of` is its pure core. A root resolves the settings: `MongoAddress.connectionFor`
 * (a country's database on the process's cluster), `MongoConnectionSettings.of` (a database named outright),
 * `settings.MongoMirror` (the local read-mirror), `ProcessConfiguration.toolMongoConnection` (a script's own); the
 * clients several connections share are `tools.MongoClients`'.
 *
 * `close()` is idempotent. Wiring constructs one of these and calls
 * `close()` in its shutdown hook; each repository's own `close()` becomes a
 * no-op when it borrowed the database from here.
 */
class MongoConnection(
    settings: MongoConnectionSettings,
    required: MongoRequirement,
    // An externally-owned `MongoClient` to bind this database view to, SHARED
    // across several per-country connections on one cluster. When set we do NOT
    // build or `close()` a client — the owner (WorkerMain) does — so N countries
    // reuse ONE connection pool / Netty event loop / replica-set monitor thread
    // set instead of each spinning a full client (the RSS blow-up this class was
    // created to avoid). When `None` we build (and close) our own from the settings'
    // uri, the single-connection default every existing call site keeps.
    sharedClient: Option[MongoClient] = None,
    // Run against the database every time a connect attempt SUCCEEDS — at boot and on a
    // background reconnect alike — before the database is published through `database`.
    // A throw fails that attempt: a transient one is retried, anything else aborts a
    // required boot or, on a reconnect, leaves the connection degraded for good. This is
    // where a writer claims its database ([[DatabaseOwner.claimFor]]), so a process that
    // booted with Mongo unreachable cannot start writing, once it reconnects, into a
    // database another country owns. Reads never wait on it: until it passes, `database`
    // is `None`, exactly the degraded state an unreachable Mongo already means.
    onConnected: MongoDatabase => Unit = _ => (),
    // Starts the background reconnect (named, runnable). A daemon thread in production;
    // the seam lets a test decide when that thread runs relative to construction.
    startReconnect: (String, Runnable) => Unit = MongoConnection.startDaemon,
    // The background reconnect's back-off between probes: blocking, on the reconnect's own daemon thread; the seam
    // lets a test run the retry loop without waiting it out.
    reconnectWait: FiniteDuration => Unit = tools.Sleep.Blocking) extends DatabaseBinding with Logging {

  private val dbName = settings.databaseName

  private val connectionRequired = required == MongoRequirement.Required

  // Stops the background reconnect. Without it a closed connection keeps probing
  // — and could publish a live client into a connection its owner has already
  // torn down — plus every test that exercises the degraded path would leak a
  // retry thread for the rest of the JVM's life.
  @volatile private var closed = false
  // Makes a close and the reconnect's publish one step each — see `close()`. Declared before
  // `initResult`, like `closed`: the reconnect thread starts in the constructor and reads both.
  private val publishLock = new AnyRef

  // Eager — connecting now (at construction) surfaces wiring / network
  // problems at boot rather than at the first request. `Wiring` touches
  // `database` at the top of `start()` so the chain fires before any
  // background worker tries to read or write. When `required`, a failure
  // here throws straight out of construction and aborts boot.
  // `var` because a `required` connection that found Mongo UNREACHABLE starts
  // degraded and swaps a live client in from the background retry (see
  // `retryInBackground`). `@volatile` so readers on request threads see it.
  @volatile private var initResult: (Option[MongoClient], Option[MongoDatabase]) = init()

  // A `required` connection still without a database after `init` is the degraded one
  // (every other required failure threw): reconnect in the background. Started only
  // HERE, once `initResult` is assigned — the reconnect loop reads it straight away,
  // and started from inside `init` it could read the unassigned field, die on an NPE,
  // and leave the connection degraded for good.
  if (connectionRequired && initResult._2.isEmpty) settings.connectionString.foreach(retryInBackground)

  /** A required connection that found Mongo unreachable at boot: every repository wired from
   *  [[database]] then holds `None` for good — see [[DatabaseBinding]]. */
  val boundDegraded: Boolean = connectionRequired && initResult._2.isEmpty

  /** The reconnect published a database nothing wired at boot will read: restart to use it. */
  def restartRequired: Boolean = boundDegraded && initResult._2.isDefined

  /** The shared `MongoDatabase` view — pre-bound to `dbName`. Repos
   *  `.withCodecRegistry(...)` it. `None` only when `required` is false and
   *  Mongo was absent/unreachable, or while a `required` one is degraded. */
  def database: Option[MongoDatabase] = initResult._2

  // Only close a client WE built. A `sharedClient` is owned by the caller
  // (WorkerMain closes it once, after every borrowing connection is stopped).
  //
  // Under `publishLock`, with the reconnect's publish: `closed` and `initResult` are two
  // fields, and a close landing between the reconnect's check of the one and its write of the
  // other found no client to close, while the reconnect published one nobody would close.
  def close(): Unit = {
    val built = publishLock.synchronized { closed = true; initResult._1 }
    if (sharedClient.isEmpty) built.foreach(_.close())
  }

  /** One connect attempt: build (or borrow) the client and PROBE it. Shared by the
   *  boot path and the background retry so both mean exactly the same thing by
   *  "connected" — a probe that actually round-tripped, not a constructed client. */
  private def connect(connectionString: String): Try[(MongoClient, MongoDatabase)] = Try {
    val client = sharedClient.getOrElse(MongoClient(settings.clientSettings(connectionString)))
    val db     = client.getDatabase(dbName)
    MongoWriteErrors.opened(dbName)
    // Touch the database to surface connectivity errors at boot
    // (same `countDocuments`-against-a-known-collection probe the
    // old per-repository init used). Picking `movies` here because it
    // exists in every environment; an empty collection still
    // round-trips fine. The wait is the settings' probe timeout (default 30s) rather
    // than the old hard-coded 10s — see `DefaultProbeTimeout`.
    tools.AnyThrowable.rethrownAfter {
      Await.result(db.getCollection("movies").countDocuments().toFuture(), settings.probeWait)
      onConnected(db)
    } { e => MongoWriteErrors.observeRead(dbName, e); if (sharedClient.isEmpty) client.close() }
    logger.info(s"MongoConnection connected to $dbName")
    (client, db)
  }

  /** Keep trying to reach an UNREACHABLE-at-boot Mongo, and publish the connection
   *  once it answers. Daemon thread so it never holds shutdown open. The backoff
   *  matters as much as the retry: a tight loop of connect probes from every web
   *  machine is the same stampede that kept the cluster down, so back off to a
   *  minute and stay there. Only reached when `required` — an optional connection
   *  that failed stays disabled, as before. */
  private def retryInBackground(connectionString: String): Unit =
    startReconnect(s"mongo-reconnect-$dbName", () => reconnectFrom(connectionString, waitSeconds = 5L))

  /** The reconnect loop: wait, try, and go on -- the wait doubled, up to a minute -- until a database is published,
   *  the connection is closed, or the database is refused for good. */
  @scala.annotation.tailrec
  private def reconnectFrom(connectionString: String, waitSeconds: Long): Unit =
    if (!closed && initResult._2.isEmpty) {
      reconnectWait(waitSeconds.seconds)
      if (!closed) reconnectAttempt(connectionString, waitSeconds) match {
        case Some(next) => reconnectFrom(connectionString, next)
        case None       => ()
      }
    }

  /** One reconnect attempt after waiting `waitSeconds`: the next wait when Mongo is still unreachable, or `None`
   *  when the attempt settled it -- published, or refused for good. */
  private def reconnectAttempt(connectionString: String, waitSeconds: Long): Option[Long] =
    connect(connectionString) match {
      case Success((client, db)) =>
        publish(client, db)
        None
      case Failure(exception: DatabaseOwnershipConflict) =>
        // Reachable, but refused for good — the `onConnected` claim found another
        // country's database. Stay degraded: publishing it would let writes through.
        // Only that: any other failure (an auth error mid role change, a command error)
        // is retried, as before the claim existed — giving up left the process
        // degraded until something restarted it.
        logger.error(s"MongoConnection to $dbName reconnected but was REFUSED (${exception.getMessage}) — " +
          "staying degraded, nothing will be read from or written to it.")
        None
      case Failure(exception) =>
        val next = math.min(waitSeconds * 2, 60L)
        logger.warn(s"MongoConnection to $dbName still unreachable (${exception.getMessage}) — " +
          s"retrying in ${next}s.")
        Some(next)
    }

  /** Publish a reconnected client -- unless `close()` landed while the probe was in flight: publishing then would
   *  hand the owner a client it will never close, so it is closed here instead. One step with `close()` — see there. */
  private def publish(client: MongoClient, db: MongoDatabase): Unit = {
    val published = publishLock.synchronized { if (!closed) initResult = (Some(client), Some(db)); !closed }
    if (published) logger.warn(s"MongoConnection to $dbName RECOVERED — but every repository wired at boot " +
      "holds no database; the process reports itself unhealthy so it is restarted onto it.")
    else if (sharedClient.isEmpty) client.close()
  }

  private def init(): (Option[MongoClient], Option[MongoDatabase]) =
    settings.connectionString match {
      case None =>
        if (connectionRequired)
          throw new IllegalStateException(
            "MONGODB_URI is not set but a Mongo connection is required — refusing to start.")
        logger.info("MONGODB_URI not set — MongoConnection disabled.")
        (None, None)
      case Some(connectionString) =>
        connect(connectionString) match {
          case Success((client, db)) => (Some(client), Some(db))
          case Failure(exception)    => unconnected(connectionString, exception)
        }
    }

  /** The boot's connect attempt failed with `exception`.
   *
   *  A REACHABILITY failure must not abort boot, even when `required`. Refusing
   *  to start means Play never binds port 9000, so Fly's health check can never
   *  pass and the machine crash-loops — and every restart re-runs the boot
   *  hydrates, keeping an already-overloaded cluster too busy to recover. That
   *  is exactly how a slow Mongo became a total outage on 2026-07-18. Start
   *  degraded instead (`database` None → repos no-op, pages render film-less),
   *  let the health check pass, and reconnect in the background once the
   *  cluster answers — and, once it does, report not-alive (`restartRequired`) so the
   *  probe restarts the process: the repositories wired now keep their `None` for good
   *  (see DatabaseBinding). A MISCONFIGURATION still aborts: nothing about a bad URI
   *  or bad credentials fixes itself, so failing loudly at boot is right. */
  private def unconnected(connectionString: String, exception: Throwable): (Option[MongoClient], Option[MongoDatabase]) = {
    val hint = MongoConnection.localUriHint(connectionString)
    if (connectionRequired && !MongoConnection.isTransient(exception))
      throw new IllegalStateException(
        s"MongoConnection init failed and a Mongo connection is required — refusing to start: ${exception.getMessage}$hint",
        exception)
    if (connectionRequired)
      logger.error(s"MongoConnection to $dbName is UNREACHABLE (${exception.getMessage}) — " +
        s"starting DEGRADED rather than crash-looping; retrying in the background.$hint")
    else
      logger.error(s"MongoConnection init failed (${exception.getMessage}) — disabled.$hint")
    (None, None)
  }
}

object MongoConnection {
  /** What a failed connect to a LOCAL URI adds to its log line: how to reach prod, which is never local. */
  private def localUriHint(connectionString: String): String =
    if (connectionString.contains("127.0.0.1") || connectionString.contains("localhost"))
      " (local URI — for prod, launch under `scripts/with-prod-mongo.sh`, which opens the ssh tunnel to mongo-1)"
    else ""

  /** The production reconnect starter: a daemon thread, so it never holds shutdown open. */
  def startDaemon(name: String, body: Runnable): Unit = {
    val thread = new Thread(body, name)
    thread.setDaemon(true)
    thread.start()
  }

  /** Is this init failure worth waiting out? Pure classifier, and the other half of
   *  the boot-failure decision alongside [[MongoRequirement.of]].
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

  /** A connection WRITING `country`'s corpus — the worker and every `worker/Test/runMain scripts.*` tool that writes —
   *  claimed for that country through [[DatabaseOwner]] before the caller can write anything. What the claim adds is
   *  that settings naming ANOTHER country's database (`.env.local`'s `kinowo` under a German run — how DE/UK rows
   *  reached the Polish corpus) are refused with an `IllegalStateException` instead of written into. The claim runs on
   *  every successful connect, including a background reconnect after an unreachable boot. */
  def forCountry(settings: MongoConnectionSettings, required: MongoRequirement, country: Country,
      sharedClient: Option[MongoClient] = None): MongoConnection =
    new MongoConnection(settings, required, sharedClient, onConnected = DatabaseOwner.claimFor(country))
}
