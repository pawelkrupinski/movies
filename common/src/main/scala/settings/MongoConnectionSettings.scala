package settings

import com.mongodb.{ConnectionString, MongoCompressor}
import org.mongodb.scala.MongoClientSettings

import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters._

/** What one [[services.MongoConnection]] opens, as one typed value: the cluster it dials (`uri`, none: disabled), the database
 *  it binds (`database`), how it dials ([[MongoTuning]]) and the
 *  driver's server-selection cap (`serverSelectionTimeout`: `None` keeps the driver's 30 s, what the prod cluster wants
 *  to ride out a slow or recovering node). A root resolves it once; the connection reads only its plain values. */
final case class MongoConnectionSettings(
    uri: Option[MongoUri],
    database: MongoDatabaseName,
    tuning: MongoTuning = MongoTuning.Default,
    serverSelectionTimeout: Option[MongoServerSelectionTimeout] = None) {

  /** The connection string to dial, when there is a cluster. */
  def connectionString: Option[String] = uri.map(_.value)

  def databaseName: String = database.value

  /** How long the connect probe waits for Mongo to answer. */
  def probeWait: FiniteDuration = tuning.probeTimeout.value

  /** The driver settings a client this connection builds for itself dials `connectionString` with. */
  def clientSettings(connectionString: String): MongoClientSettings =
    MongoConnectionSettings.driverSettings(connectionString, serverSelectionTimeout, tuning)

  /** These settings failing fast: the connect probe and every request's server selection both capped at `wait` -- a
   *  loopback mirror that does not answer within it is down, not slow. */
  def failingFastAfter(wait: FiniteDuration): MongoConnectionSettings =
    copy(tuning = tuning.copy(probeTimeout = MongoProbeTimeout(wait)), serverSelectionTimeout = Some(MongoServerSelectionTimeout(wait)))
}

object MongoConnectionSettings {

  /** The settings of a connection with the driver's own server-selection wait, as the process's cluster wants. */
  def of(uri: Option[MongoUri], database: MongoDatabaseName, tuning: MongoTuning): MongoConnectionSettings =
    MongoConnectionSettings(uri, database, tuning)

  /** Driver settings for a connection string. Wire compression (zlib — built into
   *  the JDK, no dependency) is forced ONLY when the host is loopback: the slow
   *  links are the local ssh tunnel to the prod Mongo host and a loopback read-mirror, where
   *  transfer bytes dominate. Measured there: zlib shrinks the whole-`movies` pull
   *  ~6.6x (4.4MB → 0.65MB), a ~36s tunnel hydrate → ~5s.
   *
   *  Over a direct 6PN link (prod worker/web → `*.internal` mongo) the network is a
   *  fast, already-encrypted local datacenter hop, so compressing every message
   *  would only burn CPU on the driver's I/O event-loop threads — the worker's
   *  measured CPU sink — for bandwidth we don't need. So a non-loopback link stays
   *  uncompressed. A URI that names its own `compressors=` always wins either way.
   *
   *  The tuning's `applicationName` is the `appName` the handshake sends, so mongod's slow-op log lines and
   *  `currentOp` name the tier and country behind a query; a URI's own `appName=` wins. */
  def driverSettings(
      connectionString: String,
      serverSelectionTimeout: Option[MongoServerSelectionTimeout] = None,
      tuning: MongoTuning = MongoTuning.Default): MongoClientSettings = {
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
      builder.applyToConnectionPoolSettings { b => b.maxSize(tuning.maxPoolSize.value); () }
    if (cs.getApplicationName == null) builder.applicationName(tuning.applicationName.value)
    serverSelectionTimeout.foreach { timeout =>
      builder.applyToClusterSettings { b =>
        b.serverSelectionTimeout(timeout.value.toMillis, java.util.concurrent.TimeUnit.MILLISECONDS); ()
      }
    }
    builder.build()
  }

  /** True when every host in the connection string is loopback — i.e. this is the
   *  slow local ssh tunnel or a loopback mirror, not a direct 6PN link.
   *  Wire compression earns its CPU only on such a link (see `driverSettings`). */
  def isLoopbackLink(cs: ConnectionString): Boolean = {
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
