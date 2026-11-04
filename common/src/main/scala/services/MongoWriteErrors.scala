package services

import io.prometheus.metrics.core.metrics.Counter
import io.prometheus.metrics.model.registry.PrometheusRegistry
import org.mongodb.scala.MongoCollection

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters._

/**
 * Every Mongo write that threw, counted per database and collection, and every command Mongo refused for want of a
 * grant — on the tier's registry ([[MongoWriteErrors.Series.worker]] or [[MongoWriteErrors.Series.web]]):
 *
 *  - `kinowo_<tier>_mongo_write_errors_total{database,collection,kind}` — `serialization` (the driver could not encode
 *    the document: a NUL in a field name, a codec missing), `rejected` (the server refused the document: a validator, a
 *    write concern; never a duplicate key alone, see [[kindOf]]) or `failed` (the write command itself failed). Alerted by `MongoWriteErrors`
 *    (mongodb-app.rules). Every collection a write reaches is seeded at 0 for each kind on its first write, so the first
 *    failure moves `increase()`, and a store built at boot seeds its collection then ([[seed]]), so the family exists
 *    before the first write;
 *  - `kinowo_<tier>_mongo_unauthorized_total{database}` — a command answered "not authorized on <db>" (code 13): the
 *    per-database grant `scripts/mongo-grants` gives is missing, and the pod stays healthy doing nothing. Seeded at 0 for
 *    each database a connection opens. Alerted (paged) by `MongoUnauthorized`.
 *
 * PROCESS-WIDE, like the one `MongoClient` behind every repository ([[MongoConnection]]): a repository is handed a
 * `MongoDatabase`, not the process's metrics, so the stores report here and the composition root [[bind]]s the tier's
 * series once at boot. Unbound (scripts, specs), nothing is counted. Why it exists: receipt keys holding a NUL made every
 * such agreement verdict write throw `BsonSerializationException` (fixed in 260904c36), and nothing counted it.
 */
object MongoWriteErrors {

  /** What a failed write is counted as. */
  enum Kind(val label: String) {
    case Serialization extends Kind("serialization")
    case Rejected      extends Kind("rejected")
    case Failed        extends Kind("failed")
  }

  /** Where the counts go. */
  trait Sink {
    def writeFailed(database: String, collection: String, kind: Kind): Unit
    def unauthorized(database: String): Unit
    /** Seed `collection`'s series at 0. */
    def touched(database: String, collection: String): Unit
    /** Seed `database`'s refusals at 0. */
    def opened(database: String): Unit

    /** `body`, a write to `collection`, with its failure counted and rethrown. */
    final def counting[A](database: String, collection: String)(body: => A): A = {
      touched(database, collection)
      try body
      catch {
        case exception: Throwable =>
          observe(database, collection, exception)
          throw exception
      }
    }

    /** Count `exception`, thrown by a write to `collection`, if it is a Mongo failure ([[kindOf]]); a client closing
     *  under a shutdown, a timeout and an interrupt are not. */
    final def observe(database: String, collection: String, exception: Throwable): Unit =
      if (isUnauthorized(exception)) unauthorized(database)
      else kindOf(exception).foreach(writeFailed(database, collection, _))
  }

  val noop: Sink = new Sink {
    def writeFailed(database: String, collection: String, kind: Kind): Unit = ()
    def unauthorized(database: String): Unit = ()
    def touched(database: String, collection: String): Unit = ()
    def opened(database: String): Unit = ()
  }

  @volatile private var sink: Sink = noop

  /** Count into `series` from now on: the composition root's call, once per process. */
  def bind(series: Sink): Unit = sink = series

  /** `body`, a write to `collection`, counted into the bound sink ([[Sink.counting]]). */
  def counted[A](collection: MongoCollection[?])(body: => A): A =
    sink.counting(collection.namespace.getDatabaseName, collection.namespace.getCollectionName)(body)

  /** Seed `collection`'s series at 0 now, before any write: for a store built at boot whose first write may never come
   *  (the web's `userStates` in a country with no signed-in visitor). Without it the family has no series at all, and
   *  `AppMetricAbsent` (app-metrics-absent.rules) reads that as the counter unwired. Call after [[bind]]. */
  def seed(collection: MongoCollection[?]): Unit =
    sink.touched(collection.namespace.getDatabaseName, collection.namespace.getCollectionName)

  /** [[Sink.observe]] on the bound sink. */
  def observe(database: String, collection: String, exception: Throwable): Unit = sink.observe(database, collection, exception)

  /** Count `exception`, thrown by a read or a probe of `database`, if it is a refused grant. */
  def observeRead(database: String, exception: Throwable): Unit =
    if (isUnauthorized(exception)) sink.unauthorized(database)

  /** Seed `database`'s refusals: a connection opening it calls this. */
  def opened(database: String): Unit = sink.opened(database)

  /** Whether `exception` (or a cause) is Mongo's "not authorized on <db>": code 13, Unauthorized. */
  def isUnauthorized(exception: Throwable): Boolean = causes(exception).exists {
    case server: com.mongodb.MongoServerException => server.getCode == UnauthorizedCode
    case _                                        => false
  }

  /** The kind a failed write counts as, if it is a Mongo failure at all. A duplicate key alone is not one: it is how a
   *  guarded write and a claim learn another writer got there first (`MongoGuard`, `MongoTaskQueue`), every day. Nor is
   *  the server out of reach, which the connection's own probes and the pod's health answer. */
  def kindOf(exception: Throwable): Option[Kind] = causes(exception).collectFirst {
    case _: org.bson.BsonSerializationException | _: org.bson.codecs.configuration.CodecConfigurationException |
         _: org.bson.BsonInvalidOperationException => Some(Kind.Serialization)
    case write: com.mongodb.MongoWriteException => Option.when(write.getError.getCode != DuplicateKeyCode)(Kind.Rejected)
    case bulk: com.mongodb.MongoBulkWriteException =>
      Option.when(bulk.getWriteConcernError != null || bulk.getWriteErrors.asScala.exists(_.getCode != DuplicateKeyCode))(Kind.Rejected)
    case _: com.mongodb.MongoServerException => Some(Kind.Failed)
  }.flatten

  private def causes(exception: Throwable): Iterator[Throwable] =
    Iterator.iterate(exception)(_.getCause).takeWhile(_ != null).take(10)

  /** Mongo's error code for a command the user's roles do not allow. */
  val UnauthorizedCode = 13

  /** Mongo's error code for a write that met another document's unique key. */
  val DuplicateKeyCode = 11000

  /** The Prometheus-backed [[Sink]], one per tier, each family spelt whole so the metric-coverage scans see it. */
  final class Series private (registry: PrometheusRegistry, writeErrorsName: String, unauthorizedName: String) extends Sink {
    private val writeErrors: Counter = Counter.builder()
      .name(writeErrorsName)
      .help("Mongo writes that threw, by database, collection and kind (serialization: the driver could not encode the document; rejected: the server refused it; failed: the command failed). Zero is the healthy reading.")
      .labelNames("database", "collection", "kind")
      .register(registry)

    private val refusals: Counter = Counter.builder()
      .name(unauthorizedName)
      .help("Mongo commands refused as not authorized (code 13) by database: a missing per-database grant (scripts/mongo-grants). Zero is the healthy reading.")
      .labelNames("database")
      .register(registry)

    private val seeded = ConcurrentHashMap.newKeySet[(String, String)]()

    def writeFailed(database: String, collection: String, kind: Kind): Unit =
      writeErrors.labelValues(database, collection, kind.label).inc()
    def unauthorized(database: String): Unit = refusals.labelValues(database).inc()
    def touched(database: String, collection: String): Unit =
      if (seeded.add((database, collection))) Kind.values.foreach(kind => writeErrors.labelValues(database, collection, kind.label))
    def opened(database: String): Unit = { refusals.labelValues(database); () }
  }

  object Series {
    def worker(registry: PrometheusRegistry): Series =
      new Series(registry, "kinowo_worker_mongo_write_errors_total", "kinowo_worker_mongo_unauthorized_total")
    def web(registry: PrometheusRegistry): Series =
      new Series(registry, "kinowo_web_mongo_write_errors_total", "kinowo_web_mongo_unauthorized_total")
  }
}
