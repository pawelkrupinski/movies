package services.config

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, ReplaceOptions}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import play.api.Logging

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/** The set of admin config overrides, cached in-memory so [[lookup]] is a cheap
 *  per-read call (it backs [[tools.Env]]'s override source). The cache is the
 *  single source the running process consults; [[refresh]] reloads it so a flip
 *  made on another process (web → worker) is picked up within the refresh tick.
 *
 *  Real impl: Mongo `env_overrides`. Test impl: an in-memory map with the same
 *  caching semantics — so the business logic above the trait (the service's
 *  publish/merge) is exercised identically. */
trait EnvOverrideStore {
  /** The override value for `key`, or None — read live on every Env resolution. */
  def lookup(key: String): Option[String]
  /** All current overrides (for the admin merge). */
  def all(): Map[String, String]
  /** Set/replace an override and make it visible to [[lookup]] immediately. */
  def set(key: String, value: String): Unit
  /** Remove an override ("reset to default / env"), visible immediately. */
  def clear(key: String): Unit
  /** Reload the cache from the backing store (no-op for the in-memory impl). */
  def refresh(): Unit
}

/** In-memory override store — caching is trivial since there's one process. */
class InMemoryEnvOverrideStore extends EnvOverrideStore {
  private val map = new java.util.concurrent.ConcurrentHashMap[String, String]()
  def lookup(key: String): Option[String] = Option(map.get(key))
  def all(): Map[String, String] = { import scala.jdk.CollectionConverters._; map.asScala.toMap }
  def set(key: String, value: String): Unit = { map.put(key, value); () }
  def clear(key: String): Unit = { map.remove(key); () }
  def refresh(): Unit = ()
}

/** Mongo-backed override store. Document shape: `{ _id: key, value: String }`.
 *  All writes are best-effort with a 10s timeout (a flip is operator-driven, not
 *  on a hot path); the local cache is updated synchronously on set/clear so the
 *  writing process sees its own flip without waiting for the next refresh. */
class MongoEnvOverrideStore(db: MongoDatabase) extends EnvOverrideStore with Logging {
  private val coll: MongoCollection[Document] = db.getCollection[Document]("env_overrides")

  @volatile private var cache: Map[String, String] = Map.empty
  refresh()

  def lookup(key: String): Option[String] = cache.get(key)
  def all(): Map[String, String] = cache

  def refresh(): Unit =
    Try(Await.result(coll.find().toFuture(), 10.seconds)).toOption.foreach { docs =>
      cache = docs.flatMap { d =>
        for { k <- str(d, "_id"); v <- str(d, "value") } yield k -> v
      }.toMap
    }

  private def str(d: Document, key: String): Option[String] =
    d.get(key).filter(_.isString).map(_.asString().getValue)

  def set(key: String, value: String): Unit = {
    Try(Await.result(
      coll.replaceOne(Filters.eq("_id", key),
        Document("_id" -> key, "value" -> value), ReplaceOptions().upsert(true)).toFuture(),
      10.seconds))
      .recover { case e => logger.warn(s"EnvOverrideStore.set($key) failed: ${e.getMessage}") }
    cache = cache + (key -> value)
  }

  def clear(key: String): Unit = {
    Try(Await.result(coll.deleteOne(Filters.eq("_id", key)).toFuture(), 10.seconds))
      .recover { case e => logger.warn(s"EnvOverrideStore.clear($key) failed: ${e.getMessage}") }
    cache = cache - key
  }
}
