package services.config

import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.Try

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, ReplaceOptions}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import play.api.Logging

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
  /** Set/replace an override and make it visible to [[lookup]] immediately: whether it was stored. One that could
   *  not be stored is not applied here either, so no process runs a flip the others never see. */
  def set(key: String, value: String): Boolean
  /** Remove an override ("reset to default / env"), visible immediately: whether the removal was stored, as for
   *  [[set]]. */
  def clear(key: String): Boolean
  /** Reload the cache from the backing store (no-op for the in-memory impl): whether it could be read — the cache is
   *  kept as it was when not. */
  def refresh(): Boolean
}

/** In-memory override store — caching is trivial since there's one process. Each override is written as
 *  [[MongoEnvOverrideStore]] stores it to BSON and read back. */
class InMemoryEnvOverrideStore extends EnvOverrideStore {
  private val map = new java.util.concurrent.ConcurrentHashMap[String, String]()
  def lookup(key: String): Option[String] = Option(map.get(key))
  def all(): Map[String, String] = { import scala.jdk.CollectionConverters._; map.asScala.toMap }
  def set(key: String, value: String): Boolean = {
    val (k, v) = MongoEnvOverrideStore.decode(tools.BsonRoundTrip(MongoEnvOverrideStore.encode(key, value)))
      .getOrElse(throw new IllegalStateException(s"override $key does not read back as stored"))
    map.put(k, v); true
  }
  def clear(key: String): Boolean = { map.remove(key); true }
  def refresh(): Boolean = true
}

/** Mongo-backed override store. Document shape: `{ _id: key, value: String }`.
 *  Takes the shared web/worker database (None → disabled, e.g. in tests/local
 *  without Mongo, where the in-memory impl is used instead). Each write waits up to
 *  10s (a flip is operator-driven, not on a hot path); the local cache is updated
 *  synchronously on a stored set/clear so the writing process sees its own flip
 *  without waiting for the next refresh, and not at all on a failed one: the page
 *  showed a flip as applied that no other process ever read, and the next refresh
 *  quietly reverted it here too. */
class MongoEnvOverrideStore(sharedDb: Option[MongoDatabase]) extends EnvOverrideStore with Logging {
  import MongoEnvOverrideStore.*
  private val coll: Option[MongoCollection[Document]] =
    sharedDb.map(_.getCollection[Document]("env_overrides"))

  @volatile private var cache: Map[String, String] = Map.empty
  locally { refresh(); () }

  def lookup(key: String): Option[String] = cache.get(key)
  def all(): Map[String, String] = cache

  def refresh(): Boolean = coll.forall { c =>
    val read = Try(Await.result(c.find().batchSize(tools.MongoReplies.Default).toFuture(), 10.seconds))
    read.fold(
      e => logger.warn(s"EnvOverrideStore.refresh failed, the overrides kept as last read: ${e.getMessage}"),
      docs => cache = docs.flatMap(decode).toMap)
    read.isSuccess
  }

  def set(key: String, value: String): Boolean =
    stored(s"set($key)") { c =>
      c.replaceOne(Filters.eq("_id", key), encode(key, value), ReplaceOptions().upsert(true)).toFuture()
    } && { cache = cache + (key -> value); true }

  def clear(key: String): Boolean =
    stored(s"clear($key)")(_.deleteOne(Filters.eq("_id", key)).toFuture()) && { cache = cache - key; true }

  /** Whether `write` reached the collection (trivially, with no collection to write to). */
  private def stored(what: String)(write: MongoCollection[Document] => scala.concurrent.Future[?]): Boolean =
    coll.forall { c =>
      Try(Await.result(write(c), 10.seconds)).fold(
        e => { logger.warn(s"EnvOverrideStore.$what failed, not applied: ${e.getMessage}"); false },
        _ => true)
    }
}

object MongoEnvOverrideStore {
  /** An override as stored: `{ _id: key, value }`. */
  private[config] def encode(key: String, value: String): Document = Document("_id" -> key, "value" -> value)

  /** A stored override; None for a malformed document. */
  private[config] def decode(d: Document): Option[(String, String)] =
    for { k <- str(d, "_id"); v <- str(d, "value") } yield k -> v

  private def str(d: Document, key: String): Option[String] =
    d.get(key).filter(_.isString).map(_.asString().getValue)
}
