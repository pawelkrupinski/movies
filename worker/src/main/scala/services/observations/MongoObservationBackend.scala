package services.observations

import com.mongodb.WriteConcern
import org.mongodb.scala.bson.{BsonBinary, BsonBoolean, BsonDateTime, BsonString, BsonValue, Document}
import org.mongodb.scala.model.{Filters, Indexes, Sorts, Updates}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import services.{MongoTtlIndex, TtlIndexMismatches}
import services.movies.KeysetScan

import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * One kind of observation in a SHADOW collection — `obs_listings` or `obs_lookups` — that nothing
 * serving reads. A document per observation: the key, its scope, the content hash, the gzipped
 * payload, `observedAt` / `lastSeenAt` / `expireAt` and whether it is the key's current one.
 *
 * Expiry is Mongo's TTL monitor on `expireAt` with `expireAfterSeconds = 0` (reconciled by
 * `MongoTtlIndex`): the WINDOW lives in
 * the data, stamped by [[ObservationStore]], so a change to the retention rule never needs the
 * index rebuilt (an index created with a TTL keeps it for good — the `cinema_city_detail` lesson).
 *
 * Every call is awaited and lets a failure propagate: the capture decorators decide that a lost
 * observation must not fail the call it observes; a failed read here is never an empty answer.
 */
final class MongoObservationBackend(db: MongoDatabase, collection: String, ttlMismatches: TtlIndexMismatches)
  extends ObservationBackend {

  private val Timeout = 30.seconds

  private lazy val coll: MongoCollection[Document] = {
    val c = db.getCollection[Document](collection).withWriteConcern(WriteConcern.W1.withJournal(false))
    Await.result(c.createIndex(Indexes.ascending("key", "current")).toFuture(), Timeout)
    Await.result(c.createIndex(Indexes.ascending("current", "key")).toFuture(), Timeout)
    MongoTtlIndex.reconcile(c, "expireAt", 0L, "ObservationStore", ttlMismatches)
    c
  }

  private def currentOf(key: String) = Filters.and(Filters.equal("key", key), Filters.equal("current", true))

  def current(key: String): Option[StoredObservation] =
    Await.result(coll.find(currentOf(key)).headOption(), Timeout).map(decode)

  def history(key: String): Seq[StoredObservation] =
    Await.result(coll.find(Filters.equal("key", key)).sort(Sorts.ascending("observedAt")).toFuture(), Timeout).map(decode)

  def allCurrent(): Seq[StoredObservation] = {
    val out = Seq.newBuilder[StoredObservation]
    var failure: Option[Throwable] = None
    val complete = KeysetScan.scan[StoredObservation](
      label = s"$collection current scan", batchSize = 1000, maxAttempts = 3, initialBackoff = 1.second,
      keyOf = _.key,
      fetchPage = (after, limit) => Await.result(
        coll.find(Filters.and((Filters.equal("current", true) +: after.map(Filters.gt("key", _)).toSeq)*))
          .sort(Sorts.ascending("key")).limit(limit).toFuture(), 60.seconds).map(decode),
      onIncomplete = e => failure = Some(e))(out ++= _)
    if (!complete) throw new IllegalStateException(s"$collection: incomplete read of current observations", failure.orNull)
    out.result()
  }

  def insert(o: StoredObservation): Unit =
    Await.result(coll.insertOne(Document(
      "key" -> BsonString(o.key), "scope" -> BsonString(o.scope), "hash" -> BsonString(o.hash),
      "payload" -> BsonBinary(o.payload), "observedAt" -> date(o.observedAt), "lastSeenAt" -> date(o.lastSeenAt),
      "expireAt" -> date(o.expireAt), "current" -> BsonBoolean(o.current))).toFuture(), Timeout)

  def retire(key: String, expireAt: Instant): Unit =
    Await.result(coll.updateMany(currentOf(key),
      Updates.combine(Updates.set("current", false), Updates.set("expireAt", date(expireAt)))).toFuture(), Timeout)

  def renew(key: String, lastSeenAt: Option[Instant], expireAt: Instant): Unit =
    Await.result(coll.updateOne(currentOf(key), Updates.combine(
      (Updates.set("expireAt", date(expireAt)) +: lastSeenAt.map(at => Updates.set("lastSeenAt", date(at))).toSeq)*)).toFuture(), Timeout)

  private def date(i: Instant) = BsonDateTime(i.toEpochMilli)

  private def decode(d: Document): StoredObservation = StoredObservation(
    key        = field[BsonString](d, "key").getValue,
    scope      = field[BsonString](d, "scope").getValue,
    hash       = field[BsonString](d, "hash").getValue,
    payload    = field[BsonBinary](d, "payload").getData,
    observedAt = Instant.ofEpochMilli(field[BsonDateTime](d, "observedAt").getValue),
    lastSeenAt = Instant.ofEpochMilli(field[BsonDateTime](d, "lastSeenAt").getValue),
    expireAt   = Instant.ofEpochMilli(field[BsonDateTime](d, "expireAt").getValue),
    current    = field[BsonBoolean](d, "current").getValue)

  /** A field every observation document carries — its absence is corruption, never a default. */
  private def field[T <: BsonValue](d: Document, name: String)(using ct: scala.reflect.ClassTag[T]): T =
    d.get[T](name).getOrElse(throw new IllegalStateException(s"$collection: `$name` missing on ${d.get("_id")}"))
}

object MongoObservationBackend {

  /** The store over a country's database: its two shadow collections. */
  def store(db: MongoDatabase, clock: java.time.Clock, ttlMismatches: TtlIndexMismatches): ObservationStore =
    new ObservationStore(new MongoObservationBackend(db, ObservationStore.ListingsCollection, ttlMismatches),
      new MongoObservationBackend(db, ObservationStore.LookupsCollection, ttlMismatches), clock)
}
