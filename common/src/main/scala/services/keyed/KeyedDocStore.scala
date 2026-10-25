package services.keyed

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, IndexOptions, Indexes, ReplaceOptions, Sorts}
import org.mongodb.scala.{MongoCollection, ObservableFuture, SingleObservableFuture, documentToUntypedDocument}
import play.api.Logging
import services.retention.StampedRows
import tools.ScanOutcome

import java.time.Instant
import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import scala.util.Try

/**
 * How a store's value is stored: the document's fields beside its `_id`, and back. Shared by the
 * Mongo and in-memory [[KeyedDocStore]]s, so both hold the same documents and a spec against the
 * in-memory one reads what Mongo would.
 */
trait DocCodec[T] {
  /** `value`'s fields, `_id` left out: the store writes it first. */
  def encode(value: T): Document
  /** The value of a whole stored document (its `_id` included); None for a malformed one, so one
   *  bad row cannot fail a read. */
  def decode(document: Document): Option[T]
  /** The `Date` field each write stamps, when there is one: what a TTL index expires on and a
   *  retention sweep reads ([[KeyedDocStore.retention]]). */
  def stampField: Option[String] = None
}

/**
 * One document per key (`_id`), each write replacing the last: the storage half of the small
 * bookkeeping stores (attempts, results, ledgers, memos). Pure storage — what a value means, and
 * what a failed read or write is worth, belong to the store above it. Every method but
 * [[putDetached]] blocks, and every one throws on a failed read or write.
 */
trait KeyedDocStore[T] {
  def get(key: String): Option[T]
  /** The values stored for `keys`; a key never written is absent. */
  def getAll(keys: Seq[String]): Map[String, T]
  /** Every stored value, `_id` order, handed to `each` a page at a time; Incomplete when the read
   *  stopped short — what was handed over stands, for a store that would rather have part. */
  def scan(each: Seq[(String, T)] => Unit): ScanOutcome
  def put(key: String, value: T): Unit
  /** [[put]] without waiting for it: for a store merely observing what it records. A failure goes
   *  to `onFailure`, never to the caller. */
  def putDetached(key: String, value: T)(onFailure: Throwable => Unit): Unit
  def delete(key: String): Unit
  /** The rows by their [[DocCodec.stampField]], for a retention sweep; unswept without one. */
  def retention: StampedRows

  /** Every stored value. THROWS when the read stopped short: a part is not the whole. */
  final def all(): Map[String, T] = {
    val values = Map.newBuilder[String, T]
    scan(values ++= _) match {
      case ScanOutcome.Complete          => values.result()
      case ScanOutcome.Incomplete(cause) => throw new IllegalStateException("keyed store read incomplete", cause)
    }
  }
}

object KeyedDocStore {
  private[keyed] def idOf(document: Document): Option[String] =
    document.get("_id").filter(_.isString).map(_.asString.getValue)

  /** `value` as stored under `key`: `_id` first, then the codec's fields. */
  private[keyed] def stored[T](codec: DocCodec[T], key: String, value: T): Document =
    Document("_id" -> key) ++ codec.encode(value)

  /** Stores nothing and has nothing: the store where there is no database. */
  def absent[T]: KeyedDocStore[T] = new KeyedDocStore[T] {
    def get(key: String): Option[T]                                                = None
    def getAll(keys: Seq[String]): Map[String, T]                                  = Map.empty
    def scan(each: Seq[(String, T)] => Unit): ScanOutcome                         = ScanOutcome.complete
    def put(key: String, value: T): Unit                                           = ()
    def putDetached(key: String, value: T)(onFailure: Throwable => Unit): Unit     = ()
    def delete(key: String): Unit                                                  = ()
    def retention: StampedRows                                                     = StampedRows.Unswept
  }
}

/** The documents in a map, each written to BSON bytes and read back as Mongo would ([[services.BsonRoundTrip]]):
 *  tests, and a process without Mongo. */
final class InMemoryKeyedDocStore[T](codec: DocCodec[T]) extends KeyedDocStore[T] {
  private val documents = new ConcurrentHashMap[String, Document]()

  private def decoded(key: String, document: Document) = codec.decode(document).map(key -> _)

  def get(key: String): Option[T] = Option(documents.get(key)).flatMap(codec.decode)
  def getAll(keys: Seq[String]): Map[String, T] =
    keys.distinct.flatMap(key => Option(documents.get(key)).flatMap(decoded(key, _))).toMap
  def scan(each: Seq[(String, T)] => Unit): ScanOutcome = {
    each(documents.asScala.toSeq.sortBy(_._1).flatMap(decoded))
    ScanOutcome.complete
  }
  def put(key: String, value: T): Unit = { documents.put(key, services.BsonRoundTrip(KeyedDocStore.stored(codec, key, value))); () }
  def putDetached(key: String, value: T)(onFailure: Throwable => Unit): Unit = put(key, value)
  def delete(key: String): Unit = { documents.remove(key); () }
  def retention: StampedRows = codec.stampField.fold(StampedRows.Unswept) { field =>
    // A document without a stamp is never swept: Mongo's `$lt` does not match a missing field either.
    StampedRows.inMap(documents)(_.get(field).filter(_.isDateTime).fold(Instant.MAX)(d => Instant.ofEpochMilli(d.asDateTime.getValue)))
  }
}

/**
 * The documents in `collection`: replaced whole (upserted) on each write, read by `_id`, and scanned
 * in `_id`-keyset pages — never one unbounded cursor, which at a corpus's size recurses the async
 * driver into a `StackOverflowError` ([[services.movies.KeysetScan]]). `expireAfter` keeps a TTL
 * index on the codec's stamp field.
 */
final class MongoKeyedDocStore[T](collection: MongoCollection[Document], codec: DocCodec[T], expireAfter: Option[FiniteDuration] = None)
    extends KeyedDocStore[T] with Logging {
  import MongoKeyedDocStore.*

  private val name = collection.namespace.getCollectionName

  expireAfter.foreach { ttl =>
    val field = codec.stampField.getOrElse(throw new IllegalArgumentException(s"$name: a TTL needs the codec's stamp field"))
    val init = new Thread(() => Try(Await.result(collection.createIndex(Indexes.ascending(field),
      IndexOptions().expireAfter(ttl.toSeconds, TimeUnit.SECONDS)).toFuture(), Timeout))
      .failed.foreach(e => logger.warn(s"$name TTL index not created: ${e.getMessage}")), s"$name-init")
    init.setDaemon(true); init.start()
  }

  private def decoded(document: Document) =
    for { key <- KeyedDocStore.idOf(document); value <- codec.decode(document) } yield key -> value

  def get(key: String): Option[T] =
    Await.result(collection.find(Filters.eq("_id", key)).first().headOption(), Timeout).flatMap(codec.decode)

  def getAll(keys: Seq[String]): Map[String, T] =
    keys.distinct.grouped(LookupBatch).flatMap { batch =>
      Await.result(collection.find(Filters.in("_id", batch*)).batchSize(tools.MongoReplies.Default).toFuture(), Timeout).flatMap(decoded)
    }.toMap

  def scan(each: Seq[(String, T)] => Unit): ScanOutcome =
    scanDocuments(collection, s"$name scan")(page => each(page.flatMap(decoded)))

  private def replace(key: String, value: T) =
    collection.replaceOne(Filters.eq("_id", key), KeyedDocStore.stored(codec, key, value), ReplaceOptions().upsert(true))

  def put(key: String, value: T): Unit = services.MongoWriteErrors.counted(collection) { Await.result(replace(key, value).toFuture(), Timeout); () }

  def putDetached(key: String, value: T)(onFailure: Throwable => Unit): Unit =
    Try(replace(key, value).subscribe((_: org.mongodb.scala.result.UpdateResult) => (), { (failure: Throwable) =>
      services.MongoWriteErrors.observe(collection.namespace.getDatabaseName, collection.namespace.getCollectionName, failure); onFailure(failure)
    })).failed.foreach(onFailure)

  def delete(key: String): Unit = services.MongoWriteErrors.counted(collection) { Await.result(collection.deleteOne(Filters.eq("_id", key)).toFuture(), Timeout); () }

  def retention: StampedRows = codec.stampField.fold(StampedRows.Unswept)(StampedRows.inMongo(collection, _))
}

object MongoKeyedDocStore {
  private val Timeout     = 10.seconds
  /** Keys per `_id in [...]` lookup. */
  private val LookupBatch = 500

  /** Every document of `collection` in `_id`-keyset pages of 2,000, each page retried with backoff:
   *  the way a whole small store is read, whatever it keeps in its documents. */
  def scanDocuments(collection: MongoCollection[Document], label: String)(each: Seq[Document] => Unit): ScanOutcome =
    services.movies.KeysetScan.scan[Document](
      label          = label,
      batchSize      = 2000,
      maxAttempts    = 3,
      initialBackoff = 500.millis,
      keyOf          = _.getString("_id"),
      fetchPage      = (afterId, limit) => {
        val find = afterId.fold(collection.find())(a => collection.find(Filters.gt("_id", a)))
        Await.result(find.sort(Sorts.ascending("_id")).limit(limit).toFuture(), 30.seconds)
      },
    )(each)
}
