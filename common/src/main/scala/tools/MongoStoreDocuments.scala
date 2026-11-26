package tools

import java.util.concurrent.TimeUnit

import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.util.Try

import com.mongodb.MongoWriteException
import org.bson.{BsonDocument, BsonValue}
import org.mongodb.scala.model.{BulkWriteOptions, Filters, FindOneAndUpdateOptions, IndexOptions, Indexes, ReplaceOneModel,
  ReplaceOptions, ReturnDocument, Sorts, UpdateOptions, Updates}
import org.mongodb.scala.{MongoCollection, ObservableFuture, SingleObservableFuture}
import play.api.Logging

/**
 * [[StoreDocuments]] in a Mongo collection. Reads by `_id` go in batches of 500; a scan is `_id`-keyset paged
 * ([[tools.KeysetScan]]), never one unbounded cursor; a write is counted by [[tools.MongoWriteErrors]] when it fails.
 * `expiry` keeps a TTL index on its field, made off the constructing thread (boot never waits on an index; the store
 * works without it, the index only sweeps). `timeout` bounds each round trip.
 */
final class MongoStoreDocuments(collection: MongoCollection[BsonDocument], expiry: Option[DocumentExpiry] = None,
                           timeout: FiniteDuration = MongoStoreDocuments.Timeout) extends StoreDocuments with Logging {
  import MongoStoreDocuments.*

  private val name = collection.namespace.getCollectionName

  expiry.foreach { e =>
    tools.OffThreadInit.start(s"$name-init") {
      Try(Await.result(collection.createIndex(Indexes.ascending(e.field),
        IndexOptions().expireAfter(e.after.toSeconds, TimeUnit.SECONDS)).toFuture(), timeout))
        .failed.foreach(failure => logger.warn(s"$name TTL index not created: ${failure.getMessage}"))
    }
  }

  private def await[A](future: scala.concurrent.Future[A]): A = Await.result(future, timeout)
  private def written[A](body: => A): A = tools.MongoWriteErrors.counted(collection)(body)
  private def byId(id: BsonValue) = Filters.eq("_id", id)

  def find(ids: Seq[BsonValue]): Seq[BsonDocument] =
    ids.distinct.grouped(LookupBatch).toSeq.flatMap(batch =>
      await(collection.find(Filters.in("_id", batch*)).batchSize(tools.MongoReplies.Default).toFuture()))

  def scan(each: Seq[BsonDocument] => Unit): ScanOutcome =
    tools.KeysetScan.scan[BsonDocument](
      label          = s"$name scan",
      batchSize      = ScanPage,
      maxAttempts    = 3,
      initialBackoff = 500.millis,
      keyOf          = d => KeyCodec.text(d.get("_id")),
      fetchPage      = (after, limit) => {
        val find = after.fold(collection.find())(a => collection.find(Filters.gt("_id", KeyCodec.value(a))))
        Await.result(find.sort(Sorts.ascending("_id")).limit(limit).toFuture(), ScanPageTimeout)
      })(each)

  def replace(documents: Seq[BsonDocument]): Unit = documents.grouped(LookupBatch).foreach { batch =>
    written(await(collection.bulkWrite(batch.map(d => ReplaceOneModel(byId(d.get("_id")), d, ReplaceOptions().upsert(true))),
      BulkWriteOptions().ordered(false)).toFuture()))
  }

  def insert(document: BsonDocument): Boolean =
    try { written(await(collection.insertOne(document).toFuture())); true }
    catch { case duplicate: MongoWriteException if tools.MongoErrors.isDuplicateKey(duplicate) => false }

  def take(id: BsonValue): Option[BsonDocument] = written(await(collection.findOneAndDelete(byId(id)).toFutureOption()))

  def delete(ids: Seq[BsonValue]): Int =
    ids.distinct.grouped(LookupBatch).map(batch => written(await(collection.deleteMany(Filters.in("_id", batch*)).toFuture())).getDeletedCount.toInt).sum

  // With a ceiling, the document is made first (at 0, unless it is there) and raised by a conditional update that never
  // upserts: an upsert whose filter also reads the field meets a document already at the ceiling as a duplicate `_id`,
  // indistinguishable from a failure. Two makers at once clash on the `_id` alone, which leaves it there either way.
  def increment(id: BsonValue, field: String, by: Long, below: Option[Long]): Option[Long] = written {
    val after = FindOneAndUpdateOptions().returnDocument(ReturnDocument.AFTER)
    val raised = below match {
      case None          => await(collection.findOneAndUpdate(byId(id), Updates.inc(field, by), after.upsert(true)).toFutureOption())
      case Some(ceiling) =>
        made(id, field)
        await(collection.findOneAndUpdate(Filters.and(byId(id), Filters.lt(field, ceiling)), Updates.inc(field, by), after).toFutureOption())
    }
    raised.map(_.get(field).asNumber.longValue)
  }

  private def made(id: BsonValue, field: String): Unit =
    try { await(collection.updateOne(byId(id), Updates.setOnInsert(field, 0L), UpdateOptions().upsert(true)).toFuture()); () }
    catch { case e: com.mongodb.MongoException if e.getCode == DuplicateKey => () }
}

object MongoStoreDocuments {
  /** Most stores' round-trip bound. */
  val Timeout: FiniteDuration = 10.seconds
  /** `_id`s per `_id in [...]` lookup, and documents per bulk write. */
  private val LookupBatch     = 500
  private val ScanPage        = 2000
  private val ScanPageTimeout = 30.seconds
  private val DuplicateKey    = 11000

  /** `collection` read and written as plain BSON documents. */
  def of(collection: MongoCollection[?], expiry: Option[DocumentExpiry] = None, timeout: FiniteDuration = Timeout): MongoStoreDocuments =
    new MongoStoreDocuments(collection.withDocumentClass[BsonDocument](), expiry, timeout)
}
