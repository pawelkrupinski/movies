package services.identity
package adapters

import org.bson.{BsonDocument, BsonInt64, BsonString}
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, ReplaceOneModel, ReplaceOptions}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import services.movies.ListingKey

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * The listings whose newest version the model's input gate holds ([[ListingReadiness]], docs/design/identity-structural-fixes.md
 * fix 1), persisted so a restart neither loses one nor makes it wait for its venue's next scrape: each with the version the
 * model reads meanwhile -- its last READY one, none for a listing new to the model, which is shown on its own, Pending --
 * and when it began to wait. An INPUT, as the departed listings are ([[RetainedListingStore]]): the newest version itself
 * is the archive's, and whether it is ready is read afresh from the answers, never stored, so a take-up promotes every
 * version whose answers were filed while the worker was down, and a row outliving its promotion is released by the next
 * take-up that finds the version ready.
 */
trait PendingListingStore {
  def read(): Seq[PendingListingStore.Pending]
  /** `pending` written, each over what was held under its key; `released` (taken in, or gone) removed. */
  def update(pending: Seq[PendingListingStore.Pending], released: Iterable[ListingKey]): Unit
}

object PendingListingStore {
  /** `key` waits on its newest version's questions since `since` (epoch ms); the model reads `ready` meanwhile, or nothing. */
  final case class Pending(key: ListingKey, ready: Option[Listing], since: Long)

  final class InMemory extends PendingListingStore {
    private var held = Map.empty[ListingKey, Pending]
    def read(): Seq[Pending] = synchronized(held.values.toSeq.sortBy(p => ListingKey.serialised(p.key)))
    def update(pending: Seq[Pending], released: Iterable[ListingKey]): Unit =
      synchronized { held = held -- released ++ pending.map(p => p.key -> p) }
  }
}

/** The `identity_pending` collection: one document per listing the gate holds, `_id` its serialised key, `ready` the version
 *  the model reads meanwhile as [[ListingBson]] writes it (absent for a listing new to the model), `since` when it began to
 *  wait. */
final class MongoPendingListingStore(db: MongoDatabase) extends PendingListingStore {
  import PendingListingStore.Pending
  private val coll: MongoCollection[Document] = db.getCollection[Document](MongoPendingListingStore.Collection)
  private val Timeout = 30.seconds

  def read(): Seq[Pending] =
    Await.result(coll.find().batchSize(tools.MongoReplies.Default).toFuture(), Timeout).flatMap(d => MongoPendingListingStore.pendingOf(d.toBsonDocument))

  def update(pending: Seq[Pending], released: Iterable[ListingKey]): Unit = {
    val keys = released.map(ListingKey.serialised).toSeq
    if (keys.nonEmpty) Await.result(coll.deleteMany(Filters.in("_id", keys*)).toFuture(), Timeout)
    if (pending.nonEmpty)
      Await.result(coll.bulkWrite(pending.map(p => ReplaceOneModel(Filters.eq("_id", ListingKey.serialised(p.key)),
        MongoPendingListingStore.docOf(p), ReplaceOptions().upsert(true)))).toFuture(), Timeout)
    ()
  }
}

object MongoPendingListingStore {
  val Collection = "identity_pending"

  def docOf(pending: PendingListingStore.Pending): Document = {
    val d = new BsonDocument("_id", new BsonString(ListingKey.serialised(pending.key))).append("since", new BsonInt64(pending.since))
    pending.ready.foreach(listing => d.append("ready", ListingBson.doc(listing)))
    Document(d)
  }

  /** A document as [[docOf]] writes it, read back; None for one whose key no longer parses. */
  def pendingOf(doc: BsonDocument): Option[PendingListingStore.Pending] =
    Option(doc.get("_id")).filter(_.isString).flatMap(id => ListingKey.parse(id.asString.getValue)).map { key =>
      val ready = Option(doc.get("ready")).filter(_.isDocument).flatMap(l => Try(ListingBson.of(l.asDocument)).toOption)
      val since = Option(doc.get("since")).filter(_.isNumber).map(_.asNumber.longValue).getOrElse(0L)
      PendingListingStore.Pending(key, ready, since)
    }
}
