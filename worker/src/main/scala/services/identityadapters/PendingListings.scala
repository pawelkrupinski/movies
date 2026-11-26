package services.identityadapters

import scala.concurrent.duration.*

import models.ListingKey
import org.bson.{BsonDocument, BsonInt64, BsonString}
import org.mongodb.scala.MongoDatabase
import org.mongodb.scala.bson.collection.immutable.Document
import services.identity.*
import services.identity.measures.Listing

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

  /** The held listings in memory, as [[MongoPendingListingStore]] keeps them. */
  final class InMemory extends KeyedPendingListingStore(new tools.InMemoryStoreDocuments())
}

/** The held listings in a keyed store, one document per listing: `_id` its serialised key, `ready` the version the model
 *  reads meanwhile as [[ListingBson]] writes it (absent for a listing new to the model), `since` when it began to wait.
 *  Read in key order; an update drops the released, then writes the pending, in one bulk write. */
abstract class KeyedPendingListingStore(documents: tools.StoreDocuments) extends PendingListingStore {
  import PendingListingStore.Pending
  private val held = new tools.KeyedStore(documents, KeyedPendingListingStore.Keys, KeyedPendingListingStore.Codec)

  def read(): Seq[Pending] = held.all().map(_._2)

  def update(pending: Seq[Pending], released: Iterable[ListingKey]): Unit = {
    held.deleteAll(released)
    held.putAll(pending.map(p => p.key -> p))
  }
}

object KeyedPendingListingStore {
  /** A listing's `_id`: its serialised key. */
  val Keys: tools.KeyCodec[ListingKey] = tools.KeyCodec.textual(ListingKey.serialised, ListingKey.parse)

  val Codec: tools.DocCodec[PendingListingStore.Pending] = new tools.DocCodec[PendingListingStore.Pending] {
    def encode(pending: PendingListingStore.Pending): Document = MongoPendingListingStore.docOf(pending)
    def decode(document: Document): Option[PendingListingStore.Pending] = MongoPendingListingStore.pendingOf(document.toBsonDocument)
  }
}

/** The `identity_pending` collection. */
final class MongoPendingListingStore(db: MongoDatabase) extends KeyedPendingListingStore(
  tools.MongoStoreDocuments.of(db.getCollection(MongoPendingListingStore.Collection), timeout = 30.seconds))

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
      val ready = Option(doc.get("ready")).filter(_.isDocument).flatMap(l => StoredListing.decoded(l.asDocument, "pending listings"))
      val since = Option(doc.get("since")).filter(_.isNumber).map(_.asNumber.longValue).getOrElse(0L)
      PendingListingStore.Pending(key, ready, since)
    }
}
