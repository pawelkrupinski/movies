package services.identity
package adapters

import org.bson.{BsonDocument, BsonInt64, BsonString}
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, ReplaceOneModel, ReplaceOptions}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import play.api.Logging
import services.movies.ListingKey

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * Every listing that left the country's billing, with its last-seen facts and when it left ([[ListingRetention.Departed]]),
 * persisted: the INPUT the identity model reads its kept listings from on every resolve ([[ListingRetention.retainedAt]]:
 * those a present listing of the country relates to by title). Never a kept/dropped decision made at a leave: a listing
 * goes only when it is billed again or past [[ListingRetention.MaxAge]]. The worker's take-up reads it instead of losing it
 * at every restart, and the nightly recording captures it beside the corpus (`tools.ProdRetained`), so a replay's model
 * input is the listings billed AND those kept of these, as production's is.
 */
trait RetainedListingStore {
  /** The departed listings, as last written; None when nothing was ever written (the take-up reads them from the
   *  history). One written before departures were dated reads [[ListingRetention.Unknown]] as its departure. */
  def read(): Option[Seq[ListingRetention.Departed]]
  /** `departed` written, each over what was held under its key; `released` (billed again, or aged out) removed. */
  def update(departed: Seq[ListingRetention.Departed], released: Iterable[ListingKey]): Unit
  /** Exactly `departed`, nothing else. */
  def replace(departed: Seq[ListingRetention.Departed]): Unit
}

object RetainedListingStore {
  final class InMemory(initial: Option[Seq[ListingRetention.Departed]] = Some(Nil)) extends RetainedListingStore {
    private var held: Option[Map[ListingKey, ListingRetention.Departed]] = initial.map(_.map(d => d.listing.key -> d).toMap)
    def read(): Option[Seq[ListingRetention.Departed]] =
      synchronized(held.map(_.values.toSeq.sortBy(d => ListingKey.serialised(d.listing.key))))
    def update(departed: Seq[ListingRetention.Departed], released: Iterable[ListingKey]): Unit =
      synchronized { held = Some(held.getOrElse(Map.empty) -- released ++ departed.map(d => d.listing.key -> d)) }
    def replace(departed: Seq[ListingRetention.Departed]): Unit = synchronized { held = Some(departed.map(d => d.listing.key -> d).toMap) }
  }

  /** `listings` persisted before departures were dated (a recording's capture, a fixture): each [[ListingRetention.Unknown]],
   *  dated by the take-up that reads it. */
  def undated(listings: Seq[Listing]): Seq[ListingRetention.Departed] = listings.map(ListingRetention.Departed(_, ListingRetention.Unknown))

  /** The listings the identity history last saw the model hold, by key: each listing's newest entry, where it holds the
   *  listing (`after`); one the model let go has none. What the worker kept before its set was persisted is among them,
   *  each with the facts it was last taken in with: the departed listings a take-up reads when the store was never written. */
  def fromHistory(entries: Seq[HistoryEntry]): Seq[Listing] =
    entries.iterator.filter(_.kind == HistoryEntry.Listing).toSeq.groupBy(_.id).valuesIterator
      .flatMap(_.maxBy(_.at).after).flatMap(doc => Try(ListingBson.of(doc)).toOption).toSeq
      .sortBy(l => ListingKey.serialised(l.key))
}

/** The `identity_retained` collection: one document per departed listing, `_id` its serialised key, `listing` it as
 *  [[ListingBson]] writes it (its days included), `departedAt` when it left (absent on one written before 2026-11-03:
 *  [[ListingRetention.Unknown]], dated and written back by the take-up), and one marker that the set was written at all. */
final class MongoRetainedListingStore(db: MongoDatabase) extends RetainedListingStore with Logging {
  private val coll: MongoCollection[Document] = db.getCollection[Document](MongoRetainedListingStore.Collection)
  private val Timeout = 30.seconds
  import MongoRetainedListingStore.Written

  def read(): Option[Seq[ListingRetention.Departed]] = {
    val docs = Await.result(coll.find().batchSize(tools.MongoReplies.Default).toFuture(), Timeout)
    Option.when(docs.exists(_.get("_id").exists(_.asString.getValue == Written)))(docs.flatMap(d => MongoRetainedListingStore.departedOf(d.toBsonDocument)))
  }

  def update(departed: Seq[ListingRetention.Departed], released: Iterable[ListingKey]): Unit = {
    val keys = released.map(ListingKey.serialised).toSeq
    if (keys.nonEmpty) Await.result(coll.deleteMany(Filters.in("_id", keys*)).toFuture(), Timeout)
    put(departed)
  }

  def replace(departed: Seq[ListingRetention.Departed]): Unit = {
    val keys = departed.map(d => ListingKey.serialised(d.listing.key)) :+ Written
    Await.result(coll.deleteMany(Filters.nin("_id", keys*)).toFuture(), Timeout)
    put(departed)
  }

  private def put(departed: Seq[ListingRetention.Departed]): Unit = {
    val writes = departed.map(d => ReplaceOneModel(Filters.eq("_id", ListingKey.serialised(d.listing.key)), MongoRetainedListingStore.docOf(d),
      ReplaceOptions().upsert(true))) :+
      ReplaceOneModel(Filters.eq("_id", Written), Document("_id" -> Written), ReplaceOptions().upsert(true))
    Await.result(coll.bulkWrite(writes).toFuture(), Timeout); ()
  }
}

object MongoRetainedListingStore {
  val Collection = "identity_retained"
  /** The marker's `_id`: no serialised key is a bare word. */
  val Written = "written"

  def docOf(departed: ListingRetention.Departed): Document = {
    val d = new BsonDocument("_id", new BsonString(ListingKey.serialised(departed.listing.key)))
      .append("listing", ListingBson.doc(departed.listing)).append("departedAt", new BsonInt64(departed.at))
    Document(d)
  }

  /** A document as [[docOf]] writes it -- or as written before departures were dated -- read back; None for the marker. */
  def departedOf(doc: BsonDocument): Option[ListingRetention.Departed] =
    Option(doc.get("listing")).filter(_.isDocument).flatMap(l => Try(ListingBson.of(l.asDocument)).toOption).map { listing =>
      val at = Option(doc.get("departedAt")).filter(_.isNumber).map(_.asNumber.longValue).getOrElse(ListingRetention.Unknown)
      ListingRetention.Departed(listing, at)
    }
}
