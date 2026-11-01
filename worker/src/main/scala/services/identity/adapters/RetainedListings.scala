package services.identity
package adapters

import org.bson.{BsonDocument, BsonString}
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, ReplaceOneModel, ReplaceOptions}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import play.api.Logging
import services.movies.ListingKey

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Try

/**
 * The listings the identity model keeps for their facts alone ([[ListingRetention]]: no venue bills them now, a present
 * listing of the country bills their title), persisted: the worker's take-up resumes them ([[ListingRetention.restored]])
 * instead of losing them at every restart, and the nightly recording captures them beside the corpus
 * (`tools.ProdRetained`), so a replay's model input is the listings billed AND these, as production's is.
 */
trait RetainedListingStore {
  /** The listings kept, as last written; None when nothing was ever written (the take-up starts them from the history). */
  def read(): Option[Seq[Listing]]
  /** `kept` kept now, each over what was kept under its key; `released` kept no longer. */
  def update(kept: Seq[Listing], released: Iterable[ListingKey]): Unit
  /** Exactly `kept`, nothing else. */
  def replace(kept: Seq[Listing]): Unit
}

object RetainedListingStore {
  final class InMemory(initial: Option[Seq[Listing]] = Some(Nil)) extends RetainedListingStore {
    private var held: Option[Map[ListingKey, Listing]] = initial.map(_.map(l => l.key -> l).toMap)
    def read(): Option[Seq[Listing]] = synchronized(held.map(_.values.toSeq.sortBy(l => ListingKey.serialised(l.key))))
    def update(kept: Seq[Listing], released: Iterable[ListingKey]): Unit =
      synchronized { held = Some(held.getOrElse(Map.empty) -- released ++ kept.map(l => l.key -> l)) }
    def replace(kept: Seq[Listing]): Unit = synchronized { held = Some(kept.map(l => l.key -> l).toMap) }
  }

  /** The listings the identity history last saw the model hold, by key: each listing's newest entry, where it holds the
   *  listing (`after`); one the model let go has none. What the worker kept before its set was persisted is among them,
   *  each with the facts it was last taken in with; [[ListingRetention.restored]] keeps those a present listing bills. */
  def fromHistory(entries: Seq[HistoryEntry]): Seq[Listing] =
    entries.iterator.filter(_.kind == HistoryEntry.Listing).toSeq.groupBy(_.id).valuesIterator
      .flatMap(_.maxBy(_.at).after).flatMap(doc => Try(ListingBson.of(doc)).toOption).toSeq
      .sortBy(l => ListingKey.serialised(l.key))
}

/** The `identity_retained` collection: one document per kept listing, `_id` its serialised key, `listing` it as
 *  [[ListingBson]] writes it (its days included), and one marker that the set was written at all. */
final class MongoRetainedListingStore(db: MongoDatabase) extends RetainedListingStore with Logging {
  private val coll: MongoCollection[Document] = db.getCollection[Document](MongoRetainedListingStore.Collection)
  private val Timeout = 30.seconds
  import MongoRetainedListingStore.Written

  def read(): Option[Seq[Listing]] = {
    val docs = Await.result(coll.find().batchSize(tools.MongoReplies.Default).toFuture(), Timeout)
    Option.when(docs.exists(_.get("_id").exists(_.asString.getValue == Written)))(
      docs.flatMap(d => Option(d.toBsonDocument.get("listing")).filter(_.isDocument).flatMap(l => Try(ListingBson.of(l.asDocument)).toOption)))
  }

  def update(kept: Seq[Listing], released: Iterable[ListingKey]): Unit = {
    val keys = released.map(ListingKey.serialised).toSeq
    if (keys.nonEmpty) Await.result(coll.deleteMany(Filters.in("_id", keys*)).toFuture(), Timeout)
    put(kept)
  }

  def replace(kept: Seq[Listing]): Unit = {
    val keys = kept.map(l => ListingKey.serialised(l.key)) :+ Written
    Await.result(coll.deleteMany(Filters.nin("_id", keys*)).toFuture(), Timeout)
    put(kept)
  }

  private def put(kept: Seq[Listing]): Unit = {
    val writes = kept.map(l => ReplaceOneModel(Filters.eq("_id", ListingKey.serialised(l.key)), MongoRetainedListingStore.docOf(l),
      ReplaceOptions().upsert(true))) :+
      ReplaceOneModel(Filters.eq("_id", Written), Document("_id" -> Written), ReplaceOptions().upsert(true))
    Await.result(coll.bulkWrite(writes).toFuture(), Timeout); ()
  }
}

object MongoRetainedListingStore {
  val Collection = "identity_retained"
  /** The marker's `_id`: no serialised key is a bare word. */
  val Written = "written"

  def docOf(listing: Listing): Document = {
    val d = new BsonDocument("_id", new BsonString(ListingKey.serialised(listing.key))).append("listing", ListingBson.doc(listing))
    Document(d)
  }
}
