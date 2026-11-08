package services.identity
package adapters

import org.bson.{BsonDocument, BsonInt64, BsonString}
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, Indexes, ReplaceOneModel, ReplaceOptions, Updates}
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
 * is an input until it is billed again or past the retention window ([[ListingRetention.live]]); its row then stays,
 * marked expired ([[MongoRetainedListingStore]]). The worker's take-up reads it instead of losing it
 * at every restart, and the nightly recording captures it beside the corpus (`tools.ProdRetained`), so a replay's model
 * input is the listings billed AND those kept of these, as production's is.
 */
trait RetainedListingStore {
  /** The departed listings, as last written; None when nothing was ever written (the take-up reads them from the
   *  history). One written before departures were dated reads [[ListingRetention.Unknown]] as its departure. */
  def read(): Option[Seq[ListingRetention.Departed]]
  /** `departed` written, each over what was held under its key; `released` (billed again, or aged out) no input
   *  again: never read back (the Mongo store keeps their rows, expired). */
  def update(departed: Seq[ListingRetention.Departed], released: Iterable[ListingKey]): Unit
  /** Exactly `departed` read back, nothing else (the Mongo store expires every other row). */
  def replace(departed: Seq[ListingRetention.Departed]): Unit
  /** `keys` removed for good, not expired: departed rows whose own listing is billed again under another key
   *  ([[ListingRetention.returnedUnderAnotherKey]]) -- duplicates, never a departure to keep a record of. */
  def delete(keys: Iterable[ListingKey]): Unit
}

object RetainedListingStore {
  final class InMemory(initial: Option[Seq[ListingRetention.Departed]] = Some(Nil)) extends RetainedListingStore {
    private var held: Option[Map[ListingKey, ListingRetention.Departed]] = initial.map(_.map(d => d.listing.key -> d).toMap)
    def read(): Option[Seq[ListingRetention.Departed]] =
      synchronized(held.map(_.values.toSeq.sortBy(d => ListingKey.serialised(d.listing.key))))
    def update(departed: Seq[ListingRetention.Departed], released: Iterable[ListingKey]): Unit =
      synchronized { held = Some(held.getOrElse(Map.empty) -- released ++ departed.map(d => d.listing.key -> d)) }
    def replace(departed: Seq[ListingRetention.Departed]): Unit = synchronized { held = Some(departed.map(d => d.listing.key -> d).toMap) }
    def delete(keys: Iterable[ListingKey]): Unit = synchronized { held = held.map(_ -- keys) }
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

/** The `identity_retained` collection: one document per listing that ever departed, `_id` its serialised key (a listing
 *  that departs again REPLACES its row, never adds one), `listing` it as [[ListingBson]] writes it (its days included),
 *  `departedAt` when it left (absent on one written before 2026-11-03: [[ListingRetention.Unknown]], dated and written back
 *  by the take-up), `updatedAt` when the row was last written (the injected clock), and one marker that the set was
 *  written at all.
 *
 *  A row is never deleted (the owner, 2026-11-07: "write and update all listings forever to mongo db (but not memory)
 *  ... marked as deleted when they would be deleted"). Where it would have been -- its listing billed again, or past the
 *  retention window -- it is EXPIRED: `expiredAt` and `updatedAt` set, its screening days stripped ("still wipe
 *  showtimes": the listing's details stay, title, facts, venue, links, timestamps). An expired row is no input and is
 *  never read into memory: [[read]] asks for the live rows alone, `expiredAt: null`, through the index on
 *  (`expiredAt`, `updatedAt`), which also makes a future wipe by age an indexed range ([[MongoRetainedListingStore.expiredBefore]]). */
final class MongoRetainedListingStore(db: MongoDatabase, clock: java.time.Clock) extends RetainedListingStore with Logging {
  import MongoRetainedListingStore.{Expired, Live, Updated, WipeIndex, Written}
  private val coll: MongoCollection[Document] = db.getCollection[Document](MongoRetainedListingStore.Collection)
  private val Timeout = 30.seconds

  /** [[WipeIndex]] made (idempotent): the worker's store does it at boot ([[MongoRetainedListingStore.indexed]]), before its
   *  first take-up reads the live rows, so no read scans the collection. A reader alone (the nightly recording's capture, a
   *  tool over the local mirror) makes none: it writes nothing. One that fails is logged; the reads still answer, unindexed. */
  def ensureIndex(): Unit =
    scala.util.Try(Await.result(coll.createIndex(WipeIndex).toFuture(), Timeout))
      .failed.foreach(e => logger.warn(s"identity_retained: index ${WipeIndex.toBsonDocument} not made: $e"))

  def read(): Option[Seq[ListingRetention.Departed]] = {
    val docs = Await.result(coll.find(Live).batchSize(tools.MongoReplies.Default).toFuture(), Timeout)
    Option.when(docs.exists(_.get("_id").exists(_.asString.getValue == Written)))(docs.flatMap(d => MongoRetainedListingStore.departedOf(d.toBsonDocument)))
  }

  def update(departed: Seq[ListingRetention.Departed], released: Iterable[ListingKey]): Unit = {
    val keys = released.map(ListingKey.serialised).toSeq
    if (keys.nonEmpty) expire(Filters.in("_id", keys*))
    put(departed)
  }

  def replace(departed: Seq[ListingRetention.Departed]): Unit = {
    val keys = departed.map(d => ListingKey.serialised(d.listing.key)) :+ Written
    expire(Filters.nin("_id", keys*))
    put(departed)
  }

  /** The rows `keys` names deleted: a departed listing's twin, never a departure ([[RetainedListingStore.delete]]), so the
   *  owner's "never deleted, marked expired" (2026-11-07) does not hold them: the owner, 2026-11-07, "delete the duplicates". */
  def delete(keys: Iterable[ListingKey]): Unit = {
    val ids = keys.map(ListingKey.serialised).toSeq
    if (ids.nonEmpty) { Await.result(coll.deleteMany(Filters.in("_id", ids*)).toFuture(), Timeout); () }
  }

  /** The live rows `which` names expired: kept, no input, their screening days stripped. */
  private def expire(which: org.bson.conversions.Bson): Unit = {
    val now = clock.millis()
    Await.result(coll.updateMany(Filters.and(which, Live), Updates.combine(Updates.set(Expired, now), Updates.set(Updated, now),
      Updates.unset("listing.screenings"))).toFuture(), Timeout); ()
  }

  private def put(departed: Seq[ListingRetention.Departed]): Unit = {
    val now    = clock.millis()
    val writes = departed.map(d => ReplaceOneModel(Filters.eq("_id", ListingKey.serialised(d.listing.key)), MongoRetainedListingStore.docOf(d, now),
      ReplaceOptions().upsert(true))) :+
      ReplaceOneModel(Filters.eq("_id", Written), Document("_id" -> Written), ReplaceOptions().upsert(true))
    Await.result(coll.bulkWrite(writes).toFuture(), Timeout); ()
  }
}

object MongoRetainedListingStore {
  val Collection = "identity_retained"
  /** The worker's store: [[MongoRetainedListingStore.ensureIndex]] run as it is made, at boot. */
  def indexed(db: MongoDatabase, clock: java.time.Clock): MongoRetainedListingStore = {
    val store = new MongoRetainedListingStore(db, clock)
    store.ensureIndex()
    store
  }
  /** The marker's `_id`: no serialised key is a bare word. */
  val Written = "written"
  /** When a row stopped being an input (epoch millis): absent on a live one. */
  val Expired = "expiredAt"
  /** When a row was last written (epoch millis, the store's clock): on every insert, replace and expiry. */
  val Updated = "updatedAt"
  /** The live rows, the marker among them: what the store reads. `null` matches a missing field, and uses [[WipeIndex]]. */
  val Live: org.bson.conversions.Bson = Filters.eq(Expired, null)
  /** The expired rows last written before `at`: what a future wipe by age deletes. `expiredAt > 0`, not `$exists` (which
   *  the index answers only by reading every key), so both fields are ranges on [[WipeIndex]]. */
  def expiredBefore(at: Long): org.bson.conversions.Bson = Filters.and(Filters.gt(Expired, 0L), Filters.lt(Updated, at))
  /** The one index: the live rows by its prefix, and a future wipe of the expired rows by age as a range. */
  val WipeIndex: org.bson.conversions.Bson = Indexes.ascending(Expired, Updated)

  def docOf(departed: ListingRetention.Departed, updatedAt: Long): Document = {
    val d = new BsonDocument("_id", new BsonString(ListingKey.serialised(departed.listing.key)))
      .append("listing", ListingBson.doc(departed.listing)).append("departedAt", new BsonInt64(departed.at))
      .append(Updated, new BsonInt64(updatedAt))
    Document(d)
  }

  /** A document as [[docOf]] writes it -- or as written before departures were dated -- read back; None for the marker. */
  def departedOf(doc: BsonDocument): Option[ListingRetention.Departed] =
    Option(doc.get("listing")).filter(_.isDocument).flatMap(l => Try(ListingBson.of(l.asDocument)).toOption).map { listing =>
      val at = Option(doc.get("departedAt")).filter(_.isNumber).map(_.asNumber.longValue).getOrElse(ListingRetention.Unknown)
      ListingRetention.Departed(listing, at)
    }
}
