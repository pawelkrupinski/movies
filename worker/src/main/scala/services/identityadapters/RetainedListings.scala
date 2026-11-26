package services.identityadapters

import scala.concurrent.duration.*

import models.ListingKey
import org.bson.{BsonDocument, BsonInt64, BsonString}
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.MongoDatabase
import org.mongodb.scala.model.{Filters, Indexes}
import play.api.Logging
import services.identity.*
import services.identity.measures.Listing

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
  /** The departed listings in memory, as [[MongoRetainedListingStore]] keeps them; `initial` None is a set never written. */
  final class InMemory(initial: Option[Seq[ListingRetention.Departed]] = Some(Nil), clock: java.time.Clock = InMemory.Unmoving)
      extends KeyedRetainedListingStore(new tools.InMemoryStoreDocuments(), clock) {
    initial.foreach(replace)
  }

  object InMemory {
    /** The clock of a set whose `updatedAt` nothing reads. */
    val Unmoving: java.time.Clock = java.time.Clock.fixed(java.time.Instant.EPOCH, java.time.ZoneOffset.UTC)
  }

  /** `listings` persisted before departures were dated (a recording's capture, a fixture): each [[ListingRetention.Unknown]],
   *  dated by the take-up that reads it. */
  def undated(listings: Seq[Listing]): Seq[ListingRetention.Departed] = listings.map(ListingRetention.Departed(_, ListingRetention.Unknown))

  /** The listings the identity history last saw the model hold, by key: each listing's newest entry, where it holds the
   *  listing (`after`); one the model let go has none. What the worker kept before its set was persisted is among them,
   *  each with the facts it was last taken in with: the departed listings a take-up reads when the store was never written. */
  def fromHistory(entries: Seq[HistoryEntry]): Seq[Listing] =
    entries.iterator.filter(_.kind == HistoryEntry.Listing).toSeq.groupBy(_.id).valuesIterator
      .flatMap(_.maxBy(_.at).after).flatMap(StoredListing.decoded(_, "identity history")).toSeq
      .sortBy(l => ListingKey.serialised(l.key))
}

/** The departed listings in a keyed store ([[tools.KeyedStore]]) -- in Mongo the `identity_retained` collection: one document per listing that ever departed, `_id` its serialised key (a listing
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
abstract class KeyedRetainedListingStore(documents: tools.StoreDocuments, clock: java.time.Clock) extends RetainedListingStore {
  import MongoRetainedListingStore.{Expired, Updated, Written}
  private val rows = new tools.KeyedStore(documents, KeyedRetainedListingStore.Keys, new tools.DocCodec[ListingRetention.Departed] {
    def encode(departed: ListingRetention.Departed): Document = MongoRetainedListingStore.docOf(departed, clock.millis())
    def decode(document: Document): Option[ListingRetention.Departed] = MongoRetainedListingStore.departedOf(document.toBsonDocument)
  })
  private val marker = new tools.KeyedStore(documents, tools.KeyCodec.Text, tools.NoFields)

  def read(): Option[Seq[ListingRetention.Departed]] =
    Option.when(marker.get(Written).isDefined)(rows.unmarked(Expired).map(_._2))

  def update(departed: Seq[ListingRetention.Departed], released: Iterable[ListingKey]): Unit = {
    expire(rows.mark(released, Expired, _))
    put(departed)
  }

  def replace(departed: Seq[ListingRetention.Departed]): Unit = {
    expire(rows.markAllBut(departed.map(_.listing.key), Expired, _, Seq(new BsonString(Written))))
    put(departed)
  }

  /** The rows `keys` names deleted: a departed listing's twin, never a departure ([[RetainedListingStore.delete]]), so the
   *  owner's "never deleted, marked expired" (2026-11-07) does not hold them: the owner, 2026-11-07, "delete the duplicates". */
  def delete(keys: Iterable[ListingKey]): Unit = { rows.deleteAll(keys); () }

  /** The live rows `which` picks expired: kept, no input, their screening days stripped. */
  private def expire(which: tools.DocumentEdit => Int): Unit = {
    val now = new BsonInt64(clock.millis())
    which(tools.DocumentEdit(Seq(Expired -> now, Updated -> now), Seq("listing.screenings")))
    ()
  }

  private def put(departed: Seq[ListingRetention.Departed]): Unit = {
    rows.putAll(departed.map(d => d.listing.key -> d))
    marker.put(Written, ())
  }
}

object KeyedRetainedListingStore {
  /** A listing's `_id`: its serialised key. The marker's is no key. */
  val Keys: tools.KeyCodec[ListingKey] = tools.KeyCodec.textual(ListingKey.serialised, ListingKey.parse)
}

/** The `identity_retained` collection, its index made by [[MongoRetainedListingStore.indexed]]. */
final class MongoRetainedListingStore private (documents: tools.MongoStoreDocuments, clock: java.time.Clock)
    extends KeyedRetainedListingStore(documents, clock) with Logging {
  def this(db: MongoDatabase, clock: java.time.Clock) = this(
    tools.MongoStoreDocuments.of(db.getCollection(MongoRetainedListingStore.Collection), timeout = 30.seconds), clock)

  /** [[MongoRetainedListingStore.WipeIndex]] made (idempotent): the worker's store does it at boot ([[MongoRetainedListingStore.indexed]]), before its
   *  first take-up reads the live rows, so no read scans the collection. A reader alone (the nightly recording's capture, a
   *  tool over the local mirror) makes none: it writes nothing. One that fails is logged; the reads still answer, unindexed. */
  def ensureIndex(): Unit =
    scala.util.Try(documents.createIndex(MongoRetainedListingStore.WipeIndex))
      .failed.foreach(e => logger.warn(s"identity_retained: index ${MongoRetainedListingStore.WipeIndex.toBsonDocument} not made: $e"))
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
    Option(doc.get("listing")).filter(_.isDocument).flatMap(l => StoredListing.decoded(l.asDocument, "retained listings")).map { listing =>
      val at = Option(doc.get("departedAt")).filter(_.isNumber).map(_.asNumber.longValue).getOrElse(ListingRetention.Unknown)
      ListingRetention.Departed(listing, at)
    }
}
