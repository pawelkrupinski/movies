package tools

import java.time.Instant

import scala.concurrent.duration.FiniteDuration

import org.bson.{BsonDocument, BsonValue}

/**
 * The storage boundary of a [[KeyedStore]]: documents by their `_id`, and nothing a store means by them. Mongo
 * ([[MongoStoreDocuments]]) and memory ([[InMemoryStoreDocuments]]) are its two implementations; every rule above it -- the key and
 * value codecs, what a malformed document reads as, what a counter's ceiling refuses -- is [[KeyedStore]]'s, written once.
 *
 * Each document is the WHOLE stored document, `_id` included. Every method blocks and throws on a failed read or write.
 */
trait StoreDocuments {
  /** The documents of `ids` that are stored; any number of ids (an implementation batches its own reads). */
  def find(ids: Seq[BsonValue]): Seq[BsonDocument]
  /** Every document, `_id` order, a page at a time; Incomplete when the read stopped short. */
  def scan(each: Seq[BsonDocument] => Unit): ScanOutcome
  /** Each of `documents` stored over whatever its `_id` held (an upsert): one bulk write, however many. */
  def replace(documents: Seq[BsonDocument]): Unit
  /** `document` stored over whatever its `_id` held, without waiting for it: for a store merely observing what it
   *  records. A failure goes to `onFailure`, never to the caller. */
  def replaceDetached(document: BsonDocument)(onFailure: Throwable => Unit): Unit
  /** The documents by their `Date` field `field`, for a retention sweep ([[StampedRows]]): one without a date there is
   *  never swept, as Mongo's `$lt` does not match it. */
  def stamped(field: String): StampedRows
  /** `document` stored unless its `_id` is: whether it was (false: the `_id` was taken, whatever it held). */
  def insert(document: BsonDocument): Boolean
  /** Each of `documents` stored unless its `_id` is, or (an implementation made with unique fields) another document
   *  holds the same value in one of them: how many were. Unordered -- a refused one does not stop the rest. Any other
   *  failure throws. */
  def insertAll(documents: Seq[BsonDocument]): Int
  /** The document of `id`, deleted in the same step: two callers taking one `id` at once, one of them gets it. */
  def take(id: BsonValue): Option[BsonDocument]
  /** The documents of `ids` deleted; how many there were. */
  def delete(ids: Seq[BsonValue]): Int
  /** `field` of `id`'s document raised by `by` and its new value -- the document made, the field from 0, when there is
   *  none -- or None, nothing raised, when the field already stands at `below` or more (a ceiling). In one step: two
   *  callers raising one counter at once both count. */
  def increment(id: BsonValue, field: String, by: Long, below: Option[Long]): Option[Long]
  /** `field` of `id`'s document raised to `atLeast` unless it already stands there or higher -- never lowered; the
   *  document made, the field set, when there is none. In one step (Mongo's `$max`): two callers raising one field at
   *  once leave the higher of the two, never their sum. */
  def raise(id: BsonValue, field: String, atLeast: Long): Unit
  /** `element` added to the end of the array `field` of `id`'s document -- the document made, the array begun, when there
   *  is none -- keeping only the last `keep`. In one step: two callers appending at once both land. */
  def append(id: BsonValue, field: String, element: BsonValue, keep: Int): Unit
  /** Every document whose `field` is absent or null, `_id` order, a page at a time (Mongo reads it by an index on `field`
   *  when there is one, never every document). */
  def scanAbsent(field: String, each: Seq[BsonDocument] => Unit): ScanOutcome
  /** Of the documents whose `field` is absent or null, those `selection` names edited by `edit`: how many. In one step
   *  per document: a document edited is no longer absent in `field` if `edit` sets it. */
  def editAbsent(field: String, selection: IdSelection, edit: DocumentEdit): Int
}

/** Which `_id`s an edit reaches. */
sealed trait IdSelection
object IdSelection {
  /** Exactly these. */
  final case class Only(ids: Seq[BsonValue]) extends IdSelection
  /** Every `_id` but these. */
  final case class AllBut(ids: Seq[BsonValue]) extends IdSelection
}

/** Fields set, then fields removed (a dotted path reaches into a sub-document), on every document an edit reaches. */
final case class DocumentEdit(set: Seq[(String, BsonValue)], unset: Seq[String])

object StoreDocuments {
  /** No database: holds nothing, every write dropped (the store a process without Mongo is wired with). */
  object Absent extends StoreDocuments {
    def find(ids: Seq[BsonValue]): Seq[BsonDocument]                                       = Nil
    def scan(each: Seq[BsonDocument] => Unit): ScanOutcome                                 = ScanOutcome.complete
    def replace(documents: Seq[BsonDocument]): Unit                                       = ()
    def replaceDetached(document: BsonDocument)(onFailure: Throwable => Unit): Unit         = ()
    def stamped(field: String): StampedRows                                                = StampedRows.Unswept
    def insert(document: BsonDocument): Boolean                                            = false
    def insertAll(documents: Seq[BsonDocument]): Int                                       = 0
    def take(id: BsonValue): Option[BsonDocument]                                          = None
    def delete(ids: Seq[BsonValue]): Int                                                   = 0
    def increment(id: BsonValue, field: String, by: Long, below: Option[Long]): Option[Long] = None
    def raise(id: BsonValue, field: String, atLeast: Long): Unit                            = ()
    def append(id: BsonValue, field: String, element: BsonValue, keep: Int): Unit             = ()
    def scanAbsent(field: String, each: Seq[BsonDocument] => Unit): ScanOutcome               = ScanOutcome.complete
    def editAbsent(field: String, selection: IdSelection, edit: DocumentEdit): Int            = 0
  }
}

/**
 * A document lives `after` past the date in its `field`, then is DELETED: by Mongo's TTL monitor (an index on `field`,
 * which runs about once a minute, so a document may outlive its expiry by that much), and in memory at the first call
 * whose clock reads past it. Neither hides a document before it is deleted: a caller that needs an exact cutoff (an auth
 * code redeemable for two minutes) reads the stamp itself. A document without the field, or with no date in it, never
 * expires -- as Mongo's monitor skips one.
 */
final case class DocumentExpiry(field: String, after: FiniteDuration) {
  /** When `document` expires; None when it never does. */
  def at(document: BsonDocument): Option[Instant] =
    Option(document.get(field)).filter(_.isDateTime).map(d => Instant.ofEpochMilli(d.asDateTime.getValue).plusMillis(after.toMillis))

  /** Whether `document` is past its expiry at `now`. */
  def expired(document: BsonDocument, now: Instant): Boolean = at(document).exists(!_.isAfter(now))
}
