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
  /** `document` stored unless its `_id` is: whether it was (false: the `_id` was taken, whatever it held). */
  def insert(document: BsonDocument): Boolean
  /** The document of `id`, deleted in the same step: two callers taking one `id` at once, one of them gets it. */
  def take(id: BsonValue): Option[BsonDocument]
  /** The documents of `ids` deleted; how many there were. */
  def delete(ids: Seq[BsonValue]): Int
  /** `field` of `id`'s document raised by `by` and its new value -- the document made, the field from 0, when there is
   *  none -- or None, nothing raised, when the field already stands at `below` or more (a ceiling). In one step: two
   *  callers raising one counter at once both count. */
  def increment(id: BsonValue, field: String, by: Long, below: Option[Long]): Option[Long]
  /** `element` added to the end of the array `field` of `id`'s document -- the document made, the array begun, when there
   *  is none -- keeping only the last `keep`. In one step: two callers appending at once both land. */
  def append(id: BsonValue, field: String, element: BsonValue, keep: Int): Unit
}

object StoreDocuments {
  /** No database: holds nothing, every write dropped (the store a process without Mongo is wired with). */
  object Absent extends StoreDocuments {
    def find(ids: Seq[BsonValue]): Seq[BsonDocument]                                       = Nil
    def scan(each: Seq[BsonDocument] => Unit): ScanOutcome                                 = ScanOutcome.complete
    def replace(documents: Seq[BsonDocument]): Unit                                       = ()
    def insert(document: BsonDocument): Boolean                                            = false
    def take(id: BsonValue): Option[BsonDocument]                                          = None
    def delete(ids: Seq[BsonValue]): Int                                                   = 0
    def increment(id: BsonValue, field: String, by: Long, below: Option[Long]): Option[Long] = None
    def append(id: BsonValue, field: String, element: BsonValue, keep: Int): Unit             = ()
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
