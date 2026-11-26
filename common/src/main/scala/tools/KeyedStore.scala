package tools

import org.bson.{BsonDocument, BsonValue}
import org.mongodb.scala.bson.collection.immutable.Document

/** Reading a store's values by key. */
trait StoreReader[K, V] {
  def get(key: K): Option[V]
  /** The values stored for `keys`; a key never written is absent. */
  def getAll(keys: Seq[K]): Map[K, V]
  /** Every value, `_id` order, a page at a time; Incomplete when the read stopped short -- what was handed over stands. */
  def scan(each: Seq[(K, V)] => Unit): ScanOutcome

  /** Every value, `_id` order. THROWS when the read stopped short: a part is not the whole. */
  final def all(): Seq[(K, V)] = StoreReader.whole(scan)
}

object StoreReader {
  /** What a paged `scan` hands over, whole; THROWS when it stopped short. */
  private[tools] def whole[K, V](scan: (Seq[(K, V)] => Unit) => ScanOutcome): Seq[(K, V)] = {
    val values = Vector.newBuilder[(K, V)]
    scan(values ++= _) match {
      case ScanOutcome.Complete          => values.result()
      case ScanOutcome.Incomplete(cause) => throw new IllegalStateException("keyed store read incomplete", cause)
    }
  }
}

/** Writing one value at a time, each over what its key held. */
trait StoreWriter[K, V] {
  def put(key: K, value: V): Unit
  /** Whether `key` held a value. */
  def delete(key: K): Boolean
}

/** Values written once and taken once: an insert that refuses a held key (a claim, a code minted), and a read that
 *  deletes what it reads in the same step (a code spent). Both settle a race between callers in the store. */
trait StoreOnce[K, V] {
  /** `value` stored under `key` unless `key` holds one: whether it was. */
  def insert(key: K, value: V): Boolean
  /** `key`'s value, deleted as it is read: of two callers, one gets it. */
  def take(key: K): Option[V]
  /** Each of `values` stored unless its key holds one (or the documents refuse it: a unique field), the rest unaffected:
   *  how many landed. */
  def insertAll(values: Seq[(K, V)]): Int
}

/** Many values in one write. */
trait StoreBulk[K, V] {
  /** Each value stored over what its key held, in one bulk write however many. */
  def putAll(values: Seq[(K, V)]): Unit
  /** The values of `keys` deleted; how many there were. */
  def deleteAll(keys: Iterable[K]): Int
}

/** Named counters in a key's document, each raised in one step however many callers race. */
trait StoreCounters[K] {
  /** `field` of `key` raised by `by`, and its new value; None, nothing raised, when it stands at `below` or more. */
  def increment(key: K, field: String, by: Long = 1, below: Option[Long] = None): Option[Long]
  /** `field` of `key` as it stands; 0 for a counter never raised. */
  def counted(key: K, field: String): Long
}

/** Elements added to the end of an array field of a key's document, the array kept to its last few. */
trait StoreCappedLists[K] {
  /** `element` appended to `field` of `key`'s document (made when there is none), keeping the last `keep`, in one step. */
  def append(key: K, field: String, element: BsonValue, keep: Int): Unit
}

/** Rows marked instead of deleted: a field that is absent on a live row and set on a marked one, read only on the live. */
trait StoreMarks[K, V] {
  /** Every live row (the `marker` field absent), `_id` order, a page at a time; Incomplete when the read stopped short. */
  def scanUnmarked(marker: String)(each: Seq[(K, V)] => Unit): ScanOutcome
  /** The live rows of `keys` given `edit` (which sets `marker`): how many. */
  def mark(keys: Iterable[K], marker: String, edit: DocumentEdit): Int
  /** Every live row but those of `keys` -- and the documents of `otherIds` (a marker document) -- given `edit`: how many. */
  def markAllBut(keys: Iterable[K], marker: String, edit: DocumentEdit, otherIds: Seq[BsonValue] = Nil): Int

  /** Every live row, `_id` order. THROWS when the read stopped short. */
  final def unmarked(marker: String): Seq[(K, V)] = StoreReader.whole(scanUnmarked(marker)(_))
}

/**
 * The one keyed store: values of `V` under keys of `K` in `documents` -- `_id` the key as `keys` writes it, the other
 * fields the value as `values` writes it -- each of the segregated interfaces above, so a store above it takes only the
 * ones it uses. Every rule is here, once, for Mongo and memory alike ([[StoreDocuments]] is only where they are kept): a
 * document whose `_id` or value does not decode is no value (one bad row never fails a read); a counter is a number
 * field of the key's document, 0 until raised.
 */
final class KeyedStore[K, V](documents: StoreDocuments, keys: KeyCodec[K], values: DocCodec[V])
    extends StoreReader[K, V] with StoreWriter[K, V] with StoreOnce[K, V] with StoreBulk[K, V] with StoreCounters[K] with StoreCappedLists[K] with StoreMarks[K, V] {

  /** `value` as stored under `key`: `_id` first, then the codec's fields. */
  private def stored(key: K, value: V): BsonDocument = {
    val document = new BsonDocument("_id", keys.id(key))
    values.encode(value).toBsonDocument.forEach((field, v) => if (field != "_id") { document.put(field, v); () })
    document
  }

  private def decoded(document: BsonDocument): Option[(K, V)] = for {
    key   <- Option(document.get("_id")).flatMap(keys.key)
    value <- values.decode(Document(document))
  } yield key -> value

  def get(key: K): Option[V]                               = documents.find(Seq(keys.id(key))).flatMap(decoded).headOption.map(_._2)
  def getAll(wanted: Seq[K]): Map[K, V]                    = documents.find(wanted.map(keys.id)).flatMap(decoded).toMap
  def scan(each: Seq[(K, V)] => Unit): ScanOutcome         = documents.scan(page => each(page.flatMap(decoded)))
  def put(key: K, value: V): Unit                          = documents.replace(Seq(stored(key, value)))
  def delete(key: K): Boolean                              = documents.delete(Seq(keys.id(key))) > 0
  def insert(key: K, value: V): Boolean                    = documents.insert(stored(key, value))
  def insertAll(inserted: Seq[(K, V)]): Int                = documents.insertAll(inserted.map(stored.tupled))
  def take(key: K): Option[V]                              = documents.take(keys.id(key)).flatMap(decoded).map(_._2)
  def putAll(written: Seq[(K, V)]): Unit                   = if (written.nonEmpty) documents.replace(written.map(stored.tupled))
  def deleteAll(gone: Iterable[K]): Int                    = if (gone.isEmpty) 0 else documents.delete(gone.map(keys.id).toSeq)

  def increment(key: K, field: String, by: Long = 1, below: Option[Long] = None): Option[Long] =
    documents.increment(keys.id(key), field, by, below)
  def append(key: K, field: String, element: BsonValue, keep: Int): Unit = documents.append(keys.id(key), field, element, keep)
  def scanUnmarked(marker: String)(each: Seq[(K, V)] => Unit): ScanOutcome = documents.scanAbsent(marker, page => each(page.flatMap(decoded)))
  def mark(gone: Iterable[K], marker: String, edit: DocumentEdit): Int =
    if (gone.isEmpty) 0 else documents.editAbsent(marker, IdSelection.Only(gone.map(keys.id).toSeq), edit)
  def markAllBut(kept: Iterable[K], marker: String, edit: DocumentEdit, otherIds: Seq[BsonValue] = Nil): Int =
    documents.editAbsent(marker, IdSelection.AllBut(kept.map(keys.id).toSeq ++ otherIds), edit)
  def counted(key: K, field: String): Long =
    documents.find(Seq(keys.id(key))).headOption.flatMap(d => Option(d.get(field))).filter(_.isNumber).fold(0L)(_.asNumber.longValue)
}

object KeyedStore {
  /** A store of counters alone, by text key: documents with nothing but their `_id` and the counters raised in them. */
  def counters(documents: StoreDocuments): KeyedStore[String, Unit] = new KeyedStore(documents, KeyCodec.Text, NoFields)
}

/** The value of a document with no fields beside its `_id` (a marker, a counter's holder). */
object NoFields extends DocCodec[Unit] {
  def encode(unit: Unit): Document                = Document()
  def decode(document: Document): Option[Unit]    = Some(())
}
