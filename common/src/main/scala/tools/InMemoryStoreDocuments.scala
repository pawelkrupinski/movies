package tools

import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.ConcurrentSkipListMap

import scala.jdk.CollectionConverters.*

import org.bson.{BsonArray, BsonDocument, BsonInt64, BsonValue, RawBsonDocument}

/**
 * [[StoreDocuments]] in a map, for tests and a process without Mongo: each kept as its BSON bytes, written as the driver
 * would write it ([[tools.BsonRoundTrip]]: a document Mongo would refuse throws here too) and every read decoded afresh,
 * so a reader's copy is its own. In `_id` order as Mongo sorts `_id`s (a number before a string, strings by their UTF-8
 * bytes). An expiring one ([[InMemoryStoreDocuments.expiring]]) applies its expiry on the injected clock ([[DocumentExpiry]]):
 * at every call, what is past it is gone.
 */
final class InMemoryStoreDocuments private (expiring: Option[(DocumentExpiry, java.time.Clock)]) extends StoreDocuments {
  /** Documents that never expire. */
  def this() = this(None)

  private val held = new ConcurrentSkipListMap[BsonValue, RawBsonDocument](InMemoryStoreDocuments.IdOrder)

  private def read(bytes: RawBsonDocument): BsonDocument = tools.BsonRoundTrip.read(bytes)
  private def idOf(document: BsonDocument): BsonValue =
    Option(document.get("_id")).getOrElse(throw new IllegalArgumentException("a stored document needs an _id"))

  /** What is past the expiry deleted, as Mongo's monitor would have. */
  private def swept(): Unit = expiring.foreach { (e, clock) =>
    val now = clock.instant()
    held.entrySet.removeIf(entry => e.expired(read(entry.getValue), now))
  }

  def find(ids: Seq[BsonValue]): Seq[BsonDocument] = { swept(); ids.distinct.flatMap(id => Option(held.get(id)).map(read)) }

  def scan(each: Seq[BsonDocument] => Unit): ScanOutcome = {
    swept()
    each(held.values.asScala.toSeq.map(read))
    ScanOutcome.complete
  }

  def replace(documents: Seq[BsonDocument]): Unit = {
    swept()
    documents.foreach(d => held.put(idOf(d), tools.BsonRoundTrip.bytes(d)))
  }

  def insert(document: BsonDocument): Boolean = { swept(); held.putIfAbsent(idOf(document), tools.BsonRoundTrip.bytes(document)) == null }

  def take(id: BsonValue): Option[BsonDocument] = { swept(); Option(held.remove(id)).map(read) }

  def delete(ids: Seq[BsonValue]): Int = { swept(); ids.distinct.count(id => held.remove(id) != null) }

  def increment(id: BsonValue, field: String, by: Long, below: Option[Long]): Option[Long] = {
    swept()
    var raised = Option.empty[Long]
    // The skip-list map may call the function again under contention: each call decides afresh.
    held.compute(id, (_, was) => {
      raised = None
      val document = Option(was).map(read).getOrElse(new BsonDocument("_id", id))
      val current  = Option(document.get(field)).filter(_.isNumber).fold(0L)(_.asNumber.longValue)
      if (below.exists(current >= _)) was
      else {
        raised = Some(current + by)
        tools.BsonRoundTrip.bytes(document.append(field, new BsonInt64(current + by)))
      }
    })
    raised
  }

  def append(id: BsonValue, field: String, element: BsonValue, keep: Int): Unit = {
    swept()
    held.compute(id, (_, was) => {
      val document = Option(was).map(read).getOrElse(new BsonDocument("_id", id))
      val array    = Option(document.get(field)).filter(_.isArray).fold(Seq.empty[BsonValue])(_.asArray.getValues.asScala.toSeq)
      tools.BsonRoundTrip.bytes(document.append(field, new BsonArray((array :+ element).takeRight(keep).asJava)))
    })
    ()
  }
}

object InMemoryStoreDocuments {
  /** Documents that expire by `expiry` on `clock`. */
  def expiring(expiry: DocumentExpiry, clock: java.time.Clock): InMemoryStoreDocuments = new InMemoryStoreDocuments(Some(expiry -> clock))

  /** Mongo's order of `_id`s: numbers by value, before strings, strings by their UTF-8 bytes; any other type after, by
   *  its extended JSON (no store here keys by one). */
  private[tools] val IdOrder: java.util.Comparator[BsonValue] = (a: BsonValue, b: BsonValue) => {
    def rank(v: BsonValue) = if (v.isNumber) 0 else if (v.isString) 1 else 2
    (rank(a), rank(b)) match {
      case (0, 0)                   => BigDecimal(a.asNumber.decimal128Value.bigDecimalValue).compare(BigDecimal(b.asNumber.decimal128Value.bigDecimalValue))
      case (1, 1)                   => java.util.Arrays.compareUnsigned(a.asString.getValue.getBytes(UTF_8), b.asString.getValue.getBytes(UTF_8))
      case (ra, rb) if ra != rb     => Integer.compare(ra, rb)
      case _                        => KeyCodec.text(a).compareTo(KeyCodec.text(b))
    }
  }
}
