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
final class InMemoryStoreDocuments private (expiring: Option[(DocumentExpiry, java.time.Clock)], unique: Seq[String]) extends StoreDocuments {
  /** Documents that never expire. */
  def this() = this(None, Nil)

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

  def scan(each: Seq[BsonDocument] => Unit): ScanOutcome = scanWhere(_ => true)(each)

  def scanAbsent(field: String, each: Seq[BsonDocument] => Unit): ScanOutcome = scanWhere(InMemoryStoreDocuments.absent(_, field))(each)

  private def scanWhere(which: BsonDocument => Boolean)(each: Seq[BsonDocument] => Unit): ScanOutcome = {
    swept()
    each(held.values.asScala.toSeq.map(read).filter(which))
    ScanOutcome.complete
  }

  def editAbsent(field: String, selection: IdSelection, edit: DocumentEdit): Int = {
    swept()
    val chosen: BsonValue => Boolean = selection match {
      case IdSelection.Only(ids)   => ids.toSet
      case IdSelection.AllBut(ids) => id => !ids.contains(id)
    }
    held.keySet.asScala.toSeq.filter(chosen).count { id =>
      var edited = false
      held.computeIfPresent(id, (_, bytes) => {
        val document = read(bytes)
        edited = InMemoryStoreDocuments.absent(document, field)
        if (edited) tools.BsonRoundTrip.bytes(InMemoryStoreDocuments.edited(document, edit)) else bytes
      })
      edited
    }
  }

  def replace(documents: Seq[BsonDocument]): Unit = {
    swept()
    documents.foreach(d => held.put(idOf(d), tools.BsonRoundTrip.bytes(d)))
  }

  def replaceDetached(document: BsonDocument)(onFailure: Throwable => Unit): Unit =
    scala.util.Try(replace(Seq(document))).failed.foreach(onFailure)

  def stamped(field: String): StampedRows = new StampedRows {
    private def stampOf(bytes: RawBsonDocument): Option[java.time.Instant] =
      Option(read(bytes).get(field)).filter(_.isDateTime).map(d => java.time.Instant.ofEpochMilli(d.asDateTime.getValue))
    def stampedBefore(cutoff: java.time.Instant): Seq[(String, java.time.Instant)] = {
      swept()
      held.entrySet.asScala.toSeq.flatMap(entry => KeyCodec.Text.key(entry.getKey).zip(stampOf(entry.getValue))).filter(_._2.isBefore(cutoff))
    }
    def deleteIfStill(stamped: Seq[(String, java.time.Instant)]): Int = stamped.count { case (key, at) =>
      var deleted = false
      held.computeIfPresent(KeyCodec.Text.id(key), (_, bytes) =>
        if (stampOf(bytes).exists(_.toEpochMilli == at.toEpochMilli)) { deleted = true; null } else bytes)
      deleted
    }
  }

  /** Documents held under another `_id` with the same value as `document` in a unique field. */
  private def clashes(document: BsonDocument): Boolean =
    unique.exists(field => Option(document.get(field)).exists(v => held.values.asScala.exists(other => Option(read(other).get(field)).contains(v))))

  def insertAll(documents: Seq[BsonDocument]): Int = synchronized {
    swept()
    documents.count(d => !clashes(d) && held.putIfAbsent(idOf(d), tools.BsonRoundTrip.bytes(d)) == null)
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

  /** As Mongo's `$max`: a field missing (or no number) is set; one at `atLeast` or more is left as it is, its type too. */
  def raise(id: BsonValue, field: String, atLeast: Long): Unit = {
    swept()
    held.compute(id, (_, was) => {
      val document = Option(was).map(read).getOrElse(new BsonDocument("_id", id))
      if (Option(document.get(field)).filter(_.isNumber).exists(_.asNumber.longValue >= atLeast)) was
      else tools.BsonRoundTrip.bytes(document.append(field, new BsonInt64(atLeast)))
    })
    ()
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
  /** Whether `field` is missing or null in `document`, as Mongo's `{field: null}` matches. */
  private def absent(document: BsonDocument, field: String): Boolean = Option(document.get(field)).forall(_.isNull)

  /** `document` with `edit` applied: its fields set, then its (dotted) paths removed. */
  private def edited(document: BsonDocument, edit: DocumentEdit): BsonDocument = {
    edit.set.foreach((field, value) => document.put(field, value))
    edit.unset.foreach { path =>
      val steps  = path.split('.').toList
      val parent = steps.init.foldLeft(Option[BsonDocument](document))((at, step) => at.flatMap(d => Option(d.get(step)).filter(_.isDocument).map(_.asDocument)))
      parent.foreach(_.remove(steps.last))
    }
    document
  }

  /** Documents of which no two hold the same value in any of `fields` -- a unique index's refusal in [[StoreDocuments.insertAll]]. */
  def uniqueBy(fields: String*): InMemoryStoreDocuments = new InMemoryStoreDocuments(None, fields)

  /** Documents that expire by `expiry` on `clock`. */
  def expiring(expiry: DocumentExpiry, clock: java.time.Clock): InMemoryStoreDocuments = new InMemoryStoreDocuments(Some(expiry -> clock), Nil)

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
