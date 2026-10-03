package services.retention

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{BulkWriteOptions, DeleteOneModel, Filters, Sorts}
import org.mongodb.scala.{MongoCollection, ObservableFuture, SingleObservableFuture, documentToUntypedDocument}

import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * The storage half of a retention sweep over a keyed store: which rows were last written before a
 * cutoff, and deleting a row only while it still carries the stamp that was read — so a row written
 * again after the scan is kept. The rules (which rows are orphans) belong to the sweep.
 */
trait StampedRows {
  /** Every row last written before `cutoff`, with that stamp. Throws when the scan cannot be completed. */
  def stampedBefore(cutoff: Instant): Seq[(String, Instant)]
  /** Delete each of `stamped` whose stamp is still the one given; how many were deleted. */
  def deleteIfStill(stamped: Seq[(String, Instant)]): Int
}

object StampedRows {
  /** A store that keeps nothing sweepable: a test double, or a no-op store. */
  val Unswept: StampedRows = new StampedRows {
    def stampedBefore(cutoff: Instant): Seq[(String, Instant)] = Nil
    def deleteIfStill(stamped: Seq[(String, Instant)]): Int     = 0
  }

  /** Stamps compared to the millisecond: what a Mongo `Date` keeps of an `Instant`. */
  private def sameMillis(a: Instant, b: Instant) = a.toEpochMilli == b.toEpochMilli

  /** The rows of `map`, each stamped by `stamp`: an in-memory store, or a Mongo store's mirror. */
  def inMap[A](map: ConcurrentHashMap[String, A])(stamp: A => Instant): StampedRows = new StampedRows {
    import scala.jdk.CollectionConverters._
    def stampedBefore(cutoff: Instant): Seq[(String, Instant)] =
      map.asScala.toSeq.map { case (key, row) => key -> stamp(row) }.filter(_._2.isBefore(cutoff))
    def deleteIfStill(stamped: Seq[(String, Instant)]): Int = stamped.count { case (key, at) =>
      var deleted = false
      map.computeIfPresent(key, (_, row) => if (sameMillis(stamp(row), at)) { deleted = true; null.asInstanceOf[A] } else row)
      deleted
    }
  }

  /** The documents of `collection`, stamped by their `Date` field `field`. */
  def inMongo(collection: MongoCollection[Document], field: String): StampedRows = new StampedRows {
    private val Timeout = 30.seconds
    def stampedBefore(cutoff: Instant): Seq[(String, Instant)] = {
      val found = Vector.newBuilder[(String, Instant)]
      val before = Filters.lt(field, java.util.Date.from(cutoff))
      val complete = services.movies.KeysetScan.scan[Document](
        label = s"${collection.namespace.getCollectionName} retention scan", batchSize = 2000, maxAttempts = 3,
        initialBackoff = 500.millis, keyOf = _.getString("_id"),
        fetchPage = (after, limit) => Await.result(collection
          .find(after.fold(before)(a => Filters.and(before, Filters.gt("_id", a))))
          .projection(org.mongodb.scala.model.Projections.include(field))
          .sort(Sorts.ascending("_id")).limit(limit).batchSize(tools.MongoReplies.Default).toFuture(), Timeout)
      )(_.foreach(d => Option(d.getDate(field)).foreach(at => found += d.getString("_id") -> at.toInstant)))
      complete match {
        case tools.ScanOutcome.Incomplete(cause) =>
          throw new IllegalStateException(s"${collection.namespace.getCollectionName}: retention scan incomplete", cause)
        case tools.ScanOutcome.Complete => found.result()
      }
    }
    def deleteIfStill(stamped: Seq[(String, Instant)]): Int = stamped.grouped(500).map { batch =>
      Await.result(collection.bulkWrite(batch.map { case (key, at) =>
        DeleteOneModel(Filters.and(Filters.equal("_id", key), Filters.equal(field, java.util.Date.from(at))))
      }, BulkWriteOptions().ordered(false)).toFuture(), Timeout).getDeletedCount
    }.sum
  }

  /** `durable`'s rows, deleting each from `mirror` too — a store whose reads come from a mirror. */
  def mirrored(durable: StampedRows, mirror: StampedRows): StampedRows = new StampedRows {
    def stampedBefore(cutoff: Instant): Seq[(String, Instant)] = durable.stampedBefore(cutoff)
    def deleteIfStill(stamped: Seq[(String, Instant)]): Int = { val n = durable.deleteIfStill(stamped); mirror.deleteIfStill(stamped); n }
  }
}
