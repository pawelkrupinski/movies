package services.review

import org.bson.conversions.Bson
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, Sorts}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture}
import tools.KeysetScan

import scala.concurrent.Await
import scala.concurrent.duration._

/** What every review read stands on: one country's database on the local read-mirror ([[MongoReviewSource]]), each
 *  call bounded, a whole-collection read keyset-paged. */
trait MongoReviewReads {
  protected def db: MongoDatabase
  private val Timeout = 30.seconds
  protected def collection(name: String): MongoCollection[Document] = db.getCollection[Document](name)
  protected def await[A](f: scala.concurrent.Future[A]): A = Await.result(f, Timeout)

  /** A keyset scan of `name` under `filter`, projected — every page bounded, retried, `_id`-ordered. */
  protected def scan(name: String, filter: Bson, projection: Bson, batch: Int)(onBatch: Seq[Document] => Unit): Unit = {
    val outcome = KeysetScan.scan[Document](s"review $name", batch, maxAttempts = 3, initialBackoff = 200.millis,
      keyOf = _.get("_id").map(ReviewBson.idText).getOrElse(""),
      fetchPage = (after, limit) => await(collection(name)
        .find(after.fold(filter)(id => Filters.and(filter, Filters.gt("_id", id)))).projection(projection)
        .sort(Sorts.ascending("_id")).limit(limit).batchSize(limit).toFuture()))(onBatch)
    if (!outcome.isComplete) throw new IllegalStateException(s"review: $name read incomplete")
  }
}
