package services.closure

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, ReplaceOptions}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture, documentToUntypedDocument}

import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * The venues [[ClosureSweep]] has confirmed closed, and when: so each closure pages and
 * asks for its retirement ONCE, and one that shows life again before its PR merges is
 * withdrawn. Pure storage — the sweep makes every decision.
 */
trait ClosureLedger {
  def confirmed(): Map[String, Instant]
  def confirm(cinema: String, at: Instant): Unit
  def withdraw(cinema: String): Unit
}

final class InMemoryClosureLedger extends ClosureLedger {
  private val byCinema = new ConcurrentHashMap[String, Instant]()
  def confirmed(): Map[String, Instant]           = byCinema.asScala.toMap
  def confirm(cinema: String, at: Instant): Unit  = { byCinema.put(cinema, at); () }
  def withdraw(cinema: String): Unit              = { byCinema.remove(cinema); () }
}

/** The `venue_closures` collection in the country's database: `{_id: displayName,
 *  confirmedAt}`. A handful of rows, so every read is a whole-collection find. */
final class MongoClosureLedger(db: MongoDatabase) extends ClosureLedger {
  private val coll: MongoCollection[Document] = db.getCollection[Document](MongoClosureLedger.Collection)

  def confirmed(): Map[String, Instant] =
    Await.result(coll.find().batchSize(tools.MongoReplies.Default).toFuture(), 10.seconds).map { d =>
      d.getString("_id") -> Instant.ofEpochMilli(d.getDate("confirmedAt").getTime)
    }.toMap

  def confirm(cinema: String, at: Instant): Unit = {
    Await.result(coll.replaceOne(Filters.eq("_id", cinema),
      Document("_id" -> cinema, "confirmedAt" -> new java.util.Date(at.toEpochMilli)), ReplaceOptions().upsert(true)).toFuture(), 10.seconds)
    ()
  }

  def withdraw(cinema: String): Unit = {
    Await.result(coll.deleteOne(Filters.eq("_id", cinema)).toFuture(), 10.seconds)
    ()
  }
}

object MongoClosureLedger {
  val Collection = "venue_closures"
}
