package tools

import org.mongodb.scala.bson.BsonDateTime
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.Filters
import org.mongodb.scala.{MongoDatabase, SingleObservableFuture}

import java.time.{Clock, Instant, ZoneOffset}
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * The server's TTL monitor, for a spec writing to a TTL-indexed collection.
 *
 * The monitor deletes a document once its expiry field is past by the SERVER's clock, on a pass
 * every 60 seconds — whatever clock the spec handed the code that stamped the field. A document
 * stamped from a fixed clock months in the past is already expired when it lands, so it survives
 * only until the next pass: a spec reading it back passes or fails on whether a pass fell between
 * the write and the read — how `ShadowIdentityReaperIntegrationSpec` failed "no run persisted"
 * under load.
 *
 * [[serverClock]] is the clock to stamp such documents by — the one the monitor reads, asked of the
 * server rather than this JVM — and [[sweep]] runs a monitor pass NOW, so a spec states that what it
 * wrote outlives one instead of racing it.
 */
object MongoTtlMonitor {

  /** The server's own time (`hello.localTime`), as a fixed clock. */
  def serverClock(database: MongoDatabase): Clock = Clock.fixed(serverNow(database), ZoneOffset.UTC)

  /** One monitor pass over `collection`'s `field` (an `expireAfterSeconds = 0` index): every document
   *  whose expiry the server's clock has reached is deleted. The count deleted. */
  def sweep(database: MongoDatabase, collection: String, field: String): Long = {
    val now = serverNow(database)
    Await.result(database.getCollection(collection).deleteMany(Filters.lte(field, BsonDateTime(now.toEpochMilli))).toFuture(), 30.seconds).getDeletedCount
  }

  private def serverNow(database: MongoDatabase): Instant = {
    val reply = Await.result(database.runCommand(Document("hello" -> 1)).toFuture(), 30.seconds)
    val local = reply.get("localTime").filter(_.isDateTime)
      .getOrElse(throw new IllegalStateException(s"hello answered without a localTime: ${reply.toJson()}"))
    Instant.ofEpochMilli(local.asDateTime.getValue)
  }
}
