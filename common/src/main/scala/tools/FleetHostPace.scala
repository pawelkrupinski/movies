package tools

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.BsonDateTime
import org.mongodb.scala.model.{Filters, FindOneAndUpdateOptions, ReturnDocument}
import org.mongodb.scala.{MongoCollection, MongoDatabase, SingleObservableFuture}

import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * One outbound pace per host for the WHOLE FLEET — every country's worker against the same origin from the same egress —
 * or, in memory, one process: each host's next free slot, taken one at a time. A slot within `horizon` is the caller's
 * (reserved, to wait for); one further off is not taken, and its time is handed back, so the caller can come back then
 * rather than hold a thread waiting ([[PacedHttpFetch]]). An infinite `horizon` takes every slot, however far off.
 */
trait FleetHostPace {
  /** `Right(slot)`: this request's reserved slot, at most `horizon` from `now`; `Left(next)`: the host's next slot is
   *  further off — nothing reserved. */
  def take(host: String, interval: FiniteDuration, horizon: Duration, now: Instant): Either[Instant, Instant]
}

object FleetHostPace {
  /** The slot a request takes when the next free one is `next`: no earlier than `now`. */
  private[tools] def slotOf(next: Option[Instant], now: Instant): Instant = next.filter(_.isAfter(now)).getOrElse(now)
}

/** The slots in this process — a test's, or a worker with no fleet database (it paces itself alone) — each host's next
 *  slot kept as [[MongoFleetHostPace]] would hold it, written as its document to BSON and read back. */
final class InMemoryFleetHostPace extends FleetHostPace {
  private val next = new ConcurrentHashMap[String, Instant]()
  private def stored(host: String, at: Instant) =
    MongoFleetHostPace.nextOf(services.BsonRoundTrip(MongoFleetHostPace.encode(host, at))).getOrElse(at)
  def take(host: String, interval: FiniteDuration, horizon: Duration, now: Instant): Either[Instant, Instant] = synchronized {
    val slot = FleetHostPace.slotOf(Option(next.get(host)), now)
    if (horizon.isFinite && slot.isAfter(now.plusMillis(horizon.toMillis))) Left(slot)
    else { next.put(host, stored(host, slot.plusMillis(interval.toMillis))); Right(slot) }
  }
}

/** The slots in the fleet database's `fleet_host_pace` (one document per host: `next`, its next free slot), taken by one
 *  atomic update each — every worker sees every other's. */
final class MongoFleetHostPace(db: MongoDatabase) extends FleetHostPace {
  private val Timeout = 10.seconds
  private lazy val collection: MongoCollection[Document] = db.getCollection[Document](MongoFleetHostPace.Collection)

  def take(host: String, interval: FiniteDuration, horizon: Duration, now: Instant): Either[Instant, Instant] = {
    // Taken only while the next slot is within the horizon: `next` moves on from max(next, now) by `interval`.
    val update = Seq(org.bson.BsonDocument.parse(
      s"""{"$$set": {"next": {"$$add": [{"$$max": ["$$next", {"$$toDate": ${now.toEpochMilli}}]}, ${interval.toMillis}]}}}"""))
    val within =
      if (!horizon.isFinite) Filters.eq("_id", host)
      else Filters.and(Filters.eq("_id", host), Filters.or(Filters.exists("next", false),
        Filters.lte("next", new BsonDateTime(now.plusMillis(horizon.toMillis).toEpochMilli))))
    try {
      val before = Await.result(collection.findOneAndUpdate(within, update,
        FindOneAndUpdateOptions().upsert(true).returnDocument(ReturnDocument.BEFORE)).toFutureOption(), Timeout)
      Right(FleetHostPace.slotOf(before.flatMap(MongoFleetHostPace.nextOf), now))
    } catch {
      // The host's document exists and its next slot is past the horizon: the upsert met its id.
      // A findAndModify's upsert reports the clash as a command error, not a write error.
      case e: com.mongodb.MongoException if e.getCode == 11000 =>
        Left(Await.result(collection.find(Filters.eq("_id", host)).headOption(), Timeout)
          .flatMap(MongoFleetHostPace.nextOf).getOrElse(now))
    }
  }
}

object MongoFleetHostPace {
  val Collection = "fleet_host_pace"

  /** A host's document: its next free slot. */
  def encode(host: String, next: Instant): Document = Document("_id" -> host, "next" -> BsonDateTime(next.toEpochMilli))

  /** The next free slot a host's document holds, if any. */
  def nextOf(document: Document): Option[Instant] = document.get[BsonDateTime]("next").map(at => Instant.ofEpochMilli(at.getValue))
}
