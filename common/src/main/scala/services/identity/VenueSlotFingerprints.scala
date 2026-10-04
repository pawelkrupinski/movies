package services.identity

import org.mongodb.scala.bson.BsonInt64
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.model.{Filters, InsertManyOptions, Projections}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * The venue slots the identity projection last kept in its memo ([[VenueSlotMemo]]), as fingerprints: what each
 * was built from and what it came to ([[VenueSlotMemo.fingerprint]]). Held across a restart, so the first
 * projection after a boot can tell a stored film's slots at a venue are what its unchanged listing would build,
 * and reuse them rather than rebuild every slot of the country (a US boot: ~24 s and ~7.3 GB against ~6 s and
 * ~760 MB a steady tick).
 *
 * Every fingerprint is a fact that cannot go stale — these inputs, under this build of the slot code, make
 * these slots — so one left behind (a write lost, two workers side by side through a rollout) costs only its
 * space, and one missing only a rebuild.
 */
trait VenueSlotFingerprints {
  def all(): Set[Long]
  def update(added: Set[Long], removed: Set[Long]): Unit
}

final class InMemoryVenueSlotFingerprints extends VenueSlotFingerprints {
  private var kept = Set.empty[Long]
  def all(): Set[Long] = synchronized(kept)
  def update(added: Set[Long], removed: Set[Long]): Unit = synchronized { kept = kept -- removed ++ added }
}

/** One document per fingerprint, the fingerprint its `_id`: a projection writes only the ones that moved. */
final class MongoVenueSlotFingerprints(db: MongoDatabase) extends VenueSlotFingerprints {
  import MongoVenueSlotFingerprints._
  private lazy val collection: MongoCollection[Document] = db.getCollection[Document](Collection)

  def all(): Set[Long] =
    Await.result(collection.find().projection(Projections.include("_id")).batchSize(tools.MongoReplies.Default)
      .map(_.toBsonDocument.getInt64("_id").getValue).toFuture(), Timeout).toSet

  def update(added: Set[Long], removed: Set[Long]): Unit = {
    removed.toSeq.grouped(Batch).foreach(ids => Await.result(collection.deleteMany(Filters.in("_id", ids*)).toFuture(), Timeout))
    added.toSeq.grouped(Batch).foreach { ids =>
      try Await.result(collection.insertMany(ids.map(id => Document("_id" -> BsonInt64(id))), InsertManyOptions().ordered(false)).toFuture(),
        Timeout)
      catch {
        // Already there (a write retried, or a worker beside this one wrote it): the fact stands either way.
        case bulk: com.mongodb.MongoBulkWriteException if bulk.getWriteErrors.stream().allMatch(_.getCode == DuplicateKey) => ()
      }
    }
  }
}

object MongoVenueSlotFingerprints {
  val Collection = "identity_slot_fingerprints"
  private val Timeout      = 60.seconds
  private val Batch        = 10_000
  private val DuplicateKey = 11000
}
