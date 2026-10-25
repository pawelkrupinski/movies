package services.identity.agreement

import org.mongodb.scala.bson.collection.immutable.Document
import org.bson.BsonDocument
import org.mongodb.scala.bson.{BsonInt64, BsonString}
import org.mongodb.scala.model.{Filters, ReplaceOneModel, ReplaceOptions}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}

import scala.concurrent.Await
import scala.concurrent.duration._

/** One live cluster's key as stored: the stage's counter ([[ClusterKeys]]) and the cluster's first listing
 *  ([[services.identity.StoredFamily.idOf]]) — what a restart re-attaches the key by. */
final case class StoredKey(key: Long, first: String)

/** The storage seam of the agreement stage's cluster keys, as [[AgreementVerdicts]] is its verdicts': each live cluster's
 *  key and the counter's high-water mark, so a restart neither re-keys every cluster nor hands a key out twice. */
trait AgreementKeys {
  def all(): AgreementKeys.Held
  /** Drop the keys `retired`, keep `moved`, and hold `next` as the counter's high-water mark. */
  def replace(retired: Set[Long], moved: Seq[StoredKey], next: Long): Unit
}

/** The keys as [[MongoAgreementKeys]] would hold them: each written through [[AgreementKeys.encode]] to BSON and read back. */
final class InMemoryAgreementKeys extends AgreementKeys {
  private val kept = scala.collection.mutable.LinkedHashMap.empty[Long, StoredKey]
  private var next = 0L
  private val stored = services.BsonRoundTrip.via(AgreementKeys.encode, AgreementKeys.decode)
  def all(): AgreementKeys.Held = synchronized(AgreementKeys.Held(kept.values.toSeq, next))
  def replace(retired: Set[Long], moved: Seq[StoredKey], next: Long): Unit = synchronized {
    kept --= retired; moved.foreach(k => kept(k.key) = stored(k)); this.next = next
  }
}

/** The keys in `identity_agreement_keys`, one document per live cluster (`_id` its key, `first` its first listing) and
 *  one holding the counter (`_id` "next"). Every call is awaited and lets a failure propagate; the stage writes only
 *  keys that moved. */
final class MongoAgreementKeys(db: MongoDatabase) extends AgreementKeys {
  import AgreementKeys._
  private val Timeout = 60.seconds
  private lazy val collection: MongoCollection[Document] = db.getCollection[Document](Collection)

  def all(): Held = {
    val docs = Await.result(collection.find().batchSize(tools.MongoReplies.Default).map(_.toBsonDocument).toFuture(), Timeout)
    Held(docs.collect { case d if d.get("_id").isInt64 => decode(d) }, docs.collectFirst { case d if d.get("_id").isString => d.getInt64("next").getValue }.getOrElse(0L))
  }

  def replace(retired: Set[Long], moved: Seq[StoredKey], next: Long): Unit = {
    retired.toSeq.grouped(AgreementVerdicts.WriteBatch).foreach(keys =>
      Await.result(collection.deleteMany(Filters.in("_id", keys.map(java.lang.Long.valueOf)*)).toFuture(), Timeout))
    moved.grouped(AgreementVerdicts.WriteBatch).foreach(batch => Await.result(collection.bulkWrite(batch.map(k =>
      ReplaceOneModel(Filters.equal("_id", k.key), Document(encode(k)), ReplaceOptions().upsert(true)))).toFuture(), Timeout))
    Await.result(collection.replaceOne(Filters.equal("_id", CounterId), Document("_id" -> BsonString(CounterId), "next" -> BsonInt64(next)),
      ReplaceOptions().upsert(true)).toFuture(), Timeout)
  }
}

object AgreementKeys {
  val Collection = "identity_agreement_keys"
  /** The counter's document's `_id`: a string, where every key's is a number. */
  val CounterId = "next"
  /** Every live cluster's key, and the next key the counter hands out. */
  final case class Held(keys: Seq[StoredKey], next: Long)

  /** A key as its document: `_id` the key, `first` its cluster's first listing. */
  def encode(key: StoredKey): BsonDocument = new BsonDocument("_id", BsonInt64(key.key)).append("first", BsonString(key.first))
  def decode(d: BsonDocument): StoredKey  = StoredKey(d.getInt64("_id").getValue, d.getString("first").getValue)
}
