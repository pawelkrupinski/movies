package services.identity

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonInt32, BsonInt64, BsonString}
import org.mongodb.scala.model.{Filters, Projections, ReplaceOneModel, ReplaceOptions}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import services.movies.ListingKey

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/** One family of the incremental model as stored: what it decided, what it blocks under, what it
 *  asked and read, the node each listing was, and the digest of the corpus facts it read — all an
 *  [[IncrementalResolver]] needs, after a restart, to tell whether the family still stands. */
final case class StoredFamily(id: String, family: IdentityResolver.RegionFamily, digest: Long)

object StoredFamily {
  /** A family's id: its smallest listing, stable for as long as that listing is its member. */
  def idOf(listings: Iterable[ListingKey]): String = ListingKey.serialised(listings.min(using ListingKey.ordering))
}

/**
 * The storage seam of the incremental model, and nothing else: keep its families, replace some of
 * them, hand them all back, remember which rules decided them. What a stored family is worth after
 * a restart is [[IncrementalResolver]]'s to decide, never a backend's.
 */
trait IdentityModelStore {
  def families(): Seq[StoredFamily]
  /** Drop the families `removed` names, then keep `added`. */
  def replace(removed: Set[String], added: Seq[StoredFamily]): Unit
  /** The rules the stored families were decided under ([[IncrementalResolver.rulesVersion]]). */
  def rulesVersion: Option[String]
  def recordRulesVersion(version: String): Unit
}

final class InMemoryIdentityModelStore extends IdentityModelStore {
  private val kept = scala.collection.mutable.LinkedHashMap.empty[String, StoredFamily]
  @volatile private var rules: Option[String] = None
  def families(): Seq[StoredFamily] = synchronized(kept.values.toSeq)
  def replace(removed: Set[String], added: Seq[StoredFamily]): Unit = synchronized { kept --= removed; added.foreach(f => kept(f.id) = f) }
  def rulesVersion: Option[String] = rules
  def recordRulesVersion(version: String): Unit = rules = Some(version)
}

/** The families in `identity_model_families` (one document per family, `_id` its [[StoredFamily.id]])
 *  and the rules version in `identity_model_meta`. Every call is awaited and lets a failure propagate. */
final class MongoIdentityModelStore(db: MongoDatabase) extends IdentityModelStore {
  import MongoIdentityModelStore._
  private val Timeout = 60.seconds
  private lazy val collection: MongoCollection[Document] = db.getCollection[Document](FamiliesCollection)
  private lazy val meta: MongoCollection[Document]       = db.getCollection[Document](MetaCollection)

  /** Each family decoded as its reply arrives, so the read holds the decoded families and one reply's
   *  documents — not every raw document beside them: a raw family's BSON tree is ~2x its decoded form
   *  (2,200 synthetic 17 KB families: a peak of 211 MB read whole, then decoded; 82 MB decoded as read). */
  def families(): Seq[StoredFamily] =
    Await.result(collection.find().batchSize(tools.MongoReplies.Families).map(d => decode(d.toBsonDocument)).toFuture(), Timeout)

  private val documentsWritten = new java.util.concurrent.atomic.AtomicLong
  /** How many family documents this store has written — those whose content moved. */
  def written: Long = documentsWritten.get

  /** One bulk write for `added`, not an awaited `replaceOne` each: a take-up writes every family of
   *  the country (~2,200 US), and one round trip apiece was ~30 s of a US boot's projection. The
   *  driver splits the batch to the server's message limits itself.
   *
   *  Only what moved is written: a family whose content digest matches the stored one is left alone.
   *  A rules change re-resolves every family and nearly all decide what they did before; written
   *  whole, each such boot rewrote every family document (~101 MB of oplog per rollout). */
  def replace(removed: Set[String], added: Seq[StoredFamily]): Unit = {
    if (removed.nonEmpty) Await.result(collection.deleteMany(Filters.in("_id", removed.toSeq*)).toFuture(), Timeout)
    added.grouped(WriteBatch).foreach { batch =>
      val docs   = batch.map(family => family.id -> DocumentDigest.of(encode(family), ContentField))
      val stored = Await.result(collection.find(Filters.in("_id", docs.map(_._1)*)).projection(Projections.include(ContentField))
        .batchSize(tools.MongoReplies.Default).toFuture(), Timeout)
        .flatMap(d => d.get("_id").map(_.asString.getValue -> d.get(ContentField).filter(_.isInt64).map(_.asInt64.getValue))).toMap
      val moved  = docs.filterNot { case (id, doc) => stored.get(id).flatten.contains(doc.getInt64(ContentField).getValue) }
      if (moved.nonEmpty) {
        Await.result(collection.bulkWrite(moved.map { case (id, doc) =>
          ReplaceOneModel(Filters.equal("_id", id), Document(doc), ReplaceOptions().upsert(true)) }).toFuture(), Timeout)
        documentsWritten.addAndGet(moved.size.toLong)
      }
    }
  }

  def rulesVersion: Option[String] =
    Await.result(meta.find(Filters.equal("_id", RulesDocument)).headOption(), Timeout).flatMap(_.get[BsonString]("version")).map(_.getValue)

  def recordRulesVersion(version: String): Unit = {
    Await.result(meta.replaceOne(Filters.equal("_id", RulesDocument), Document("_id" -> RulesDocument, "version" -> version),
      ReplaceOptions().upsert(true)).toFuture(), Timeout)
    ()
  }
}

object MongoIdentityModelStore {
  val FamiliesCollection = "identity_model_families"
  val MetaCollection     = "identity_model_meta"
  /** A family document's content digest, which a rewrite of an unchanged family is skipped by. */
  val ContentField       = "content"
  /** How many families one read of stored digests and one bulk write carry. */
  val WriteBatch         = 500
  private val RulesDocument = "rules"

  private def strings(values: Iterable[String]) = BsonArray.fromIterable(values.toSeq.sorted.map(BsonString(_)))
  private def readStrings(d: BsonDocument, name: String): Set[String] = d.getArray(name).getValues.asScala.map(_.asString.getValue).toSet
  private def ints(values: Iterable[Int]) = BsonArray.fromIterable(values.toSeq.sorted.map(BsonInt32(_)))
  private def readInts(d: BsonDocument, name: String): Set[Int] = d.getArray(name).getValues.asScala.map(_.asInt32.getValue).toSet
  private def queries(values: Iterable[CandidateQuery]) = strings(values.map(_.sortKey))
  private def readQueries(d: BsonDocument, name: String): Set[CandidateQuery] = readStrings(d, name).flatMap(CandidateQuery.fromSortKey)

  private[identity] def encode(stored: StoredFamily): BsonDocument = {
    val family = stored.family
    val reads  = family.reads
    new BsonDocument()
      .append("_id", BsonString(stored.id))
      .append("listings", ListingKeyBson.encodeAll(family.listings.toSeq.sorted(using ListingKey.ordering)))
      .append("decisions", BsonArray.fromIterable(family.decisions.map(ResolverDecisionBson.encode)))
      .append("blockKeys", strings(family.blockKeys))
      .append("queries", queries(family.queries))
      .append("films", ints(family.films))
      .append("reads", new BsonDocument()
        .append("titles", strings(reads.titles)).append("groups", strings(reads.groups)).append("segments", strings(reads.segments))
        .append("banners", strings(reads.banners)).append("films", ints(reads.films)).append("queries", queries(reads.queries)))
      .append("nodes", BsonArray.fromIterable(family.nodeKeys.toSeq.sortBy(_._1)(using ListingKey.ordering).map { case (listing, node) =>
        new BsonDocument().append("listing", ListingKeyBson.encode(listing)).append("node", BsonString(node)) }))
      .append("digest", BsonInt64(stored.digest))
  }

  private[identity] def decode(d: BsonDocument): StoredFamily = {
    val reads = d.getDocument("reads")
    // One instance of each listing key and each node text per family: the `nodes` pairs repeat the
    // `listings` keys, and a node's text repeats for each of its listings — decoded as they stand,
    // every restore kept a second key and a node text per listing for the model's lifetime.
    val listings = ListingKeyBson.decodeAll(d.getArray("listings")).toSet
    val keyOf    = listings.iterator.map(key => key -> key).toMap
    val texts    = scala.collection.mutable.HashMap.empty[String, String]
    StoredFamily(d.getString("_id").getValue, IdentityResolver.RegionFamily(
      listings,
      d.getArray("decisions").getValues.asScala.toSeq.map(v => ResolverDecisionBson.decode(v.asDocument)),
      readStrings(d, "blockKeys"), readQueries(d, "queries"), readInts(d, "films"),
      CorpusContext.Reads(readStrings(reads, "titles"), readStrings(reads, "groups"), readStrings(reads, "segments"),
        readStrings(reads, "banners"), readInts(reads, "films"), readQueries(reads, "queries")),
      d.getArray("nodes").getValues.asScala.toSeq.map(_.asDocument).map(n =>
        { val key = ListingKeyBson.decode(n.getDocument("listing")); keyOf.getOrElse(key, key) } ->
          { val text = n.getString("node").getValue; texts.getOrElseUpdate(text, text) }).toMap),
      d.getInt64("digest").getValue)
  }
}
