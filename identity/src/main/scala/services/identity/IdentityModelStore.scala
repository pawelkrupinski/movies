package services.identity

import scala.concurrent.Await
import scala.concurrent.duration.*

import models.ListingKey
import org.bson.codecs.configuration.CodecRegistries
import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonInt32, BsonInt64, BsonString, BsonValue}
import org.mongodb.scala.model.{Filters, ReplaceOptions, UpdateOneModel, UpdateOptions}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import services.identity.measures.ResolverDecision
import services.identity.vocabulary.{BsonFields, CandidateQuery, DocumentDigest, ListingKeyBson}

/** One family of the incremental model as stored: what it decided, what it blocks under, what it
 *  asked and read, the node each listing was, and the digest of the corpus facts it read — all an
 *  [[IncrementalResolver]] needs, after a restart, to tell whether the family still stands. After one under other title
 *  rules, also the titles it read through the title forms and the digest of its members as the model read them
 *  ([[TitleRuleChange]]): `None` for a family stored before either was kept, which such a restart re-resolves. */
final case class StoredFamily(id: String, family: IdentityResolver.RegionFamily, digest: Long,
                              titles: Option[TitleFormInputs] = None, members: Option[Long] = None)

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
  /** The title rules the stored families were decided under ([[TitleRuleRecord]]), kept apart from the rules version: a
   *  change to them alone re-resolves only the families that read what moved. */
  def titleRules: Option[String]
  def recordTitleRules(record: String): Unit
}

/** The families as [[MongoIdentityModelStore]] would hold them: each written through its codec to BSON and read back. */
final class InMemoryIdentityModelStore extends IdentityModelStore {
  private val kept = scala.collection.mutable.LinkedHashMap.empty[String, StoredFamily]
  @volatile private var rules: Option[String] = None
  @volatile private var titles: Option[String] = None
  private val stored = tools.BsonRoundTrip.via(MongoIdentityModelStore.encode, MongoIdentityModelStore.decode)
  def families(): Seq[StoredFamily] = synchronized(kept.values.toSeq)
  def replace(removed: Set[String], added: Seq[StoredFamily]): Unit = synchronized { kept --= removed; added.foreach(f => kept(f.id) = stored(f)) }
  def rulesVersion: Option[String] = rules
  def recordRulesVersion(version: String): Unit = rules = Some(version)
  def titleRules: Option[String] = titles
  def recordTitleRules(record: String): Unit = titles = Some(record)
}

/** The families in `identity_model_families` (one document per family, `_id` its [[StoredFamily.id]])
 *  and the rules version in `identity_model_meta`. Every call is awaited and lets a failure propagate. */
final class MongoIdentityModelStore(db: MongoDatabase) extends IdentityModelStore {
  import MongoIdentityModelStore._
  private val Timeout = 60.seconds
  private lazy val collection: MongoCollection[Document] = db.getCollection[Document](FamiliesCollection)
  private lazy val meta: MongoCollection[Document]       = db.getCollection[Document](MetaCollection)

  /** Each family read straight off its reply's bytes into the model's types ([[StoredFamilyCodec]]), as the reply
   *  arrives: no BSON tree of the document is built. Decoded as one, a US take-up's ~2,100 families were ~400k maps
   *  and ~1M key strings beside the decoded families, alive long enough at boot to be promoted and then dropped. */
  def families(): Seq[StoredFamily] =
    Await.result(db.withCodecRegistry(CodecRegistries.fromRegistries(CodecRegistries.fromCodecs(StoredFamilyCodec), db.codecRegistry))
      .getCollection[StoredFamily](FamiliesCollection).find().batchSize(tools.MongoReplies.Families).toFuture(), Timeout)

  private val documentsWritten = new java.util.concurrent.atomic.AtomicLong
  /** How many family documents this store has written — those whose content moved. */
  def written: Long = documentsWritten.get

  /** One bulk write for `added`, not an awaited `replaceOne` each: a take-up writes every family of
   *  the country (~2,200 US), and one round trip apiece was ~30 s of a US boot's projection. The
   *  driver splits the batch to the server's message limits itself.
   *
   *  Only what moved is written: a family whose content digest matches the stored one is left alone.
   *  A rules change re-resolves every family and nearly all decide what they did before; written
   *  whole, each such boot rewrote every family document (~101 MB of oplog per rollout).
   *
   *  What moved is written as the document's replacement through an update pipeline, not a `replaceOne`: Mongo logs
   *  a pipeline update as the diff between the stored document and the new one, a replacement whole. A large family
   *  moves by a listing joining it or its corpus digest changing, and its document is up to ~1.6 MB (US): written
   *  whole, each such move was its whole document in the oplog, ~230 MB/h across the countries — the oplog's largest
   *  source. The arrays a listing joins are [[bucketed]] so that the diff stays the size of what moved. */
  def replace(removed: Set[String], added: Seq[StoredFamily]): Unit = {
    if (removed.nonEmpty) Await.result(collection.deleteMany(Filters.in("_id", removed.toSeq*)).toFuture(), Timeout)
    added.grouped(WriteBatch).foreach { batch =>
      val docs    = batch.map(family => family.id -> DocumentDigest.of(encode(family), ContentField))
      val written = DocumentDigest.writeMoved(collection, docs, ContentField, Timeout) { (id, doc) =>
        UpdateOneModel(Filters.equal("_id", id), Seq(Document("$replaceWith" -> Document("$literal" -> doc))), UpdateOptions().upsert(true))
      }
      documentsWritten.addAndGet(written.toLong)
    }
  }

  def rulesVersion: Option[String] =
    Await.result(meta.find(Filters.equal("_id", RulesDocument)).headOption(), Timeout).flatMap(_.get[BsonString]("version")).map(_.getValue)

  def recordRulesVersion(version: String): Unit = {
    Await.result(meta.replaceOne(Filters.equal("_id", RulesDocument), Document("_id" -> RulesDocument, "version" -> version),
      ReplaceOptions().upsert(true)).toFuture(), Timeout)
    ()
  }

  def titleRules: Option[String] =
    Await.result(meta.find(Filters.equal("_id", TitleRulesDocument)).headOption(), Timeout).flatMap(_.get[BsonString]("rules")).map(_.getValue)

  def recordTitleRules(record: String): Unit = {
    Await.result(meta.replaceOne(Filters.equal("_id", TitleRulesDocument), Document("_id" -> TitleRulesDocument, "rules" -> record),
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
  private val TitleRulesDocument = "titleRules"

  /** About how many entries one bucket of a family's listing-keyed array holds ([[bucketed]]). */
  val ListingsPerBucket = 16

  /** A listing-keyed array of a family — its listings, the node of each, a decision's members — as it is stored:
   *  flat below twice [[ListingsPerBucket]] entries, else split into a power-of-two count of buckets by the hash of
   *  each entry's listing key, each bucket in the entries' own order. Mongo logs an update as the diff of the two
   *  documents array index by array index, so in one sorted array a listing joining a ~3,000-listing family shifted
   *  every entry after it, and the diff was up to the whole document; bucketed, only the entries of its bucket shift.
   *  The count changes only as the array doubles, and a reader takes either layout ([[BsonFields.bucketed]]). */
  private[identity] def bucketed[A](entries: Seq[A])(key: A => ListingKey, encode: A => BsonValue): BsonArray =
    bucketedBy(entries)(entry => ListingKey.serialised(key(entry)), encode)

  /** [[bucketed]], each entry's bucket by the hash of `name`. */
  private def bucketedBy[A](entries: Seq[A])(name: A => String, encode: A => BsonValue): BsonArray = {
    val count = Integer.highestOneBit(math.max(1, entries.size / ListingsPerBucket))
    if (count == 1) BsonArray.fromIterable(entries.map(encode))
    else {
      val buckets = Array.fill(count)(new BsonArray())
      entries.foreach(entry => buckets(Math.floorMod(name(entry).hashCode, count)).add(encode(entry)))
      BsonArray.fromIterable(buckets.toSeq)
    }
  }

  /** Titles as a family's [[TitleFormInputs]] keep them: sorted and bucketed, so a title the family newly reads moves
   *  only its bucket in the oplog's diff. */
  private def titles(values: Set[String]): BsonArray = bucketedBy(values.toSeq.sorted)(identity, BsonString(_))
  private def readTitles(reader: org.bson.BsonReader): Set[String] = {
    val all = Set.newBuilder[String]; BsonFields.bucketed(reader)(all += reader.readString()); all.result()
  }

  /** A decision's members, bucketed when they are in the sorted order a reader of buckets restores; else as they
   *  stand. */
  private def members(keys: Seq[ListingKey]): BsonValue =
    if (keys.lazyZip(keys.drop(1)).forall(ListingKey.ordering.lteq)) bucketed(keys)(identity, ListingKeyBson.encode)
    else ListingKeyBson.encodeAll(keys)

  private def strings(values: Iterable[String]) = BsonArray.fromIterable(values.toSeq.sorted.map(BsonString(_)))
  private def ints(values: Iterable[Int]) = BsonArray.fromIterable(values.toSeq.sorted.map(BsonInt32(_)))
  private def queries(values: Iterable[CandidateQuery]) = strings(values.map(_.sortKey))

  private[identity] def encode(stored: StoredFamily): BsonDocument = {
    val family = stored.family
    val reads  = family.reads
    val document = new BsonDocument()
      .append("_id", BsonString(stored.id))
      .append("listings", bucketed(family.listings.toSeq.sorted(using ListingKey.ordering))(identity, ListingKeyBson.encode))
      .append("decisions", BsonArray.fromIterable(family.decisions.map(ResolverDecisionBson.encodeWith(_, members))))
      .append("blockKeys", strings(family.blockKeys))
      .append("queries", queries(family.queries))
      .append("films", ints(family.films))
      .append("reads", new BsonDocument()
        .append("titles", strings(reads.titles)).append("groups", strings(reads.groups)).append("segments", strings(reads.segments))
        .append("banners", strings(reads.banners)).append("films", ints(reads.films)).append("queries", queries(reads.queries)))
      .append("nodes", bucketed(family.nodeKeys.toSeq.sortBy(_._1)(using ListingKey.ordering))(_._1, { case (listing, node) =>
        new BsonDocument().append("listing", ListingKeyBson.encode(listing)).append("node", BsonString(node)) }))
      .append("digest", BsonInt64(stored.digest))
    stored.titles.foreach(inputs => document.append("titleKeys", titles(inputs.keys)).append("titleSearches", titles(inputs.searches)))
    stored.members.foreach(digest => document.append("members", BsonInt64(digest)))
    document
  }

  private[identity] def decode(d: BsonDocument): StoredFamily = read(new org.bson.BsonDocumentReader(d))

  /** A stored family, from the reply's bytes as the store reads it ([[BsonFields]]). */
  private[identity] object StoredFamilyCodec extends org.bson.codecs.Codec[StoredFamily] {
    private val documents = new org.bson.codecs.BsonDocumentCodec
    def decode(reader: org.bson.BsonReader, context: org.bson.codecs.DecoderContext): StoredFamily = read(reader)
    def encode(writer: org.bson.BsonWriter, family: StoredFamily, context: org.bson.codecs.EncoderContext): Unit =
      documents.encode(writer, MongoIdentityModelStore.encode(family), context)
    def getEncoderClass: Class[StoredFamily] = classOf[StoredFamily]
  }

  /** The family the reader is at, read field by field. */
  private[identity] def read(reader: org.bson.BsonReader): StoredFamily = {
    var id: String                              = null
    var listings: Set[ListingKey]               = null
    var decisions: Seq[ResolverDecision]        = null
    var blockKeys: Set[String]                  = null
    var queries: Set[CandidateQuery]            = null
    var films: Set[Int]                         = null
    var reads: CorpusContext.Reads              = null
    var nodes: Vector[(ListingKey, String)]     = null
    var digest: java.lang.Long                  = null
    val titles                                  = new TitleFields
    BsonFields.document(reader) {
      case "_id"       => id = reader.readString()
      case "listings"  => listings = { val all = Set.newBuilder[ListingKey]; BsonFields.bucketed(reader)(all += ListingKeyBson.read(reader)); all.result() }
      case "decisions" => decisions = { val all = Vector.newBuilder[ResolverDecision]; BsonFields.array(reader)(all += ResolverDecisionBson.read(reader)); all.result() }
      case "blockKeys" => blockKeys = BsonFields.stringSet(reader)
      case "queries"   => queries = readQueries(reader)
      case "films"     => films = BsonFields.intSet(reader)
      case "reads"     => reads = readReads(reader)
      case "nodes"     => nodes = readNodes(reader)
      case "digest"    => digest = reader.readInt64()
      case _           => titles.read(reader)
    }
    val held = BsonFields.required(listings, "listings")
    StoredFamily(BsonFields.required(id, "_id"), IdentityResolver.RegionFamily(
      held, BsonFields.required(decisions, "decisions"), BsonFields.required(blockKeys, "blockKeys"),
      BsonFields.required(queries, "queries"), BsonFields.required(films, "films"), BsonFields.required(reads, "reads"),
      sharedNodes(held, BsonFields.required(nodes, "nodes"))),
      BsonFields.required(digest, "digest").longValue,
      titles.inputs, titles.members)
  }

  /** A family's fields of a title-rule change ([[StoredFamily.titles]], [[StoredFamily.members]]), absent from a family
   *  stored before they were kept; every other field it does not know is skipped. */
  private final class TitleFields {
    private var keys, searches: Set[String] = null
    var members: Option[Long] = None
    /** The field the reader is at, its name read already. */
    def read(reader: org.bson.BsonReader): Unit = reader.getCurrentName match {
      case "titleKeys"     => keys = readTitles(reader)
      case "titleSearches" => searches = readTitles(reader)
      case "members"       => members = Some(reader.readInt64())
      case _               => reader.skipValue()
    }
    def inputs: Option[TitleFormInputs] = for (k <- Option(keys); s <- Option(searches)) yield TitleFormInputs(k, s)
  }

  /** One instance of each listing key and each node text per family: the `nodes` pairs repeat the `listings` keys
   *  (`held`), and a node's text repeats for each of its listings — decoded as they stand, every restore kept a second
   *  key and a node text per listing for the model's lifetime. */
  private def sharedNodes(held: Set[ListingKey], pairs: Vector[(ListingKey, String)]): Map[ListingKey, String] = {
    val keyOf = held.iterator.map(key => key -> key).toMap
    val texts = new java.util.HashMap[String, String](pairs.size * 2)
    pairs.iterator.map { case (key, text) => keyOf.getOrElse(key, key) -> texts.computeIfAbsent(text, identity) }.toMap
  }

  /** A family's `nodes`: each listing's evidence node, bucketed. */
  private def readNodes(reader: org.bson.BsonReader): Vector[(ListingKey, String)] = {
    val all = Vector.newBuilder[(ListingKey, String)]
    BsonFields.bucketed(reader) {
      var key: ListingKey = null
      var node: String    = null
      BsonFields.document(reader) {
        case "listing" => key = ListingKeyBson.read(reader)
        case "node"    => node = reader.readString()
        case _         => reader.skipValue()
      }
      all += BsonFields.required(key, "listing") -> BsonFields.required(node, "node")
    }
    all.result()
  }

  private def readQueries(reader: org.bson.BsonReader): Set[CandidateQuery] =
    BsonFields.stringSet(reader).flatMap(CandidateQuery.fromSortKey)

  private def readReads(reader: org.bson.BsonReader): CorpusContext.Reads = {
    var titles, groups, segments, banners: Set[String] = null
    var films: Set[Int]                                = null
    var queries: Set[CandidateQuery]                   = null
    BsonFields.document(reader) {
      case "titles"   => titles = BsonFields.stringSet(reader)
      case "groups"   => groups = BsonFields.stringSet(reader)
      case "segments" => segments = BsonFields.stringSet(reader)
      case "banners"  => banners = BsonFields.stringSet(reader)
      case "films"    => films = BsonFields.intSet(reader)
      case "queries"  => queries = readQueries(reader)
      case _          => reader.skipValue()
    }
    CorpusContext.Reads(BsonFields.required(titles, "titles"), BsonFields.required(groups, "groups"),
      BsonFields.required(segments, "segments"), BsonFields.required(banners, "banners"), BsonFields.required(films, "films"),
      BsonFields.required(queries, "queries"))
  }
}
