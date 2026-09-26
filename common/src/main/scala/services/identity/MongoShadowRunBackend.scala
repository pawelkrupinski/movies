package services.identity

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.{BsonArray, BsonDateTime, BsonDocument, BsonDouble, BsonInt32, BsonNull, BsonString, BsonValue}
import org.mongodb.scala.model.{Filters, Indexes, Sorts}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import services.{MongoTtlIndex, TtlIndexMismatches}

import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * The shadow runs in two SHADOW collections nothing serving reads: `identity_shadow_decisions`
 * (one document per resolver cluster) and `identity_shadow_diff` (one per family where the
 * resolver and the pipeline part ways). Every document carries its run's `runAt` and `expireAt`;
 * a run replaces the previous one by inserting its documents and then deleting every other run's.
 *
 * Expiry is Mongo's TTL monitor on `expireAt` (`expireAfterSeconds = 0`), the instant stamped by
 * [[ShadowRunStore]] — the WRITER (the worker) reconciles that index; a reader (the web's admin
 * view) never touches it. Every call is awaited and lets a failure propagate.
 */
final class MongoShadowRunBackend private (db: MongoDatabase, reconcile: MongoCollection[Document] => Unit)
    extends ShadowRunBackend {
  import MongoShadowRunBackend._

  private val Timeout = 60.seconds

  private lazy val decisions: MongoCollection[Document] = prepared(ShadowRunStore.DecisionsCollection)
  private lazy val diff: MongoCollection[Document]      = prepared(ShadowRunStore.DiffCollection)

  private def prepared(name: String): MongoCollection[Document] = {
    val c = db.getCollection[Document](name)
    reconcile(c)
    c
  }

  def replace(run: ShadowRun, expireAt: Instant): Unit = {
    def stamped(d: BsonDocument, seq: Int) =
      Document(d.append("runAt", date(run.at)).append("expireAt", date(expireAt)).append("seq", BsonInt32(seq)))
    insert(decisions, run.clusters.zipWithIndex.map { case (c, i) => stamped(encodeCluster(c), i) })
    insert(diff, run.families.zipWithIndex.map { case (f, i) => stamped(encodeFamily(f), i) })
    Seq(decisions, diff).foreach(c => Await.result(c.deleteMany(Filters.ne("runAt", date(run.at))).toFuture(), Timeout))
  }

  def latest(): Option[(ShadowRun, Instant)] =
    Await.result(decisions.find().sort(Sorts.descending("runAt")).limit(1).headOption(), Timeout).map { head =>
      val runAt = head.get[BsonDateTime]("runAt").get
      def of(c: MongoCollection[Document]) = Await.result(c.find(Filters.equal("runAt", runAt)).sort(Sorts.ascending("seq")).toFuture(), Timeout)
      ShadowRun(Instant.ofEpochMilli(runAt.getValue), of(decisions).map(d => decodeCluster(d.toBsonDocument)), of(diff).map(d => decodeFamily(d.toBsonDocument))) ->
        Instant.ofEpochMilli(head.get[BsonDateTime]("expireAt").get.getValue)
    }

  private def insert(c: MongoCollection[Document], docs: Seq[Document]): Unit =
    if (docs.nonEmpty) { Await.result(c.insertMany(docs).toFuture(), Timeout); () }
}

object MongoShadowRunBackend {

  /** The worker's: it writes the runs and owns the indexes, the TTL one included. */
  def writer(db: MongoDatabase, ttlMismatches: TtlIndexMismatches): MongoShadowRunBackend =
    new MongoShadowRunBackend(db, c => {
      Await.result(c.createIndex(Indexes.ascending("runAt", "seq")).toFuture(), 60.seconds)
      MongoTtlIndex.reconcile(c, "expireAt", 0L, "ShadowRunStore", ttlMismatches)
    })

  /** The admin view's: reads only, and leaves the collections and indexes to the writer. */
  def reader(db: MongoDatabase): MongoShadowRunBackend = new MongoShadowRunBackend(db, _ => ())

  private def date(i: Instant) = BsonDateTime(i.toEpochMilli)
  private def optInt(v: Option[Int]): BsonValue = v.fold[BsonValue](BsonNull())(BsonInt32(_))
  private def readInt(d: BsonDocument, name: String): Option[Int] = Option(d.get(name)).filter(_.isInt32).map(_.asInt32.getValue)
  private def strings(xs: Seq[String]) = BsonArray.fromIterable(xs.map(BsonString(_)))
  private def readStrings(d: BsonDocument, name: String): Seq[String] = d.getArray(name).getValues.asScala.toSeq.map(_.asString.getValue)

  private def encodeFilm(p: PipelineFilmRef): BsonDocument =
    new BsonDocument().append("id", BsonString(p.id)).append("tmdbId", optInt(p.tmdbId))
  private def decodeFilm(d: BsonDocument): PipelineFilmRef = PipelineFilmRef(d.getString("id").getValue, readInt(d, "tmdbId"))

  private def document(fields: (String, BsonValue)*): BsonDocument =
    fields.foldLeft(new BsonDocument()) { case (d, (k, v)) => d.append(k, v) }

  private[identity] def encodeCluster(c: ShadowCluster): BsonDocument = document(
    "family"         -> BsonInt32(c.family),
    "members"        -> ListingKeyBson.encodeAll(c.decision.members),
    "film"           -> optInt(c.decision.film),
    "confidence"     -> BsonDouble(c.decision.confidence),
    "basis"          -> BsonString(c.decision.basis.toString),
    "explanation"    -> strings(c.decision.explanation),
    "contradictions" -> strings(c.decision.contradictions),
    "relation"       -> c.relation.fold[BsonValue](BsonNull())(r => BsonString(r.label)),
    "pipelineFilms"  -> BsonArray.fromIterable(c.pipelineFilms.map(encodeFilm)))

  private[identity] def decodeCluster(d: BsonDocument): ShadowCluster = {
    ShadowCluster(
      ResolverDecision(ListingKeyBson.decodeAll(d.getArray("members")), readInt(d, "film"), d.getDouble("confidence").getValue,
        ResolverDecision.Basis.valueOf(d.getString("basis").getValue), readStrings(d, "explanation"), readStrings(d, "contradictions")),
      d.getInt32("family").getValue,
      Option(d.get("relation")).filter(_.isString).flatMap(v => ShadowRelation.fromLabel(v.asString.getValue)),
      d.getArray("pipelineFilms").getValues.asScala.toSeq.map(v => decodeFilm(v.asDocument)))
  }

  private[identity] def encodeFamily(f: ShadowFamily): BsonDocument = document(
    "family"    -> BsonInt32(f.family),
    "resolver"  -> BsonArray.fromIterable(f.resolver.map { case (film, members) =>
      new BsonDocument().append("film", optInt(film)).append("members", ListingKeyBson.encodeAll(members)) }),
    "pipeline"  -> BsonArray.fromIterable(f.pipeline.map { case (film, members) =>
      encodeFilm(film).append("members", ListingKeyBson.encodeAll(members)) }),
    "unplaced"  -> ListingKeyBson.encodeAll(f.unplaced),
    "relations" -> document(f.relations.toSeq.sortBy(_._1.ordinal).map { case (r, n) => r.label -> (BsonInt32(n): BsonValue) }*))

  private[identity] def decodeFamily(d: BsonDocument): ShadowFamily = {
    def docs(name: String) = d.getArray(name).getValues.asScala.toSeq.map(_.asDocument)
    ShadowFamily(
      d.getInt32("family").getValue,
      docs("resolver").map(r => readInt(r, "film") -> ListingKeyBson.decodeAll(r.getArray("members"))),
      docs("pipeline").map(p => decodeFilm(p) -> ListingKeyBson.decodeAll(p.getArray("members"))),
      ListingKeyBson.decodeAll(d.getArray("unplaced")),
      d.getDocument("relations").entrySet.asScala.toSeq.flatMap(e => ShadowRelation.fromLabel(e.getKey).map(_ -> e.getValue.asInt32.getValue)).toMap)
  }
}
