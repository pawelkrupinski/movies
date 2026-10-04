package services.identity.agreement

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.{BsonArray, BsonDocument, BsonInt32, BsonInt64, BsonNull, BsonString, BsonValue}
import org.mongodb.scala.model.{Filters, ReplaceOneModel, ReplaceOptions}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}
import services.identity.IdentityMeasures

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/** One cluster's agreement verdict as stored: the digest of its listings, every family question it read with the
 *  digest of the answer it got, and the film the families agreed on, if any — all the stage needs, after a restart or
 *  a new answer elsewhere, to tell whether the verdict still stands without asking the resolver again. */
final case class StoredVerdict(id: String, listings: Long, reads: Map[String, Long], agreed: Option[AgreedFilm])

/** The storage seam of the agreement's verdicts, as [[services.identity.IdentityModelStore]] is the model's. */
trait AgreementVerdicts {
  def all(): Seq[StoredVerdict]
  /** Drop the verdicts `removed` names, then keep `added`. */
  def replace(removed: Set[String], added: Seq[StoredVerdict]): Unit
}

final class InMemoryAgreementVerdicts extends AgreementVerdicts {
  private val kept = scala.collection.mutable.LinkedHashMap.empty[String, StoredVerdict]
  def all(): Seq[StoredVerdict] = synchronized(kept.values.toSeq)
  def replace(removed: Set[String], added: Seq[StoredVerdict]): Unit = synchronized { kept --= removed; added.foreach(v => kept(v.id) = v) }
}

/** The verdicts in `identity_agreements`, one document per cluster (`_id` its smallest listing). Every call is awaited
 *  and lets a failure propagate; the stage writes only verdicts that moved. */
final class MongoAgreementVerdicts(db: MongoDatabase) extends AgreementVerdicts {
  import AgreementVerdicts._
  private val Timeout = 60.seconds
  private lazy val collection: MongoCollection[Document] = db.getCollection[Document](Collection)

  def all(): Seq[StoredVerdict] =
    Await.result(collection.find().batchSize(tools.MongoReplies.Default).map(d => decode(d.toBsonDocument)).toFuture(), Timeout)

  def replace(removed: Set[String], added: Seq[StoredVerdict]): Unit = {
    if (removed.nonEmpty) removed.toSeq.grouped(WriteBatch).foreach(ids => Await.result(collection.deleteMany(Filters.in("_id", ids*)).toFuture(), Timeout))
    added.grouped(WriteBatch).foreach(batch => Await.result(collection.bulkWrite(batch.map(verdict =>
      ReplaceOneModel(Filters.equal("_id", verdict.id), Document(encode(verdict)), ReplaceOptions().upsert(true)))).toFuture(), Timeout))
  }
}

object AgreementVerdicts {
  val Collection = "identity_agreements"
  val WriteBatch = 500

  private def strings(values: Map[String, String]) =
    new BsonDocument(values.toSeq.sorted.map { case (k, v) => new org.bson.BsonElement(k, BsonString(v)) }.asJava)
  private def stringsOf(d: BsonDocument): Map[String, String] = d.asScala.map { case (k, v) => k -> v.asString.getValue }.toMap

  /** A verdict as BSON: the agreed film as the families' labels, their ids, its cross-ids and the title and year the
   *  explanation names — what taking it reads, not the whole record. */
  def encode(verdict: StoredVerdict): BsonDocument = new BsonDocument()
    .append("_id", BsonString(verdict.id))
    .append("listings", BsonInt64(verdict.listings))
    .append("reads", new BsonDocument(verdict.reads.toSeq.sorted.map { case (q, digest) => new org.bson.BsonElement(q, BsonInt64(digest)) }.asJava))
    .append("agreed", verdict.agreed.fold[BsonValue](BsonNull()) { film =>
      val d = new BsonDocument("families", BsonArray.fromIterable(film.families.toSeq.map(_.label).sorted.map(BsonString(_))))
        .append("ids", strings(film.ids.map { case (family, id) => family.label -> id }))
        .append("crossIds", strings(film.record.crossIds))
        .append("title", BsonString(film.record.film.title))
      film.record.film.year.foreach(year => d.append("year", BsonInt32(year)))
      d
    })

  def decode(d: BsonDocument): StoredVerdict = {
    val byLabel = VoterFamily.values.map(f => f.label -> f).toMap
    StoredVerdict(d.getString("_id").getValue, d.getInt64("listings").getValue,
      d.getDocument("reads").asScala.map { case (q, digest) => q -> digest.asInt64.getValue }.toMap,
      Option(d.get("agreed")).filter(_.isDocument).map(_.asDocument).map { a =>
        val film = IdentityMeasures.Film(a.getString("title").getValue, None, Nil, Option(a.get("year")).map(_.asInt32.getValue), None, None, None, None)
        AgreedFilm(a.getArray("families").getValues.asScala.flatMap(v => byLabel.get(v.asString.getValue)).toSet,
          SourceRecord(film, stringsOf(a.getDocument("crossIds"))),
          stringsOf(a.getDocument("ids")).flatMap { case (label, id) => byLabel.get(label).map(_ -> id) })
      })
  }
}
