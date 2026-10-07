package services.review

import org.mongodb.scala.bson.collection.immutable.Document
import org.mongodb.scala.bson.{BsonArray, BsonDateTime, BsonDocument, BsonInt32, BsonNull, BsonString, BsonValue}
import org.mongodb.scala.model.{Filters, Sorts}
import org.mongodb.scala.{MongoCollection, MongoDatabase, ObservableFuture, SingleObservableFuture}

import java.time.Instant
import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/** The storage seam of the review answers: append one, read them all back in the order they were
 *  given. What an answer history MEANS ([[ReviewAnswers]]) is never a backend's to decide. */
trait ReviewAnswerStore {
  def append(answer: ReviewAnswer): Unit
  /** Every answer ever given, oldest first. */
  def all(): Seq[ReviewAnswer]
}

final class InMemoryReviewAnswerStore extends ReviewAnswerStore {
  private val kept = scala.collection.mutable.ArrayBuffer.empty[ReviewAnswer]
  def append(answer: ReviewAnswer): Unit = synchronized(kept += answer)
  def all(): Seq[ReviewAnswer] = synchronized(kept.toList)
}

/**
 * The answers in `review_answers`, one document per answer — on the LOCAL mirror instance, in its
 * own database ([[MongoReviewAnswerStore.Database]]) so a mirror re-seed, which drops and refills
 * the mirrored collections, can never touch them, and so they never reach prod.
 *
 * `_id` is the answer's time (`at`, zero-padded epoch millis — the clock the caller gave it) and a per-process sequence, so
 * the keyset read below returns them in the order they were given.
 */
final class MongoReviewAnswerStore(db: MongoDatabase) extends ReviewAnswerStore {
  import MongoReviewAnswerStore._
  private val Timeout = 30.seconds
  private lazy val collection: MongoCollection[Document] = db.getCollection[Document](Collection)
  private val sequence = new AtomicLong

  def append(answer: ReviewAnswer): Unit = {
    val id = f"${answer.at.toEpochMilli}%015d-${sequence.incrementAndGet()}%06d-${java.util.UUID.randomUUID().toString.take(8)}"
    Await.result(collection.insertOne(Document(encode(answer).append("_id", BsonString(id)))).toFuture(), Timeout)
    ()
  }

  def all(): Seq[ReviewAnswer] = {
    val out = Vector.newBuilder[ReviewAnswer]
    val outcome = services.movies.KeysetScan.scan[Document](
      "review_answers", batchSize = tools.MongoReplies.Default, maxAttempts = 3, initialBackoff = 200.millis,
      keyOf = _.toBsonDocument.getString("_id").getValue,
      fetchPage = (after, limit) => Await.result(collection.find(after.fold(Filters.empty())(id => Filters.gt("_id", id)))
        .sort(Sorts.ascending("_id")).limit(limit).batchSize(limit).toFuture(), Timeout))(batch => out ++= batch.map(d => decode(d.toBsonDocument)))
    if (!outcome.isComplete) throw new IllegalStateException("review_answers read incomplete")
    out.result()
  }
}

object MongoReviewAnswerStore {
  val Collection = "review_answers"
  /** The database on the local mirror instance the answers live in — never a mirrored one. */
  val Database   = "review_local"

  private def str(v: Option[String]): BsonValue = v.fold[BsonValue](BsonNull())(BsonString(_))
  private def strings(v: Seq[String]) = BsonArray.fromIterable(v.map(BsonString(_)))
  private def opt(d: BsonDocument, name: String): Option[String] = Option(d.get(name)).filter(_.isString).map(_.asString.getValue)
  private def optInt(d: BsonDocument, name: String): Option[Int] = Option(d.get(name)).filter(_.isInt32).map(_.asInt32.getValue)
  private def list(d: BsonDocument, name: String): Seq[String] =
    Option(d.get(name)).filter(_.isArray).toSeq.flatMap(_.asArray.getValues.asScala.map(_.asString.getValue))

  private def encodeFilm(f: FilmFacts): BsonDocument = new BsonDocument("ref", BsonString(f.ref.render))
    .append("title", str(f.title)).append("year", f.year.fold[BsonValue](BsonNull())(BsonInt32(_))).append("directors", strings(f.directors))
  private def decodeFilm(d: BsonDocument): Option[FilmFacts] =
    opt(d, "ref").flatMap(FilmRef.formed).map(FilmFacts(_, opt(d, "title"), optInt(d, "year"), list(d, "directors")))

  private[review] def encode(a: ReviewAnswer): BsonDocument = new BsonDocument()
    .append("clusterId", BsonString(a.clusterId))
    .append("country", BsonString(a.country))
    .append("page", BsonString(a.page.code))
    .append("verdict", BsonString(a.verdict.code))
    .append("ref", str(a.ref.map(_.render)))
    .append("shown", a.shown.fold[BsonValue](BsonNull())(encodeFilm))
    .append("title", BsonString(a.title))
    .append("members", BsonArray.fromIterable(a.members.map(m => new BsonDocument("venue", BsonString(m.venue))
      .append("rawTitle", BsonString(m.rawTitle)).append("page", str(m.page))
      .append("year", m.year.fold[BsonValue](BsonNull())(BsonInt32(_))).append("directors", strings(m.directors)))))
    .append("who", BsonString(a.who))
    .append("at", BsonDateTime(a.at.toEpochMilli))
    .append("warnings", strings(a.warnings))
    .append("legacyId", str(a.legacyId))

  private[review] def decode(d: BsonDocument): ReviewAnswer = ReviewAnswer(
    clusterId = d.getString("clusterId").getValue,
    country   = d.getString("country").getValue,
    page      = ReviewPage.byCode(d.getString("page").getValue).getOrElse(ReviewPage.Queue),
    verdict   = ReviewVerdict.byCode(d.getString("verdict").getValue).getOrElse(ReviewVerdict.Undo),
    ref       = opt(d, "ref").flatMap(FilmRef.formed),
    shown     = Option(d.get("shown")).filter(_.isDocument).flatMap(v => decodeFilm(v.asDocument)),
    title     = d.getString("title").getValue,
    members   = d.getArray("members").getValues.asScala.toSeq.map(_.asDocument).map(m =>
      ReviewMember(m.getString("venue").getValue, m.getString("rawTitle").getValue, opt(m, "page"), optInt(m, "year"), list(m, "directors"))),
    who       = d.getString("who").getValue,
    at        = Instant.ofEpochMilli(d.getDateTime("at").getValue),
    warnings  = list(d, "warnings"),
    legacyId  = opt(d, "legacyId"))
}

/**
 * What the answer history means — shared by every store. The CURRENT answer of a cluster is the
 * latest one given for it, unless that is an `Undo`; a cluster is answered when a current answer
 * covers it — given for its id, or for any listing it holds now (so a cluster that gains a venue
 * after it was answered does not come back into the queue).
 */
final class ReviewAnswers(store: ReviewAnswerStore) {
  def record(answer: ReviewAnswer): Unit = store.append(answer)
  def history(): Seq[ReviewAnswer] = store.all()
  def current(): Seq[ReviewAnswer] = ReviewAnswers.current(history())

  /** Answers imported from elsewhere, minus those already imported (by legacy id): importing twice adds nothing. */
  def importAll(answers: Seq[ReviewAnswer]): Int = {
    val known = history().flatMap(_.legacyId).toSet
    val fresh = answers.filterNot(a => a.legacyId.exists(known))
    fresh.foreach(store.append)
    fresh.size
  }
}

object ReviewAnswers {
  /** An `Undo` withdraws every answer that covers the card it was given on — by its id, or by a listing it holds —
   *  as a card answered under an earlier cluster id (before it gained a venue) posts its Undo under its new one. */
  def current(history: Seq[ReviewAnswer]): Seq[ReviewAnswer] =
    history.foldLeft(Vector.empty[ReviewAnswer]) { (kept, a) =>
      val withdrawn = kept.filterNot(_.clusterId == a.clusterId)
      if (a.verdict != ReviewVerdict.Undo) withdrawn :+ a
      else {
        val listings = a.members.map(_.identity).toSet
        withdrawn.filterNot(_.members.exists(m => listings(m.identity)))
      }
    }

  /** Looks a cluster's current answer up by its id, then by any of its listings. */
  final class Index(current: Seq[ReviewAnswer]) {
    private val byId       = current.map(a => a.clusterId -> a).toMap
    private val byIdentity = current.flatMap(a => a.members.map(_.identity -> a)).toMap
    def answerFor(clusterId: String, members: Seq[ReviewMember]): Option[ReviewAnswer] =
      byId.get(clusterId).orElse(members.iterator.flatMap(m => byIdentity.get(m.identity)).nextOption())
  }
}
