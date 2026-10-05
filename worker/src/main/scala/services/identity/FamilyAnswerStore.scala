package services.identity

import org.bson.{BsonDocument, BsonInt32, BsonInt64, BsonString}
import org.mongodb.scala.bson.BsonArray
import services.identity.agreement.{FamilyAnswers, SourceHit, SourceRecord, VoterFamily}

import java.time.Clock
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * What the other film database FAMILIES answered ([[VoterFamily]]): each one's title and person searches' films and
 * its records of films, filed once fetched and read from here — a document not filed yet is a gap ([[Answer.Unknown]])
 * the agreement fill asks, never "no film". Kept long: a search's films are asked again only after [[SearchAge]], a
 * record after [[RecordAge]] (a released film's year, credits and running time do not move), and the stale answer is
 * read meanwhile. One document per question, under `<family>|title|<text>`, `<family>|director|<name>` and
 * `<family>|record|<id>`.
 */
final class FamilyAnswerStore(docs: TmdbDocuments, clock: Clock) extends services.identity.agreement.AnswerChanges {
  import FamilyAnswerStore._

  private val filed = new java.util.concurrent.atomic.AtomicLong()
  /** How many answers this store filed since it was made: a reader's verdicts over them hold while it does not move. */
  def version: Long = filed.get

  /** Each filing's question id, by the version it made — what [[changedSince]] answers from; the oldest dropped past
   *  [[FamilyAnswerStore.ChangesKept]], a version before them then unknown. */
  private val changes = new java.util.concurrent.ConcurrentSkipListMap[Long, String]()
  /** The questions filed after `version`, or `None` once filings that old are no longer kept. */
  def changedSince(version: Long): Option[Set[String]] =
    Option.when(changes.isEmpty || version >= changes.firstKey - 1 || version >= filed.get)(
      changes.tailMap(version, false).values.asScala.toSet)

  /** `family`'s answers, as the agreement reads them. */
  def answers(of: VoterFamily): FamilyAnswers = new FamilyAnswers {
    val family: VoterFamily = of
    def titled(text: String): Answer[Seq[SourceHit]]     = hits(titleId(family, text))
    def directedBy(name: String): Answer[Seq[SourceHit]] = hits(directorId(family, name))
    def record(id: String): Answer[Option[SourceRecord]] =
      document(recordId(family, id)).fold[Answer[Option[SourceRecord]]](Answer.Unknown)(d => Answer.Known(recordOf(d)))
    override def fresh(question: String): Boolean = !wanted(questionId(family, question))
  }

  def fileTitled(family: VoterFamily, text: String, found: Seq[SourceHit]): Unit     = put(titleId(family, text), hitsDoc(found))
  def fileDirected(family: VoterFamily, name: String, found: Seq[SourceHit]): Unit   = put(directorId(family, name), hitsDoc(found))
  def fileRecord(family: VoterFamily, id: String, found: Option[SourceRecord]): Unit = put(recordId(family, id), recordDoc(found))

  /** Is the question's answer missing, or older than its kind keeps it ([[SearchAge]], [[RecordAge]])? */
  def wanted(id: String): Boolean = document(id).forall { d =>
    val age = Option(d.get(TmdbStore.FetchedAt)).filter(_.isInt64).map(at => clock.millis() - at.asInt64.getValue)
    age.forall(_ > (if (id.contains("|record|")) RecordAge else SearchAge).toMillis)
  }

  private def hits(id: String): Answer[Seq[SourceHit]] =
    document(id).fold[Answer[Seq[SourceHit]]](Answer.Unknown)(d => Answer.Known(hitsOf(d)))
  // Read as an answer: kept by the worker's document cache (`CachedTmdbDocuments`) until a filing replaces it — a whole-
  // document `get` asks Mongo each time, and the stage checks each waiting cluster's questions on every projection.
  private[identity] def document(id: String): Option[BsonDocument] = docs.answers(TmdbKind.Family, Seq(id)).get(id)
  /** Files `d` under `id`, stamped and counted as every answer is: what [[PosterAnswerStore]] files the posters' hashes by,
   *  so one version and one [[changedSince]] cover everything the agreement reads. */
  private[identity] def put(id: String, d: BsonDocument): Unit = {
    docs.put(TmdbKind.Family, Seq(id -> d.append(TmdbStore.FetchedAt, BsonInt64(clock.millis()))))
    changes.put(filed.incrementAndGet(), id)
    while (changes.size > FamilyAnswerStore.ChangesKept) changes.pollFirstEntry()
    ()
  }
}

object FamilyAnswerStore {
  /** How long a search's films are read before the fill asks again: venues list a title for weeks, and a new film
   *  joining a search is what a re-ask would find. */
  val SearchAge: FiniteDuration = 90.days
  /** How long a film's record is read before the fill asks again: a released film's facts do not move. */
  val RecordAge: FiniteDuration = 365.days
  /** How many filings [[FamilyAnswerStore.changedSince]] can name: a burst past it re-reads the verdicts' answers. */
  val ChangesKept = 100000

  def titleId(family: VoterFamily, text: String): String    = s"${family.label}|title|$text"
  def directorId(family: VoterFamily, name: String): String = s"${family.label}|director|$name"
  def recordId(family: VoterFamily, id: String): String     = s"${family.label}|record|$id"
  /** The document id of `question` as the agreement names it (`title|<text>`, `director|<name>`, `record|<id>`). */
  def questionId(family: VoterFamily, question: String): String = s"${family.label}|$question"

  private def hitsDoc(found: Seq[SourceHit]): BsonDocument =
    new BsonDocument("hits", BsonArray.fromIterable(found.map { hit =>
      val d = new BsonDocument("id", BsonString(hit.id)).append("title", BsonString(hit.title))
      hit.originalTitle.foreach(t => d.append("originalTitle", BsonString(t)))
      hit.year.foreach(y => d.append("year", BsonInt32(y)))
      d
    }))
  private def hitsOf(d: BsonDocument): Seq[SourceHit] =
    Option(d.get("hits")).filter(_.isArray).toSeq.flatMap(_.asArray.getValues.asScala.map(_.asDocument)).map { h =>
      SourceHit(h.getString("id").getValue, h.getString("title").getValue, Option(h.get("originalTitle")).map(_.asString.getValue),
        Option(h.get("year")).map(_.asInt32.getValue))
    }

  private def recordDoc(found: Option[SourceRecord]): BsonDocument = found.fold(new BsonDocument("record", org.bson.BsonNull()))(r =>
    new BsonDocument("record", IdentityAnswerBson.film(Some(r.film)))
      .append("crossIds", new BsonDocument(r.crossIds.toSeq.sortBy(_._1).map { case (k, v) => new org.bson.BsonElement(k, BsonString(v)) }.asJava)))
  private def recordOf(d: BsonDocument): Option[SourceRecord] =
    Option(d.get("record")).flatMap(IdentityAnswerBson.filmOf).map(film => SourceRecord(film,
      Option(d.get("crossIds")).filter(_.isDocument).fold(Map.empty[String, String])(_.asDocument.asScala.map { case (k, v) => k -> v.asString.getValue }.toMap)))
}
