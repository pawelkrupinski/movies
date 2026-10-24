package services.identity
package adapters

import org.bson.{BsonDocument, BsonInt32, BsonInt64, BsonString}
import org.mongodb.scala.bson.BsonArray
import services.identity.agreement.{AnswerFeed, FamilyAnswers, Showing, SourceHit, SourceRecord, VoterFamily}

import java.time.Clock
import scala.concurrent.duration.FiniteDuration
import scala.jdk.CollectionConverters._

/**
 * What the other film database FAMILIES answered ([[VoterFamily]]): each one's title and person searches' films and
 * its records of films, filed once fetched and read from here — a document not filed yet is a gap ([[Answer.Unknown]])
 * the agreement fill asks, never "no film". Kept long: a search's films are asked again only after
 * [[QuestionPolicies.FamilySearchAge]], a record after [[QuestionPolicies.FamilyRecordAge]] (a released film's year,
 * credits and running time do not move), and the stale answer is read meanwhile. One document per question, under
 * `<family>|title|<text>`, `<family>|director|<name>` and `<family>|record|<id>`; and a venue's programme on the
 * family's site under `<family>|showing|<venue>`, asked again after [[QuestionPolicies.FamilyShowingAge]] — a programme
 * moves daily.
 */
final class FamilyAnswerStore(docs: TmdbDocuments, clock: Clock, feed: AnswerFeed = new FiledAnswerFeed)
    extends services.identity.agreement.AnswerChanges {
  import FamilyAnswerStore._

  /** The agreement's view of the feed ([[AnswerFeed.agreementKeys]]): every answer this store files, and every other one
   *  an agreement question names that is published there — the stage's verdicts hold while its version does not move. */
  val changes: AnswerFeed.View = feed.view(AnswerFeed.agreementKeys)
  /** How many agreement answers were filed since this store was made. */
  def version: Long = changes.version

  /** Announces a filing under `id` that another store made — a TMDB record read again for its release day: the stage's
   *  verdicts hold while the version does not move, so it must. Counted only when an agreement question names `id`. */
  def noteFiled(id: String): Unit = feed.publish(id)

  /** The questions filed after `version`, or `None` once filings that old are no longer kept. */
  def changedSince(version: Long): Option[Set[String]] = changes.changedSince(version)

  /** `family`'s answers, as the agreement reads them. */
  def answers(of: VoterFamily): FamilyAnswers = new FamilyAnswers {
    val family: VoterFamily = of
    def titled(text: String): Answer[Seq[SourceHit]]     = hits(titleId(family, text))
    def directedBy(name: String): Answer[Seq[SourceHit]] = hits(directorId(family, name))
    def record(id: String): Answer[Option[SourceRecord]] =
      document(recordId(family, id)).map(recordOf).toAnswer
    override def showing(venue: String): Answer[Seq[Showing]] =
      document(showingId(family, venue)).map(showingsOf).toAnswer
    override def fresh(question: String): Boolean = !wanted(questionId(family, question))
  }

  def fileTitled(family: VoterFamily, text: String, found: Seq[SourceHit]): Unit     = put(titleId(family, text), hitsDoc(found))
  def fileDirected(family: VoterFamily, name: String, found: Seq[SourceHit]): Unit   = put(directorId(family, name), hitsDoc(found))
  def fileRecord(family: VoterFamily, id: String, found: Option[SourceRecord]): Unit = put(recordId(family, id), recordDoc(found))
  def fileShowing(family: VoterFamily, venue: String, found: Seq[Showing]): Unit     = put(showingId(family, venue), showingsDoc(found))

  /** Is the question's answer missing, or older than its kind keeps it (the policy's `FamilySearchAge`, `FamilyRecordAge`,
   *  `FamilyShowingAge`)? */
  def wanted(id: String): Boolean = document(id).forall(TmdbStore.olderThan(_, ageOf(id), clock))

  private def hits(id: String): Answer[Seq[SourceHit]] =
    document(id).map(hitsOf).toAnswer
  // Read as an answer: kept by the worker's document cache (`CachedTmdbDocuments`) until a filing replaces it — a whole-
  // document `get` asks Mongo each time, and the stage checks each waiting cluster's questions on every projection.
  private[identity] def document(id: String): Option[BsonDocument] = docs.answers(TmdbKind.Family, Seq(id)).get(id)
  /** Files `d` under `id`, stamped and published as every answer is: what [[PosterAnswerStore]] files the posters' hashes
   *  by, so one version and one [[changedSince]] cover everything the agreement reads. */
  private[identity] def put(id: String, d: BsonDocument): Unit = {
    docs.put(TmdbKind.Family, Seq(id -> d.append(TmdbStore.FetchedAt, BsonInt64(clock.millis()))))
    feed.publish(id)
  }
}

object FamilyAnswerStore {
  /** How long the answer filed under `id` is read before it is asked again, as [[QuestionPolicies]] sets it for the
   *  question's kind. */
  def ageOf(id: String): FiniteDuration =
    if (id.contains("|record|")) QuestionPolicies.FamilyRecordAge
    else if (id.contains("|showing|")) QuestionPolicies.FamilyShowingAge
    else QuestionPolicies.FamilySearchAge

  def titleId(family: VoterFamily, text: String): String    = s"${family.label}|title|$text"
  def directorId(family: VoterFamily, name: String): String = s"${family.label}|director|$name"
  def recordId(family: VoterFamily, id: String): String     = s"${family.label}|record|$id"
  def showingId(family: VoterFamily, venue: String): String = s"${family.label}|showing|$venue"
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
    BsonFields.documentsOf(d, "hits").map { h =>
      SourceHit(h.getString("id").getValue, h.getString("title").getValue, Option(h.get("originalTitle")).map(_.asString.getValue),
        Option(h.get("year")).map(_.asInt32.getValue))
    }

  /** A programme as two parallel arrays — each film's id, and its days as epoch days — the fields the join reads. */
  private def showingsDoc(found: Seq[Showing]): BsonDocument =
    new BsonDocument("films", BsonArray.fromIterable(found.map(showing => BsonString(showing.film))))
      .append("days", BsonArray.fromIterable(found.map(showing => BsonArray.fromIterable(showing.days.days.map(day => BsonInt32(day.toEpochDay.toInt))))))
  private def showingsOf(d: BsonDocument): Seq[Showing] =
    BsonFields.elementsOf(d, "films").zip(d.getArray("days").getValues.asScala).map { case (film, days) =>
      Showing(film.asString.getValue, ScreeningDays.of(days.asArray.getValues.asScala.map(day => java.time.LocalDate.ofEpochDay(day.asInt32.getValue.toLong))))
    }

  private def recordDoc(found: Option[SourceRecord]): BsonDocument = found.fold(new BsonDocument("record", org.bson.BsonNull()))(r =>
    new BsonDocument("record", IdentityAnswerBson.film(Some(r.film)))
      .append("crossIds", new BsonDocument(r.crossIds.toSeq.sortBy(_._1).map { case (k, v) => new org.bson.BsonElement(k, BsonString(v)) }.asJava)))
  private def recordOf(d: BsonDocument): Option[SourceRecord] =
    Option(d.get("record")).flatMap(IdentityAnswerBson.filmOf).map(film => SourceRecord(film,
      Option(d.get("crossIds")).filter(_.isDocument).fold(Map.empty[String, String])(_.asDocument.asScala.map { case (k, v) => k -> v.asString.getValue }.toMap)))
}
