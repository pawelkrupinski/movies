package services.identity

import org.bson.{BsonDocument, BsonInt64, BsonNull}
import org.mongodb.scala.bson.BsonArray
import services.identity.agreement.AgreementStage.PosterQuestion
import services.sharecards.{PosterDownload, PosterFailure, PosterShrinker}
import services.tasks.HandlerOutcome.{Done, Skipped}
import services.tasks.{HandlerOutcome, Task, TaskHandler, TaskType}

import java.nio.file.Files
import java.time.Clock
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * The posters' hashes the agreement stage reads ([[PosterEvidence]]): a venue's poster by its URL, and a TMDB film's
 * posters — hashes only, never the images — filed once hashed and read from here; a poster not filed yet is a gap
 * ([[Answer.Unknown]]) the poster fill asks, never "no poster". Filed among the other families' answers (`answers`:
 * `identity_family_answers`, kept long, and counted in its `version`), under `poster|venue|<url>` and
 * `poster|film|<tmdbId>`, and hashed again only after [[PosterAnswerStore.Age]]: an image at one URL, and a released
 * film's artwork, rarely change.
 */
final class PosterAnswerStore(answers: FamilyAnswerStore, clock: Clock) extends PosterAnswers {
  import PosterAnswerStore._
  import PosterAnswers.idOf

  def venue(url: String): Answer[Option[PosterHash]] =
    document(idOf(PosterQuestion.Venue(url))).fold[Answer[Option[PosterHash]]](Answer.Unknown)(d => Answer.Known(hashesOf(d).headOption))
  def film(tmdbId: Int): Answer[Seq[PosterHash]] =
    document(idOf(PosterQuestion.Film(tmdbId))).fold[Answer[Seq[PosterHash]]](Answer.Unknown)(d => Answer.Known(hashesOf(d)))

  /** `question`'s hashes, filed: one at most for a venue's poster, a film's up to [[PosterEvidence.FilmPosters]]. A venue
   *  poster with none was not READ — its link fetches nothing, the origin refused it, no decoder reads it — and a failed
   *  read is no data: it is [[giveUp given up on]]. A film with none is one TMDB keeps no poster of. */
  def file(question: PosterQuestion, hashes: Seq[PosterHash]): Unit = question match {
    case PosterQuestion.Venue(_) if hashes.isEmpty => giveUp(question)
    case _ =>
      answers.put(idOf(question), new BsonDocument("hashes", if (hashes.isEmpty) BsonNull() else BsonArray.fromIterable(hashes.map(h => BsonInt64(h.bits)))))
  }

  /** `question`'s poster GIVEN UP on: not read — refused, unreadable, or its fetch failed every attempt the queue allowed
   *  it ([[GiveUpAttempts]]) — so it is filed as no poster and marked [[Unread]]: no evidence either way
   *  ([[PosterAnswers.unread]]), never a gap waited on for ever, and asked again after [[UnreadAge]]. */
  def giveUp(question: PosterQuestion): Unit =
    answers.put(idOf(question), new BsonDocument("hashes", BsonNull()).append(Unread, org.bson.BsonBoolean.TRUE))

  override def unread(question: PosterQuestion): Boolean = document(idOf(question)).exists(isUnread)

  override def fresh(question: PosterQuestion): Boolean = !wanted(question)

  /** Is the question's answer missing, or older than [[Age]] — [[UnreadAge]] for a venue poster with no hash: one given
   *  up on, or one filed so before a failed read was told from data (prod PL 2026-10-06: all 226 biletyna.pl posters,
   *  refused by the origin, filed as "no poster" with no unread mark, for a year)? */
  def wanted(question: PosterQuestion): Boolean = document(idOf(question)).forall { d =>
    val unreadVenue = isUnread(d) || (question.isInstanceOf[PosterQuestion.Venue] && hashesOf(d).isEmpty)
    val age         = if (unreadVenue) UnreadAge else Age
    Option(d.get(TmdbStore.FetchedAt)).filter(_.isInt64).forall(at => clock.millis() - at.asInt64.getValue > age.toMillis)
  }

  private def document(id: String): Option[BsonDocument] = answers.document(id)
}

object PosterAnswerStore {
  /** How long a hash is read before the fill hashes the poster again. */
  val Age: FiniteDuration = 365.days
  /** How long a poster given up on is read as unread before the fill tries it again. */
  val UnreadAge: FiniteDuration = 7.days
  /** The attempt on which a poster whose fetch keeps failing is given up on: about an hour and a half of the queue's
   *  backoff (Kino Kryterium's posters, 2026-10-06: ten attempts between 14:55 and 16:09). */
  val GiveUpAttempts = 8
  private val Unread = "unread"

  private def isUnread(d: BsonDocument): Boolean = d.getBoolean(Unread, org.bson.BsonBoolean.FALSE).getValue

  private def hashesOf(d: BsonDocument): Seq[PosterHash] =
    Option(d.get("hashes")).filter(_.isArray).toSeq.flatMap(_.asArray.getValues.asScala.map(v => PosterHash(v.asInt64.getValue)))
}

/**
 * Hashes a poster: downloaded through `download` (the enrichment fetch chain, with its pacing and breakers), decoded and
 * cut to the card's 2:3 slot by `shrinker` (under the process's one decode gate), hashed ([[PosterHash]]) — the image is
 * never kept. A TMDB film's posters are its `/images` artwork (`images`) in its country's language, English and none,
 * by votes, at TMDB's 185-wide print.
 *
 * A poster the origin refuses, or no decoder can read, is not read (`None`: filed unread, asked again after
 * [[PosterAnswerStore.UnreadAge]]); a failure that may pass — a timeout,
 * a 5xx, the network — THROWS, for the queue to ask again on its backoff. A venue's link is fetched escaped as a card
 * serves it ([[services.movies.SlotFields.url]]: a raw space no fetch takes is no network failure), and a film TMDB
 * answers durably gone ([[tools.HttpStatusException.isDurable]]) has no posters.
 */
final class PosterHashing(download: PosterDownload, shrinker: PosterShrinker, images: Int => Seq[clients.TmdbClient.PosterImage], language: String) {

  def venue(url: String): Option[PosterHash] = services.movies.SlotFields.url(url).flatMap(hash)

  def film(tmdbId: Int): Seq[PosterHash] = {
    val posters = try images(tmdbId) catch { case e: tools.HttpStatusException if tools.HttpStatusException.isDurable(e.code) => Nil }
    PosterHashing.chosen(posters, language).flatMap(path => hash(s"${clients.TmdbClient.PosterHashBase}$path"))
  }

  private def hash(url: String): Option[PosterHash] = download.fetch(url) match {
    case Left(reason) if PosterHashing.Passing(reason) => throw new java.io.IOException(s"poster $url: $reason")
    case Left(_)                                       => None
    case Right(file) =>
      try shrinker.coverSlot(file) match {
        case Right(slot) => Some(PosterHash.of(slot))
        case Left(reason) if PosterHashing.Passing(reason) => throw new java.io.IOException(s"poster $url: $reason")
        case Left(_)     => None
      } finally Files.deleteIfExists(file)
  }
}

object PosterHashing {
  /** The failures that may pass: asked again, never filed as no poster. */
  val Passing: Set[String] = Set(PosterFailure.Timeout, PosterFailure.Network, PosterFailure.Http5xx, PosterFailure.HttpOther)

  /** The paths of a film's posters to hash: its country's `language` first, then English, then language-neutral, then
   *  the rest, each by votes — [[PosterEvidence.FilmPosters]] of them. */
  def chosen(images: Seq[clients.TmdbClient.PosterImage], language: String): Seq[String] = {
    def order(image: clients.TmdbClient.PosterImage) = image.language match {
      case Some(l) if l == language => 0
      case Some("en")               => 1
      case None                     => 2
      case _                        => 3
    }
    images.sortBy(image => (order(image), -image.voteCount, image.filePath)).map(_.filePath).distinct.take(PosterEvidence.FilmPosters)
  }
}

/** One poster to hash, as a queue task ([[agreement.AgreementStage.PosterQuestion]]): hashed and filed — skipped while
 *  the store holds a fresh hash — and, once filed, a projection asked for (`filed`), which reads it. */
final class AgreementPosterHandler(store: PosterAnswerStore, hashing: PosterHashing, filed: () => Unit, clock: Clock,
                                   metrics: AgreementQuestionMetrics = AgreementQuestionMetrics.Silent) extends TaskHandler {
  val taskType: TaskType = TaskType.AgreementPoster

  def handle(task: Task): HandlerOutcome = AgreementQuestions.posterOf(task).fold[HandlerOutcome](Skipped) { question =>
    if (!store.wanted(question)) { metrics.asked(AgreementQuestionMetrics.Poster, AgreementQuestionMetrics.Fresh); Skipped }
    else {
      val outcome = try {
        store.file(question, question match {
          case PosterQuestion.Venue(url)   => hashing.venue(url).toSeq
          case PosterQuestion.Film(tmdbId) => hashing.film(tmdbId)
        })
        filed(); Done
      } catch { case scala.util.control.NonFatal(e) => AgreementQuestions.failed(e, s"poster ${PosterAnswers.idOf(question)}", clock) }
      metrics.asked(AgreementQuestionMetrics.Poster, outcome match {
        case Done                   => AgreementQuestionMetrics.Answered
        case _: HandlerOutcome.Deferred => AgreementQuestionMetrics.Deferred
        case _                      => AgreementQuestionMetrics.Failed
      })
      outcome match {
        // its last attempt failed too: given up on, read as unread rather than waited on for ever
        case _: HandlerOutcome.Reschedule if task.attempts >= PosterAnswerStore.GiveUpAttempts => store.giveUp(question); filed(); Done
        case other => other
      }
    }
  }
}
