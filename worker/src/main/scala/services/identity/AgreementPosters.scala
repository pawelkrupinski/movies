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

  def venue(url: String): Answer[Option[PosterHash]] =
    document(idOf(PosterQuestion.Venue(url))).fold[Answer[Option[PosterHash]]](Answer.Unknown)(d => Answer.Known(hashesOf(d).headOption))
  def film(tmdbId: Int): Answer[Seq[PosterHash]] =
    document(idOf(PosterQuestion.Film(tmdbId))).fold[Answer[Seq[PosterHash]]](Answer.Unknown)(d => Answer.Known(hashesOf(d)))

  /** `question`'s hashes, filed: one at most for a venue's poster (none: it could not be read), a film's up to
   *  [[PosterEvidence.FilmPosters]]. */
  def file(question: PosterQuestion, hashes: Seq[PosterHash]): Unit = {
    answers.put(idOf(question), new BsonDocument("hashes", if (hashes.isEmpty) BsonNull() else BsonArray.fromIterable(hashes.map(h => BsonInt64(h.bits)))))
  }

  /** Is the question's answer missing, or older than [[Age]]? */
  def wanted(question: PosterQuestion): Boolean = document(idOf(question)).forall { d =>
    Option(d.get(TmdbStore.FetchedAt)).filter(_.isInt64).forall(at => clock.millis() - at.asInt64.getValue > Age.toMillis)
  }

  private def document(id: String): Option[BsonDocument] = answers.document(id)
}

object PosterAnswerStore {
  /** How long a hash is read before the fill hashes the poster again. */
  val Age: FiniteDuration = 365.days

  def idOf(question: PosterQuestion): String = question match {
    case PosterQuestion.Venue(url)    => s"poster|venue|$url"
    case PosterQuestion.Film(tmdbId)  => s"poster|film|$tmdbId"
  }

  private def hashesOf(d: BsonDocument): Seq[PosterHash] =
    Option(d.get("hashes")).filter(_.isArray).toSeq.flatMap(_.asArray.getValues.asScala.map(v => PosterHash(v.asInt64.getValue)))
}

/**
 * Hashes a poster: downloaded through `download` (the enrichment fetch chain, with its pacing and breakers), decoded and
 * cut to the card's 2:3 slot by `shrinker` (under the process's one decode gate), hashed ([[PosterHash]]) — the image is
 * never kept. A TMDB film's posters are its `/images` artwork (`images`) in its country's language, English and none,
 * by votes, at TMDB's 185-wide print.
 *
 * A poster the origin refuses, or no decoder can read, is no poster (`Right(None)`); a failure that may pass — a timeout,
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
      } catch { case scala.util.control.NonFatal(e) => AgreementQuestions.failed(e, s"poster ${PosterAnswerStore.idOf(question)}", clock) }
      metrics.asked(AgreementQuestionMetrics.Poster, outcome match {
        case Done                   => AgreementQuestionMetrics.Answered
        case _: HandlerOutcome.Deferred => AgreementQuestionMetrics.Deferred
        case _                      => AgreementQuestionMetrics.Failed
      })
      outcome
    }
  }
}
