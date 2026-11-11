package services.identity
package adapters

import org.bson.{BsonDocument, BsonInt32, BsonInt64, BsonNull}
import org.mongodb.scala.bson.BsonArray
import services.identity.agreement.AgreementStage.PosterQuestion
import services.sharecards.{PosterDownload, PosterFailure, PosterShrinker}

import java.nio.file.Files
import java.time.Clock

/**
 * The posters' hashes the agreement stage reads ([[PosterEvidence]]): a venue's poster by its URL, and a TMDB film's
 * posters — hashes only, never the images — filed once hashed and read from here; a poster not filed yet is a gap
 * ([[Answer.Unknown]]) the poster fill asks, never "no poster". Filed among the other families' answers (`answers`:
 * `identity_family_answers`, kept long, and counted in its `version`), under `poster|venue|<url>` and
 * `poster|film|<tmdbId>`, and hashed again only after [[QuestionPolicies.PosterAge]]: an image at one URL, and a released
 * film's artwork, rarely change — or, a film's, once [[PosterEvidence.FilmPosters]] rose past the cap a document holding
 * that many was filed under. A film's document keeps beside its hashes the TMDB paths they were read from
 * ([[PosterAnswers.Paths]]), for the review pages to show and for a film hashed again to download only the posters TMDB
 * added; the agreement reads only the hashes. A film `released` (its TMDB record's day) within
 * [[QuestionPolicies.YoungFilmWindow]] of its filing is marked YOUNG and asked again after
 * [[QuestionPolicies.YoungPosterAge]]: TMDB gets a new film's local artwork around its release.
 */
final class PosterAnswerStore(answers: FamilyAnswerStore, clock: Clock, released: Int => Option[java.time.LocalDate] = _ => None)
    extends PosterAnswers {
  import PosterAnswerStore._
  import PosterAnswers.idOf

  def venue(url: String): Answer[Option[PosterHash]] =
    document(idOf(PosterQuestion.Venue(url))).map(d => hashesOf(d).headOption).toAnswer
  def film(tmdbId: Int): Answer[Seq[PosterHash]] =
    document(idOf(PosterQuestion.Film(tmdbId))).map(hashesOf).toAnswer

  /** `question`'s posters, filed: one at most for a venue's poster, a film's up to [[PosterEvidence.FilmPosters]], and a
   *  film's each with the TMDB path it was read from. A venue poster with none was not READ — its link fetches nothing,
   *  the origin refused it, no decoder reads it — and a failed read is no data: it is [[giveUp given up on]]. A film
   *  with none is one TMDB keeps no poster of. */
  def file(question: PosterQuestion, posters: Seq[HashedPoster]): Unit = question match {
    case PosterQuestion.Venue(_) if posters.isEmpty => giveUp(question)
    case _ =>
      val d = new BsonDocument("hashes", if (posters.isEmpty) BsonNull() else BsonArray.fromIterable(posters.map(p => BsonInt64(p.hash.bits))))
      // a venue's poster is filed under its URL already
      // and the cap they were chosen under: TMDB may keep more than a cap's worth, read once the cap is raised ([[cut]])
      if (question.isInstanceOf[PosterQuestion.Film] && posters.nonEmpty)
        d.append(PosterAnswers.Paths, PosterAnswers.pathsValue(posters.map(_.path))).append(Cap, BsonInt32(PosterEvidence.FilmPosters))
      question match {
        case PosterQuestion.Film(tmdbId) if young(tmdbId) => d.append(Young, org.bson.BsonBoolean.TRUE)
        case _                                            => ()
      }
      answers.put(idOf(question), d)
  }

  /** A film's posters as filed, each with the TMDB path it was read from: what hashing it again reuses ([[PosterHashing.film]]).
   *  None for a venue's poster, or a film's filed before its paths were. */
  def filed(question: PosterQuestion): Seq[HashedPoster] = question match {
    case PosterQuestion.Venue(_) => Nil
    case PosterQuestion.Film(_) => document(idOf(question)).toSeq.flatMap { d =>
      val (paths, hashes) = (PosterAnswers.pathsOf(d), hashesOf(d))
      if (paths.sizeIs == hashes.size) paths.zip(hashes).map(HashedPoster.apply) else Nil
    }
  }

  /** `question`'s poster GIVEN UP on: not read — refused, unreadable, or its fetch failed
   *  [[QuestionPolicies.PosterGiveUpAttempts]] times in a row — so it is filed as no poster and marked [[Unread]]: no
   *  evidence either way ([[PosterAnswers.unread]]), never a gap waited on for ever, and asked again after
   *  [[QuestionPolicies.PosterUnreadAge]]. */
  def giveUp(question: PosterQuestion): Unit =
    answers.put(idOf(question), new BsonDocument("hashes", BsonNull()).append(Unread, org.bson.BsonBoolean.TRUE))

  override def unread(question: PosterQuestion): Boolean = document(idOf(question)).exists(isUnread)

  override def fresh(question: PosterQuestion): Boolean = !wanted(question)

  /** Is the question's answer missing, cut at a lower cap than [[PosterEvidence.FilmPosters]] ([[cut]]), a young film's
   *  older than [[QuestionPolicies.YoungPosterAge]], or older than [[QuestionPolicies.PosterAge]] — [[QuestionPolicies.PosterUnreadAge]]
   *  for a venue poster with no hash: one given up on, or one filed so before a failed read was told from data (prod PL
   *  2026-10-06: all 226 biletyna.pl posters, refused by the origin, filed as "no poster" with no unread mark, for a
   *  year)? */
  def wanted(question: PosterQuestion): Boolean = document(idOf(question)).forall { d =>
    val unreadVenue = isUnread(d) || (question.isInstanceOf[PosterQuestion.Venue] && hashesOf(d).isEmpty)
    val age         = if (unreadVenue) QuestionPolicies.PosterUnreadAge else QuestionPolicies.PosterAge
    TmdbStore.olderThan(d, age, clock) || (question.isInstanceOf[PosterQuestion.Film] && (cut(d) ||
      (d.getBoolean(Young, org.bson.BsonBoolean.FALSE).getValue && TmdbStore.olderThan(d, QuestionPolicies.YoungPosterAge, clock))))
  }

  /** Is the film released within [[QuestionPolicies.YoungFilmWindow]] of today, either side? */
  private def young(tmdbId: Int): Boolean = released(tmdbId).exists { day =>
    val today = java.time.LocalDate.ofInstant(clock.instant(), java.time.ZoneOffset.UTC)
    math.abs(java.time.temporal.ChronoUnit.DAYS.between(day, today)) <= QuestionPolicies.YoungFilmWindow.toDays
  }

  private def document(id: String): Option[BsonDocument] = answers.document(id)
}

object PosterAnswerStore {
  private val Unread = "unread"

  private def isUnread(d: BsonDocument): Boolean = d.getBoolean(Unread, org.bson.BsonBoolean.FALSE).getValue

  /** The cap a film's posters were chosen under ([[PosterEvidence.FilmPosters]] at its filing); a document filed unmarked
   *  was filed under 8 (to 2026-11-11). */
  private val Cap = "cap"
  private val UnmarkedCap = 8
  /** A film's document filed within [[QuestionPolicies.YoungFilmWindow]] of its release. */
  private val Young = "young"

  /** Was the film's document filed holding as many posters as a cap lower than today's: TMDB may keep more, hashed once
   *  asked again (those filed reused, [[PosterHashing.film]]). */
  private def cut(d: BsonDocument): Boolean = {
    val cap = d.getInt32(Cap, BsonInt32(UnmarkedCap)).getValue
    cap < PosterEvidence.FilmPosters && hashesOf(d).sizeIs >= cap
  }

  private def hashesOf(d: BsonDocument): Seq[PosterHash] =
    BsonFields.elementsOf(d, "hashes").map(v => PosterHash(v.asInt64.getValue))
}

/** A poster hashed: where it was read from — a venue's URL, a TMDB film poster's path — and its hash. */
final case class HashedPoster(path: String, hash: PosterHash)

/**
 * Hashes a poster: downloaded through `download` (the enrichment fetch chain, with its pacing and breakers), decoded and
 * cut to the card's 2:3 slot by `shrinker` (under the process's one decode gate), hashed ([[PosterHash]]) — the image is
 * never kept. A TMDB film's posters are its `/images` artwork (`images`) in its country's language, English and none,
 * by votes, at TMDB's 185-wide print.
 *
 * A poster the origin refuses, or no decoder can read, is not read (`None`: filed unread, asked again after
 * [[QuestionPolicies.PosterUnreadAge]]); a failure that may pass — a timeout, a 5xx, the network — THROWS, for the
 * asker to remember and ask again on the failure backoff ([[QuestionMemory]]). A venue's link is fetched escaped as a card
 * serves it ([[services.movies.SlotFields.url]]: a raw space no fetch takes is no network failure), and a film TMDB
 * answers durably gone ([[tools.HttpStatusException.isDurable]]) has no posters.
 */
final class PosterHashing(download: PosterDownload, shrinker: PosterShrinker, images: Int => Seq[clients.TmdbClient.PosterImage], language: String) {

  /** `question`'s poster hashed, or a film's posters — those `filed` already reused by their path. */
  def of(question: PosterQuestion, filed: Seq[HashedPoster] = Nil): Seq[HashedPoster] = question match {
    case PosterQuestion.Venue(url)   => venue(url).map(HashedPoster(url, _)).toSeq
    case PosterQuestion.Film(tmdbId) => film(tmdbId, filed)
  }

  def venue(url: String): Option[PosterHash] = services.movies.SlotFields.url(url).flatMap(hash)

  /** The film's posters TMDB keeps now, each path hashed once: a path `filed` already is its hash (a TMDB path names one
   *  image), so asking a film again downloads only the posters TMDB added since. */
  def film(tmdbId: Int, filed: Seq[HashedPoster] = Nil): Seq[HashedPoster] = {
    val posters = try images(tmdbId) catch { case e: tools.HttpStatusException if tools.HttpStatusException.isDurable(e.code) => Nil }
    PosterHashing.chosen(posters, language).flatMap(path =>
      filed.find(_.path == path).orElse(hash(s"${PosterAnswers.FilmPosterBase}$path").map(HashedPoster(path, _))))
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
