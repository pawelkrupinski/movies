package services.identity.agreement

import services.identity.{Answer, Film, IdentityMeasures, VoterFamily}

import scala.annotation.unused

/** One film a family's search names: the family's own id for it, and its title and year as the search gave them. */
final case class SourceHit(id: String, title: String, originalTitle: Option[String], year: Option[Int])

/** A family's record of a film: what the identity measures read of it, and the ids other databases know it by
 *  (`"imdb" -> "tt0087843"`, `"tmdb" -> "311"`) — what links two families' films without comparing their facts. */
final case class SourceRecord(film: Film, crossIds: Map[String, String] = Map.empty)

object SourceRecord {
  /** TMDB's record of film `tmdbId`: linked by its TMDB id, and by the IMDb id TMDB names, if any. */
  def ofTmdb(tmdbId: Int, film: Film): SourceRecord =
    SourceRecord(film, Map("tmdb" -> tmdbId.toString) ++ IdentityMeasures.imdbId(film.imdbNumber).map("imdb" -> _))

  /** The TMDB film a no-match leans to, as the agreement links it: by its TMDB and IMDb ids alone. */
  def ofLeaning(lean: services.identity.ResolverDecision.Leaning): SourceRecord =
    SourceRecord(Film(""), Map("tmdb" -> lean.film.toString) ++ IdentityMeasures.imdbId(lean.imdbNumber).map("imdb" -> _))
}

/** One film a venue's programme on a family's site screens — the family's own id for it — and the days it does. */
final case class Showing(film: String, days: services.identity.ScreeningDays)

/** What a family answers, from what it keeps: `Unknown` while a question is not answered yet — a gap, never "no film". */
trait FamilyAnswers {
  def family: VoterFamily
  def titled(text: String): Answer[Seq[SourceHit]]
  /** The films a person of this name directed; `Known(Nil)` for a family with no person search. */
  def directedBy(name: String): Answer[Seq[SourceHit]]
  def record(id: String): Answer[Option[SourceRecord]]
  /** The films the family's site lists a venue screening ([[VenueListings]]): its own programme of the venue, else its
   *  town's; `Known(Nil)` for a family that lists no programmes. */
  def showing(@unused venue: String): Answer[Seq[Showing]] = Answer.Known(Nil)
  /** Is there an answer to `question` (`title|<text>`, `director|<name>`, `record|<id>`, `showing|<venue>`), and is it
   *  still fresh? A stale one is read all the same, and asked again; a missing one is a gap. */
  def fresh(question: String): Boolean = true
}

/** Which family answers were filed since a version of them (`<family>|<kind>|<text>`, as a verdict's reads name them):
 *  `None` when that is not known — the stage then reads a verdict's answers again to tell. */
trait AnswerChanges { def changedSince(version: Long): Option[Set[String]] }
object AnswerChanges { val Unknown: AnswerChanges = _ => None }

/**
 * The one feed answers are announced on, whichever store filed them: a key per filing (a TMDB document's
 * `<collection>:<id>`, a family answer's `<family>|<kind>|<text>`, a venue page's detail key). Each store decides what
 * counts as a filing — the TMDB store a document whose value moved, the family store every answer it files, the venue
 * page index a page whose answer moved at a settle — and publishes it here. Readers subscribe (called on the publishing
 * thread, in order: what the model reads of the asking thread, `ObservationReads.changed`, is that thread's), or count
 * the keys they read through a [[AnswerFeed.View]] of their own.
 */
trait AnswerFeed {
  /** Announces a filing under `key`: counted by every view accepting it, then handed to every subscriber, on this thread. */
  def publish(key: String): Unit
  /** Calls `listener` with every key published from now on. */
  def subscribe(listener: String => Unit): Unit
  /** The filings `read` takes, each under the key `read` names it by (the key itself, or the id its reader asks it
   *  under), numbered by a version of the view's own: one it does not take (`None`) moves nothing. */
  def view(read: String => Option[String]): AnswerFeed.View
}
object AnswerFeed {
  /** One reader's count of the filings it reads ([[version]]), and which ones came since a version ([[changedSince]]). */
  trait View extends AnswerChanges { def version: Long }
  /** The keys the agreement stage reads: those an agreement question names ([[services.identity.Question.parse]]) —
   *  never a TMDB document's or a venue page's, so the model's filings leave its version where it was. */
  val agreementKeys: String => Option[String] = key => Option.when(services.identity.Question.parse(key).isDefined)(key)
}
