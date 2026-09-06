package services.resolution

import models.SourceData
import services.movies.{RuntimeCorroboration, SamePerson}

/**
 * A film a resolution might conclude, as TMDB describes it — the CLAIM the
 * cinemas' [[FilmEvidence]] is weighed against.
 *
 * `crew` is everyone a venue could plausibly print as "the director": TMDB's
 * directors AND writers, because cinemas credit either ("Drzewo magii" is directed
 * by Ben Gregor and written by Simon Farnaby, and venues print one or the other).
 * A candidate reached by walking a person's own filmography carries that person in
 * its crew by construction.
 */
final case class Candidate(
  tmdbId:  Int,
  titles:  Set[String]    = Set.empty,
  year:    Option[Int]    = None,
  runtime: Option[Int]    = None,
  crew:    Seq[String]    = Nil,
  cast:    Seq[String]    = Nil
)

object Candidate {
  /** The film a row is currently resolved to, read off its own `Tmdb` slot. */
  def fromSlot(tmdbId: Int, slot: SourceData): Candidate =
    Candidate(tmdbId,
      titles  = Set(slot.title, slot.originalTitle, slot.englishTitle).flatten,
      year    = slot.releaseYear,
      runtime = slot.runtimeMinutes,
      crew    = slot.director,
      cast    = slot.cast)
}

/** What the cinemas disagree with a candidate about. */
enum Contradiction { case Runtime, Director }

/** Is this candidate the film the cinemas are showing? */
enum Verdict {
  /** The cinemas' own evidence supports it: a credited person agrees, or the
   *  published minutes are compatible and nobody contradicts. */
  case Accept(support: Support)
  /** The cinemas positively contradict it. A `Director` rejection compares NAMES,
   *  and a name can disagree for reasons that are not a different film (a
   *  pseudonym, the film's other director); a caller about to SPEND on it should
   *  confirm against TMDB's crew ids first (`CrewConfirmation`). */
  case Reject(reason: Contradiction)
  /** Nothing comparable on either side. */
  case Insufficient

  def isReject: Boolean = this match { case Reject(_) => true; case _ => false }
}

/** Which evidence carried an [[Verdict.Accept]]. */
enum Support { case Crew, Runtime }

/**
 * The ONE answer to "is candidate X the film these cinemas are showing?", asked
 * by the sweep's contradiction check, the event-time re-verify of a resolved row,
 * and the resolver's veto of a search hit. Each used to ask it in its own
 * vocabulary, and they disagreed: the resolver vetoed a director-walk hit on
 * runtime alone, while the sweep let an agreeing director settle a runtime
 * mismatch — Almodóvar's 30-minute "The Human Voice" advertised at 90 with a Q&A.
 *
 * The rule, in order:
 *   1. A cinema-credited name on the candidate's crew is the strongest evidence
 *      either side carries, and it is final — a short film in a longer slot is
 *      still the film.
 *   2. Failing that, minutes a category apart deny the candidate
 *      (`RuntimeCorroboration.plausible`: nothing in a cinema's rounding turns
 *      110 into 18).
 *   3. Then a credited name that matches nobody on the crew denies it, subject to
 *      the caller confirming (see [[Verdict.Reject]]).
 *   4. Otherwise the runtimes are compatible or absent: accept on runtime, or
 *      abstain when neither side published anything comparable.
 *
 * Both signals demand POSITIVE contradiction: a venue that published nothing is
 * not disagreeing, and a credit that folds away entirely (a CJK name) compares as
 * nothing rather than as a stranger.
 */
object Verdict {

  def of(evidence: FilmEvidence, candidate: Candidate): Verdict = {
    val cinemaNames = evidence.directors.map(SamePerson.tokens).filter(_.nonEmpty)
    val crewNames   = candidate.crew.map(SamePerson.tokens).filter(_.nonEmpty)
    val namesAgree  = Option.when(cinemaNames.nonEmpty && crewNames.nonEmpty)(
      cinemaNames.exists(c => crewNames.exists(SamePerson.sameTokens(c, _))))
    val runtimePlausible = RuntimeCorroboration.plausible(evidence.runtimes, candidate.runtime)
    namesAgree match {
      case Some(true)                => Accept(Support.Crew)
      case _ if !runtimePlausible    => Reject(Contradiction.Runtime)
      case Some(false)               => Reject(Contradiction.Director)
      case None if evidence.runtimes.nonEmpty && candidate.runtime.nonEmpty => Accept(Support.Runtime)
      case None                      => Insufficient
    }
  }
}
