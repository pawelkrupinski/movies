package services.tasks

import models.MovieRecord
import play.api.Logging
import services.movies.CinemaCorroboration
import services.movies.CinemaCorroboration.Contradiction

/**
 * Confirms a contradiction with TMDB before anything acts on it.
 *
 * `CinemaCorroboration` compares STRINGS, because a cinema publishes a name and
 * nothing else. Six rounds of folding, prefixes, edit distances and an alias list
 * narrowed that from 202 flagged rows to ~60, and the two biggest remaining classes
 * are ones no string comparison can reach: a venue crediting the film's OTHER
 * director (Ethan for a film TMDB credits to Joel Coen, Karl Freund on the 1931
 * "Dracula"), and a pseudonym sharing no letters with the person behind it (Loriot
 * / Vicco von Bülow). TMDB knows both — the first is in the film's crew, the second
 * is one person id — so this asks TMDB instead of guessing from the letters.
 *
 * Layered deliberately. The string check stays pure and cheap, so the corpus-scan
 * metric can use it unchanged; only a caller about to SPEND something — a forced
 * re-resolution — pays for the lookups. And spending is not free: re-resolving a
 * correct row can lose its resolution outright, because a director walk on a name
 * TMDB does not credit finds nothing at all.
 *
 * A RUNTIME contradiction is passed straight through. It compares numbers the
 * cinemas published against the film's own, needs no interpreting, and was never
 * the source of the false positives.
 */
class CrewConfirmation(credits: CrewConfirmation.Credits) extends Logging {

  /** True when the contradiction survives asking TMDB who worked on the film.
   *  False when there is no contradiction, when the venue names someone who did,
   *  or when TMDB cannot answer — an unanswered question is not evidence. */
  def confirmed(record: MovieRecord): Boolean =
    CinemaCorroboration.contradiction(record) match {
      case None                          => false
      case Some(Contradiction.Runtime)   => true
      case Some(Contradiction.Director)  => record.tmdbId.exists(directorIsAStranger(record, _))
    }

  private def directorIsAStranger(record: MovieRecord, tmdbId: Int): Boolean = {
    val crew = credits.crewIds(tmdbId)
    // No crew read, no answer. TMDB failing or having no credits says nothing about
    // whether this is the right film, and treating it as agreement would re-resolve
    // rows on the strength of a failed request.
    if (crew.isEmpty) return false
    val named = record.cinemaDirector.flatMap(personIds)
    // Likewise a name TMDB has never heard of: unknown is not absent.
    if (named.isEmpty) return false
    val stranger = !named.exists(crew.contains)
    if (!stranger)
      logger.debug(s"crew confirmation: the venue's director is on tmdbId=$tmdbId's crew — not a contradiction")
    stranger
  }

  /** The people a credited name can mean, retrying without the middle names when
   *  TMDB knows nobody by the whole thing.
   *
   *  A venue writes the name in full where TMDB holds the working form: German
   *  venues published "David Kerrick Hand" for Disney's "Snow White", and TMDB has
   *  him only as "David Hand" (5446, Directing). The search is exact enough that
   *  the fuller name returns NOTHING, so the confirmation abstained and a genuinely
   *  mis-resolved row — the 1937 Disney feature sitting on a 1939 German film of
   *  the same title — went untouched.
   *
   *  Only ever reached when the full name found nobody, so it cannot change an
   *  answer TMDB already gave; and a name it resolves still has to be ABSENT from
   *  the crew before anything acts. */
  private def personIds(name: String): Seq[Int] =
    credits.personIds(name) match {
      case Nil   => withoutMiddleNames(name).map(credits.personIds).getOrElse(Seq.empty)
      case found => found
    }

  /** "David Kerrick Hand" as "David Hand" — the first and last of three or more
   *  SUFFIX-FREE tokens. Fewer than three has no middle name to drop.
   *
   *  Never across a NOBILIARY PARTICLE. "Lars von Trier" shortens to "Lars Trier",
   *  which is a different person if TMDB has one at all — and a person TMDB does
   *  answer for is exactly what turns an abstention into a confirmed contradiction,
   *  so a wrong answer here force-re-resolves a row that was right. A middle NAME is
   *  droppable; a particle is part of the surname. */
  private def withoutMiddleNames(name: String): Option[String] = {
    // Generational suffixes are dropped BEFORE first-and-last is taken, not treated
    // as the surname: venues publish them ("David G. Derrick Jr."), and keeping the
    // suffix as the last word yields "David Jr." — which discards the one token that
    // identifies the person, and whoever TMDB returns for it is by construction off
    // the film's crew, so a correct row gets force-re-resolved.
    val words = name.split("\\s+").filter(_.nonEmpty)
    val core  = words.filterNot(isSuffix)
    // THREE suffix-free tokens, i.e. there is a middle name to drop. Two is not the
    // same shape and must not be shortened: "Robert Downey Jr." minus its suffix is
    // "Robert Downey", who is his FATHER — TMDB carries him (59874) and returns him
    // FIRST, because `findPersonCandidates` ranks Directing ahead of Acting. That
    // person is by construction off the film's crew, so the shortening would confirm
    // a contradiction and force-re-resolve a correct row. Same for Cuba Gooding and
    // Sammy Davis. Abstaining is the safe answer for a name with no middle to drop.
    // No `!= name` guard: with three or more suffix-free tokens the two-token result
    // can never equal the original, so it would be unreachable.
    Option.when(core.length >= 3)(s"${core.head} ${core.last}")
      .filterNot(_ => core.tail.dropRight(1).exists(isParticle))
  }

  /** Jr. / Sr. / II / III / IV — the same set `CinemaCorroboration` strips before
   *  comparing credits, for the same reason: they are not the surname. */
  private def isSuffix(word: String): Boolean =
    CinemaCorroboration.Suffixes.contains(word.toLowerCase.stripSuffix("."))

  /** The lowercase-by-convention words that bind a surname to its prefix. Compared
   *  case-insensitively because venues capitalise inconsistently ("Von Trier"). */
  private def isParticle(word: String): Boolean =
    CrewConfirmation.Particles.contains(word.toLowerCase.stripSuffix("."))
}

object CrewConfirmation {
  /** Nobiliary and patronymic particles: a middle word that belongs to the SURNAME
   *  rather than being a middle name, so shortening across it renames the person. */
  private val Particles: Set[String] = Set(
    "von", "van", "de", "del", "della", "der", "den", "di", "da", "dos", "das",
    "du", "la", "le", "el", "al", "bin", "ibn", "ben", "af", "av", "ter", "te", "zu")

  /** The two questions this asks TMDB, as a seam so a spec answers them directly
   *  and production wires them to the real client. */
  trait Credits {
    /** Person ids this credited name could refer to; empty when TMDB knows none. */
    def personIds(name: String): Seq[Int]
    /** Every person id credited anywhere on this film's crew; empty when unreadable. */
    def crewIds(tmdbId: Int): Set[Int]
  }
}
