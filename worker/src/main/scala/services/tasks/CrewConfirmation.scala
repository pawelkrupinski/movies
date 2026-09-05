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
   *  words. Fewer than three has no middle to drop, and the result would just be
   *  the query that already failed. */
  private def withoutMiddleNames(name: String): Option[String] = {
    val words = name.split("\\s+").filter(_.nonEmpty)
    Option.when(words.length >= 3)(s"${words.head} ${words.last}")
  }
}

object CrewConfirmation {
  /** The two questions this asks TMDB, as a seam so a spec answers them directly
   *  and production wires them to the real client. */
  trait Credits {
    /** Person ids this credited name could refer to; empty when TMDB knows none. */
    def personIds(name: String): Seq[Int]
    /** Every person id credited anywhere on this film's crew; empty when unreadable. */
    def crewIds(tmdbId: Int): Set[Int]
  }
}
