package services.cinemas.common

import models.CinemaMovie
import play.api.Logging

import java.time.LocalDateTime
import scala.util.{Failure, Success, Try}

/**
 * A venue's listing from its primary feed, with the facts its SECOND feed states about the very same screenings put on
 * the rows the primary bills bare.
 *
 * Cineamo serves ~1,300 German venues, and for a film its operator never linked to a record (`cineamoMovieId` null) a
 * showing is a bare title: no year, running time, director or original title. A bare title cannot be told from its
 * namesakes by anything of its own, so it stays unmatched or takes the wrong one. Most of those venues sell through
 * kinoheld.de too, whose show for the same screening often states the facts (Cinema Borken's "Bad Apples FK": 2026,
 * Jonatan Etzler, 100 minutes; Capitol Mainz's "THEMENGESPRÄCH. The Broken Circle": 2013, Felix van Groeningen).
 *
 * A second-feed row lends its facts to a primary row only when it is the same screening of the same billing:
 *  - it shares a start time with the primary row — a fact of THIS listing, never borrowed across dates;
 *  - its title, or its original title, names exactly the words the primary bills ([[KinoheldClient.words]], a year in
 *    brackets aside): kinoheld links a venue's show to a catalogue entry by guess, and an entry naming more ("Lauras
 *    Stern" linked to "Lauras Stern und der geheimnisvolle Drache Nian", 2009, at Universum-City Karlsruhe, which billed
 *    the 2021 film) or other words ("Tour du cinéma - Cinéma! Berberian Sound Studio" linked to "Orelsan - Civilisation
 *    Tour au cinéma" at Thalia Dresden) is another film's facts;
 *  - a year the primary bills is the entry's too;
 *  - it is the only second-feed film doing so, and a dated one ([[states]]).
 * Only a row that states no fact of its own and links nothing (no catalogue id, no page) takes them, and it takes the
 * second feed's page with them: the facts stay that feed's claim (`SourceReliability.factsOf` reads the page — kinoheld's
 * `/film/` pages are a feed's, trusted to support a take and never to decide one alone), exactly as they are on a venue
 * kinoheld serves as primary. Synopsis, cast and poster are not lent: they describe the entry, not the screening.
 *
 * The second feed is read only when the primary lists a bare row. Listing nothing matching leaves the primary's listing
 * as it came. A read of it that FAILS lends instead the facts it lent last time (`lastListing`: the venue's listing as
 * the scrape archive last stored it, durable, so a restart's boot burst keeps them too): the rows of it carrying the
 * second feed's page, each lent to the bare row of the very same screening by the same rule ([[lend]]). A failed read
 * used to drop them, and a bare row's take could flip from its film to none and back with kinoheld's 429s. The facts are
 * the source's input kept, not a take kept: lent with the listing, the whole resolve reads exactly what the incremental
 * one did. They expire with the showing (a start time in common with the row as listed now, never another date's), and
 * a read that succeeds replaces them all. Either way the listing is never failed, never incomplete: a page the second
 * feed lost is not a page of the listing.
 */
class SecondFeedFacts(primary: CinemaScraper, second: CinemaScraper, lastListing: models.Cinema => Try[Seq[CinemaMovie]])
    extends DelegatingCinemaScraper(primary) with Logging {
  import SecondFeedFacts._

  override def scrapeHosts: Set[String] = primary.scrapeHosts ++ second.scrapeHosts

  def fetch(): Seq[CinemaMovie] = {
    val listing = primary.fetch()
    if (!listing.exists(bare)) listing
    else ListingReads.during(Try(second.fetch()))._1 match {
      case Success(rows) =>
        val lent = listing.map(lend(_, rows))
        val filled = lent.zip(listing).count((after, before) => after ne before)
        if (filled > 0) logger.info(s"${cinema.displayName}: ${second.getClass.getSimpleName} lent its facts to $filled bare row(s)")
        lent
      case Failure(t) =>
        val kept   = lastListing(cinema) match {
          case Success(stored) => stored.filter(lentBy(second))
          // the archive unread: no facts known to keep, the listing as it came -- what a failed read left before
          case Failure(e) => logger.warn(s"${cinema.displayName}: its last stored listing not read, no second-feed facts kept: $e"); Nil
        }
        val lent   = listing.map(lend(_, kept))
        val filled = lent.zip(listing).count((after, before) => after ne before)
        logger.info(s"${cinema.displayName}: the second feed ${second.getClass.getSimpleName} failed, the facts it lent last time kept on " +
          s"$filled bare row(s): ${t.getClass.getSimpleName}: ${t.getMessage}")
        lent
    }
  }
}

object SecondFeedFacts {

  /** Does `row` carry `second`'s page: facts that feed lent it ([[lend]] lends its page with them)? */
  def lentBy(second: CinemaScraper)(row: CinemaMovie): Boolean =
    row.filmUrl.flatMap(tools.UrlHost.of).exists(second.scrapeHosts)

  /** Does the row state no fact and link nothing — the bare billing a second feed may speak for? An original title
   *  that only restates the title in other letters (Cineamo's record name "In the Mood for Love" beside Neues Arena's
   *  "IN THE MOOD FOR LOVE") is no fact. */
  def bare(row: CinemaMovie): Boolean =
    row.externalIds.isEmpty && row.filmUrl.isEmpty && row.director.forall(_.trim.isEmpty) && {
      val movie = row.movie
      movie.runtimeMinutes.isEmpty && movie.releaseYear.isEmpty && movie.countries.isEmpty &&
        movie.originalTitle.forall(original => words(original) == words(movie.title))
    }

  /** `row` with the facts of the one second-feed row screening it ([[sameScreening]]), when it is bare and that row is a
   *  dated film's ([[states]]); else `row` itself. */
  def lend(row: CinemaMovie, secondRows: Seq[CinemaMovie]): CinemaMovie =
    if (!bare(row)) row
    else secondRows.filter(sameScreening(row, _)) match {
      case Seq(only) if states(only) =>
        row.copy(
          movie    = row.movie.copy(runtimeMinutes = only.movie.runtimeMinutes, releaseYear = only.movie.releaseYear,
                                    countries = only.movie.countries,
                                    originalTitle = only.movie.originalTitle.filter(original => words(original) != words(row.movie.title))
                                      .orElse(row.movie.originalTitle)),
          director = only.director,
          filmUrl  = only.filmUrl)
      case _ => row
    }

  /** Is `secondRow` a screening of `row`: a start time in common, and its title (or original title) billing the very
   *  words `row` bills? */
  def sameScreening(row: CinemaMovie, secondRow: CinemaMovie): Boolean = {
    val billed = words(row.movie.title)
    billed.nonEmpty && startTimes(row).exists(startTimes(secondRow)) &&
      (secondRow.movie.title +: secondRow.movie.originalTitle.toSeq).exists(words(_) == billed) && datesAlike(row, secondRow)
  }

  /** A year the venue bills ("Der Grinch (2000)", "THE KILLER 1989") is the venue's own word on which film it screens: an
   *  entry not stating that year — in its year or its title — is another film's (kinoheld's "Der Grinch" is the 2018 one). */
  private def datesAlike(row: CinemaMovie, secondRow: CinemaMovie): Boolean = {
    val billed = years(row.movie.title)
    billed.isEmpty || (secondRow.movie.releaseYear.toSet ++ years(secondRow.movie.title)).exists(billed)
  }
  private def years(title: String): Set[Int] = KinoheldClient.words(title).filter(Year.matches).map(_.toInt)

  /** Is the row a dated film's entry, with a page to carry its facts as its feed's claim? An entry kinoheld dates no
   *  production year is a venue's own, made for its event and filled by kinoheld from a film database by name (as
   *  [[KinoheldClient.entryGuessed]]'s): Kino Aurich's "Der Herr der Ringe - Die Rückkehr des Königs (Extended Version)"
   *  credits James Cameron, its "Dune" Villeneuve's "Dune Double Feature", Thalia Dresden's "Matinee" McTiernan's "Last
   *  Action Hero"; even its running time, the venue's own, is an event's (Zeughauskino's "SHOAH (TEIL I)": 292 minutes).
   *  Lent to the recorded DE corpus of 2026-11-04, undated entries' running times alone moved 30 takes to none and 30 more
   *  to a film; dated entries' facts lost none. */
  private def states(row: CinemaMovie): Boolean = row.filmUrl.isDefined && row.movie.releaseYear.isDefined

  private def startTimes(row: CinemaMovie): Set[LocalDateTime] = row.showtimes.map(_.dateTime).toSet

  /** A title's counted words ([[KinoheldClient.words]]), a year it states aside: kinoheld names a catalogue entry
   *  "In the Mood for Love (2000)" where the venue bills "IN THE MOOD FOR LOVE". */
  private def words(title: String): Set[String] = KinoheldClient.words(title).filterNot(Year.matches)
  private val Year = "(18|19|20)\\d\\d".r
}
