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
 * Jonatan Etzler, 100 minutes; Babylon's "Cicle Gaudí: LA FURIA": 107 minutes).
 *
 * A second-feed row lends its facts to a primary row only when it is the same screening of the same billing:
 *  - it shares a start time with the primary row — a fact of THIS listing, never borrowed across dates;
 *  - its title, or its original title, names exactly the words the primary bills ([[KinoheldClient.words]], a year in
 *    brackets aside): kinoheld links a venue's show to a catalogue entry by guess, and an entry naming more ("Lauras
 *    Stern" linked to "Lauras Stern und der geheimnisvolle Drache Nian", 2009, at Universum-City Karlsruhe, which billed
 *    the 2021 film) or other words ("Tour du cinéma - Cinéma! Berberian Sound Studio" linked to "Orelsan - Civilisation
 *    Tour au cinéma" at Thalia Dresden) is another film's facts;
 *  - it is the only second-feed film doing so.
 * Only a row that states no fact of its own and links nothing (no catalogue id, no page) takes them, and it takes the
 * second feed's page with them: the facts stay that feed's claim (`SourceReliability.factsOf` reads the page — kinoheld's
 * `/film/` pages are a feed's, trusted to support a take and never to decide one alone), exactly as they are on a venue
 * kinoheld serves as primary. Synopsis, cast and poster are not lent: they describe the entry, not the screening.
 *
 * The second feed is read only when the primary lists a bare row. It failing, or listing nothing matching, leaves the
 * primary's listing as it came — never failed, never incomplete: a page the second feed lost is not a page of the listing.
 * The facts are rebuilt from each scrape's own read; nothing is carried from an earlier one.
 */
class SecondFeedFacts(primary: CinemaScraper, second: CinemaScraper) extends DelegatingCinemaScraper(primary) with Logging {
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
        logger.info(s"${cinema.displayName}: the second feed ${second.getClass.getSimpleName} failed, its facts left out: ${t.getClass.getSimpleName}: ${t.getMessage}")
        listing
    }
  }
}

object SecondFeedFacts {

  /** Does the row state no fact and link nothing — the bare billing a second feed may speak for? An original title
   *  that only restates the title in other letters (Cineamo's record name "In the Mood for Love" beside Neues Arena's
   *  "IN THE MOOD FOR LOVE") is no fact. */
  def bare(row: CinemaMovie): Boolean =
    row.externalIds.isEmpty && row.filmUrl.isEmpty && row.director.forall(_.trim.isEmpty) && {
      val movie = row.movie
      movie.runtimeMinutes.isEmpty && movie.releaseYear.isEmpty && movie.countries.isEmpty &&
        movie.originalTitle.forall(original => words(original) == words(movie.title))
    }

  /** `row` with the facts of the one second-feed row screening it ([[sameScreening]]), when it is bare and that row
   *  states any; else `row` itself. */
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
      (secondRow.movie.title +: secondRow.movie.originalTitle.toSeq).exists(words(_) == billed)
  }

  /** Does the row state a fact the identity reads — and a page to carry them as its feed's claim? */
  private def states(row: CinemaMovie): Boolean =
    row.filmUrl.isDefined && (row.movie.runtimeMinutes.isDefined || row.movie.releaseYear.isDefined ||
      row.director.exists(_.trim.nonEmpty) || row.movie.countries.nonEmpty || row.movie.originalTitle.isDefined)

  private def startTimes(row: CinemaMovie): Set[LocalDateTime] = row.showtimes.map(_.dateTime).toSet

  /** A title's counted words ([[KinoheldClient.words]]), a year it states aside: kinoheld names a catalogue entry
   *  "In the Mood for Love (2000)" where the venue bills "IN THE MOOD FOR LOVE". */
  private def words(title: String): Set[String] = KinoheldClient.words(title).filterNot(Year.matches)
  private val Year = "(18|19|20)\\d\\d".r
}
