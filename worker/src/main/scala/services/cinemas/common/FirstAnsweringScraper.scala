package services.cinemas.common

import models.{Cinema, CinemaMovie}

import scala.util.{Failure, Success, Try}

/**
 * A venue's ordered FALLBACK CHAIN as one scraper: each source in turn, serving the
 * first that lists a showtime. It is the `fallback` a [[SourceFallbackScraper]] hands
 * over to once the venue's primary keeps failing or comes back empty, so a German
 * venue whose primary is Cineamo falls to Filmstarts, then kinoheld, then
 * kinoprogramm.com — whichever of them still lists it — with the grace, the backoff
 * and the /uptime state that wrapper already keeps.
 *
 * A source that throws or lists nothing passes the venue on to the next. When none
 * lists a showtime, an empty answer from any of them is the chain's (the venue is
 * dark everywhere); only when EVERY source threw does the chain throw, with the first
 * source's error. The page failures of the source that served are the listing's
 * ([[ListingReads]]); a source that was passed over leaves none behind.
 */
class FirstAnsweringScraper(sources: Seq[CinemaScraper]) extends CinemaScraper {
  require(sources.nonEmpty, "a fallback chain needs a source")

  val cinema: Cinema           = sources.head.cinema
  def scrapeHosts: Set[String] = sources.flatMap(_.scrapeHosts).toSet
  override def sourceUrl: Option[String] = sources.head.sourceUrl
  override def sourceKey: Option[String] = sources.head.sourceKey

  def fetch(): Seq[CinemaMovie] = {
    val attempts = sources.iterator.map(source => ListingReads.during(Try(source.fetch())))
    var firstEmpty: Option[(Seq[CinemaMovie], ListingReads)] = None
    var firstError: Option[Throwable] = None
    while (attempts.hasNext) {
      attempts.next() match {
        case (Success(movies), reads) if movies.exists(_.showtimes.nonEmpty) =>
          reads.failed.foreach(ListingReads.pageFailed)
          return movies
        case (Success(movies), reads) => if (firstEmpty.isEmpty) firstEmpty = Some((movies, reads))
        case (Failure(t), _)          => if (firstError.isEmpty) firstError = Some(t)
      }
    }
    firstEmpty match {
      case Some((movies, reads)) => reads.failed.foreach(ListingReads.pageFailed); movies
      case None                  => throw firstError.get
    }
  }
}
