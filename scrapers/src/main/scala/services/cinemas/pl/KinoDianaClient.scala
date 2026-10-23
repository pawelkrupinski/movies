package services.cinemas.pl

import services.cinemas.common.ScraperParse
import services.movies.FormatTags
import tools.{HttpFetch, HttpRead}
import models._
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.LocalDateTime

/**
 * Kino Diana (Prudnik). Its ticketing site biletykinodiana.pl is a Ticket
 * Manager box office ([[TicketManagerFeed]]); its titles are ALL-CAPS with a
 * trailing format tag ("… - dubbing 2D"), cleaned by [[cleanTitle]] so the
 * variants of one film merge. Previously scraped from
 * Filmweb, which had silently gone empty for the venue (every poll returned
 * `[]` → zero showtimes on /uptime).
 */
class KinoDianaClient(http: HttpFetch, override val cinema: Cinema = KinoDiana)
    extends CinemaScraper {

  import KinoDianaClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  // The feed IS the listing: a failed fetch fails the scrape (red), not an empty one.
  def fetch(): Seq[CinemaMovie] =
    SlotsToMovies.fold(
      parseFeed(HttpRead.page(http, FeedUrl)),
      keyOf      = _._1,
      showtimeOf = { case (_, dt, booking) => Showtime(dt, booking) }
    ) { (title, _, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
}

object KinoDianaClient {

  val BaseUrl = "https://biletykinodiana.pl"
  val FeedUrl = TicketManagerFeed.feedUrl(s"$BaseUrl/")

  /** (cleaned title, screening datetime, booking URL) for each screening in the feed. */
  private[cinemas] def parseFeed(body: String): Seq[(String, LocalDateTime, Option[String])] =
    TicketManagerFeed.screenings(body, s"$BaseUrl/").map(s => (cleanTitle(s.billedTitle), s.dateTime, s.bookingUrl))

  /** Strip the trailing format tag and sentence-case the ALL-CAPS title so
   *  variants merge and the result reads like the other sources. */
  private[cinemas] def cleanTitle(raw: String): String =
    ScraperParse.sentenceCase(FormatTags.stripFormatTags(raw))
}
