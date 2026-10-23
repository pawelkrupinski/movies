package services.cinemas.pl

import services.cinemas.common.{CinemaScraper, SlotsToMovies}
import services.movies.FormatTags
import models._
import tools.{HttpFetch, HttpRead}

/**
 * Kino Kultura (Chojnów, run by MOKSiR Chojnów). Its programme is read from the
 * town's Ticket Manager box office (`bilety.chojnow.eu`, [[TicketManagerFeed]]),
 * which carries every screening with its time and seat-map link.
 *
 * Not from the cinema's own "Kursorek" CMS page (`kino.chojnow.eu/repertuar,m15,s1500.html`):
 * its cards give a film's run as a bare-date `Rozpoczęcie:`/`Zakończenie:` pair
 * and leave the times to prose ("sobota i niedziela, 24 i 25 października,
 * godz. 18:00"), so on 2026-10-23 a weekend of three films parsed to nothing.
 *
 * The box office also sells the culture house's stage events, so
 * [[OnlyMovieEventsFilter]] drops those.
 */
class KinoKulturaChojnowClient(http: HttpFetch, override val cinema: Cinema = KinoKulturaChojnow)
    extends CinemaScraper with OnlyMovieEventsFilter {

  import KinoKulturaChojnowClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BoxOfficeUrl)
  override def sourceUrl: Option[String] = Some(BoxOfficeUrl)

  // The feed IS the listing: a failed fetch fails the scrape (red), not an empty one.
  protected def fetchUnfiltered(): Seq[CinemaMovie] = {
    val slots = TicketManagerFeed.screenings(HttpRead.page(http, TicketManagerFeed.feedUrl(BoxOfficeUrl)), BoxOfficeUrl).map { screening =>
      val (title, format) = FormatTags.extractFormatTags(screening.billedTitle)
      Slot(title, format, screening)
    }
    SlotsToMovies.fold(slots, _.title, s => Showtime(s.screening.dateTime, s.screening.bookingUrl, None, s.format)) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = group.flatMap(_.screening.posterUrl).headOption,
        filmUrl   = None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }
}

object KinoKulturaChojnowClient {
  val BoxOfficeUrl = "https://bilety.chojnow.eu/"

  private case class Slot(title: String, format: List[String], screening: TicketManagerFeed.Screening)
}
