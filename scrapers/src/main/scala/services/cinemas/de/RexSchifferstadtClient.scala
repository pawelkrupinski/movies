package services.cinemas.de

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{CinemaScraper, ScrapeHorizon, ScraperParse, SlotsToMovies}
import tools.{HttpFetch, HttpRead}

import java.time.{LocalDate, LocalDateTime}
import scala.jdk.CollectionConverters._

/**
 * Rex Kino-Center Schifferstadt — a two-screen town cinema that takes bookings by
 * phone and sells through no ticketing vendor, so no aggregator lists it. Its
 * programme week (Thursday to Wednesday) is the home page of rex-schifferstadt.de,
 * a Joomla SP Page Builder page, one block per film:
 *
 *   <h2 class="sppb-image-layout-title">VATERLAND</h2>
 *   …
 *   <table><tr><td>DO 08.10.</td>…<td>MI 14.10.</td></tr>
 *          <tr><td>&nbsp;</td>…<td><strong>19:30</strong></td></tr></table>
 *
 * the header row's days, the rows under it each day's times by column — a cell may
 * hold more than one time, or none. The days carry no year: [[ScraperParse.upcomingDate]]
 * places them. Each table belongs to the film title before it.
 */
class RexSchifferstadtClient(
  http: HttpFetch,
  page: String,
  override val cinema: Cinema,
  /** The day the yearless dates are placed from, asked per scrape: today in Germany. */
  today: => LocalDate
) extends CinemaScraper {
  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(page)

  override def sourceUrl: Option[String] = Some(page)

  /** The week's showtimes from today on: the table keeps the days already past. */
  def fetch(): Seq[CinemaMovie] = {
    val from = today
    ScrapeHorizon.startingOn(from)(RexSchifferstadtClient.parse(HttpRead.page(http, page), cinema, from))
  }
}

object RexSchifferstadtClient {
  private val Clock = ScraperParse.ClockText.r
  private val Day   = """(\d{1,2})\.(\d{1,2})\.""".r

  private final case class Slot(title: String, dateTime: LocalDateTime)

  def parse(html: String, cinema: Cinema, today: LocalDate): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html)
    var title    = Option.empty[String]
    val slots = document.select("h2.sppb-image-layout-title, table").asScala.toSeq.flatMap { el =>
      if (el.tagName == "h2") { title = Some(el.text.trim).filter(_.nonEmpty); Nil }
      else { val slots = title.toSeq.flatMap(t => week(el, t, today)); title = None; slots }
    }
    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, None)) { (title, _, showtimes) =>
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

  /** One film's week table: the first row's days, every later row's times by column. */
  private def week(table: Element, title: String, today: LocalDate): Seq[Slot] =
    table.select("tr").asScala.toSeq match {
      case header +: rows =>
        val days = header.select("td, th").asScala.toSeq.map { cell =>
          Day.findFirstMatchIn(cell.text).flatMap(m => ScraperParse.monthDay(m.group(1).toInt, m.group(2).toInt))
            .flatMap(ScraperParse.upcomingDate(_, today))
        }
        for {
          row          <- rows
          (cell, day)  <- row.select("td").asScala.toSeq.zip(days)
          date         <- day.toSeq
          clock        <- Clock.findAllIn(cell.text).toSeq
          time         <- ScraperParse.parseHHmm(clock).toSeq
        } yield Slot(title, date.atTime(time))
      case _ => Nil
    }
}
