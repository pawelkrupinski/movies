package services.cinemas.de

import java.time.LocalDate

import scala.jdk.CollectionConverters.*

import models.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{ScrapeHorizon, ScrapedSlot, SingleSiteScraper, SlotsToMovies, ScrapeClock, ScrapeDates}
import tools.{HttpFetch, HttpRead}

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
 * hold more than one time, or none. The days carry no year: [[ScrapeDates.upcomingDate]]
 * places them. Each table belongs to the film title before it.
 */
class RexSchifferstadtClient(
  http: HttpFetch,
  page: String,
  override val cinema: Cinema,
  /** The day the yearless dates are placed from, asked per scrape: today in Germany. */
  today: => LocalDate
) extends SingleSiteScraper {
  protected def siteUrl: String = page

  /** The week's showtimes from today on: the table keeps the days already past. */
  def fetch(): Seq[CinemaMovie] = {
    val from = today
    ScrapeHorizon.startingOn(from)(RexSchifferstadtClient.parse(HttpRead.page(http, page), cinema, from))
  }
}

object RexSchifferstadtClient {
  private val Clock = ScrapeClock.ClockText.r
  private val Day   = """(\d{1,2})\.(\d{1,2})\.""".r

  def parse(html: String, cinema: Cinema, today: LocalDate): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html)
    var title    = Option.empty[String]
    val slots = document.select("h2.sppb-image-layout-title, table").asScala.toSeq.flatMap { el =>
      if (el.tagName == "h2") { title = Some(el.text.trim).filter(_.nonEmpty); Nil }
      else { val slots = title.toSeq.flatMap(t => week(el, t, today)); title = None; slots }
    }
    SlotsToMovies.basic(slots, cinema)
  }

  /** One film's week table: the first row's days, every later row's times by column. */
  private def week(table: Element, title: String, today: LocalDate): Seq[ScrapedSlot] =
    table.select("tr").asScala.toSeq match {
      case header +: rows =>
        val days = header.select("td, th").asScala.toSeq.map { cell =>
          Day.findFirstMatchIn(cell.text).flatMap(m => ScrapeDates.monthDay(m.group(1).toInt, m.group(2).toInt))
            .flatMap(ScrapeDates.upcomingDate(_, today))
        }
        for {
          row          <- rows
          (cell, day)  <- row.select("td").asScala.toSeq.zip(days)
          date         <- day.toSeq
          clock        <- Clock.findAllIn(cell.text).toSeq
          time         <- ScrapeClock.parseHHmm(clock).toSeq
        } yield ScrapedSlot(title, date.atTime(time))
      case _ => Nil
    }
}
