package services.cinemas.pl

import scala.jdk.CollectionConverters.*

import models.{Cinema, CinemaMovie}
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{ScrapedSlot, SlotsToMovies, ScrapeClock, ScrapeDates}

/** A WordPress "Theater for WP" (`wp_theatre`) programme page: one self-contained `div.wp_theatre_event` per
 *  screening -- `.wp_theatre_event_title a` (title and the venue's own film page), `.wp_theatre_event_startdate` (a
 *  full Polish date with year, "12 czerwca 2026"), `.wp_theatre_event_starttime` ("16:45") and
 *  `a.wp_theatre_event_tickets_url` (booking). What a venue adds on its own card (a poster, a format line)
 *  `decorate` reads. */
object WpTheatreEvents {

  def parse(html: String, baseUrl: String, cinema: Cinema)(decorate: (Element, ScrapedSlot) => ScrapedSlot): Seq[CinemaMovie] =
    SlotsToMovies.basic(
      Jsoup.parse(html, baseUrl).select("div.wp_theatre_event").asScala.toSeq.flatMap(card => slotOf(card).map(decorate(card, _))),
      cinema)

  private def slotOf(card: Element): Option[ScrapedSlot] =
    for {
      titleElement <- Option(card.selectFirst(".wp_theatre_event_title a"))
      title    = titleElement.text.trim if title.nonEmpty
      date     <- Option(card.selectFirst(".wp_theatre_event_startdate")).flatMap(d => ScrapeDates.parseDayMonthYear(d.text))
      time     <- Option(card.selectFirst(".wp_theatre_event_starttime")).flatMap(t => ScrapeClock.parseHHmm(t.text))
    } yield ScrapedSlot(
      title      = title,
      dateTime   = date.atTime(time),
      bookingUrl = Option(card.selectFirst("a.wp_theatre_event_tickets_url")).map(_.attr("abs:href")).filter(_.nonEmpty),
      filmUrl    = Option(titleElement.attr("abs:href")).filter(_.nonEmpty)
    )
}
