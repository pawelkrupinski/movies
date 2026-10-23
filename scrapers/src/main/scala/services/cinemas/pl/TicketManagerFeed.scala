package services.cinemas.pl

import services.cinemas.common.ScraperParse
import org.jsoup.Jsoup
import play.api.libs.json.{JsArray, Json}

import java.time.{LocalDate, LocalDateTime}
import scala.util.Try

/**
 * The "Ticket Manager" box office (ASP.NET MVC) that small Polish culture houses
 * run their ticketing on: Kino Remus, Kino Diana, MOKSiR Chojnów. Its calendar
 * renders in the browser, but the jQuery `eventCalendar` widget feeds from
 * `/Repertuar/Kalendarz1JsonDane?filter=<site root>`: a JSON array with one
 * object per screening, weeks ahead. Each object's `date` is the day stamped
 * `23:59:00`; the real time, the title, the poster and the booking link sit in
 * the object's `title`, an HTML fragment: `p.event-title`, `p.event-hour`
 * (`czwartek, godz.17:00`), `.poster img` and `a.bt` (`/Bilety/Sala/<id>`; absent
 * for a screening sold only at the box office).
 */
object TicketManagerFeed {

  /** One screening, its title as billed (quotes and format tags are each
   *  venue's own to strip). */
  final case class Screening(billedTitle: String, dateTime: LocalDateTime, bookingUrl: Option[String], posterUrl: Option[String])

  /** The feed of the box office at `siteRoot` (with its trailing slash). */
  def feedUrl(siteRoot: String): String = s"${siteRoot}Repertuar/Kalendarz1JsonDane?filter=$siteRoot"

  private val ScreeningHour = raw"""godz\.\s*(${ScraperParse.ClockTextDotted})""".r

  /** Every screening in the feed; an event lacking a title or a `godz.` time is
   *  skipped. Anything but an array — an error object, an HTML page — is no
   *  programme, and throws rather than landing as an empty one. */
  def screenings(body: String, siteRoot: String): Seq[Screening] = {
    val records = Json.parse(body) match {
      case array: JsArray => array.value.toSeq
      case other          => throw new IllegalStateException(s"$siteRoot feed is not an array: ${other.toString.take(120)}")
    }
    records.flatMap { record =>
      val fragment = Jsoup.parseBodyFragment((record \ "title").asOpt[String].getOrElse(""), siteRoot)
      for {
        day   <- (record \ "date").asOpt[String].flatMap(stamp => Try(LocalDate.parse(stamp.take(10))).toOption)
        title <- Option(fragment.selectFirst("p.event-title")).map(_.text.trim).filter(_.nonEmpty)
        hour  <- Option(fragment.selectFirst("p.event-hour")).flatMap(p => ScreeningHour.findFirstMatchIn(p.text))
        time  <- ScraperParse.parseHHmm(hour.group(1).replace('.', ':'))
      } yield Screening(
        billedTitle = title,
        dateTime    = LocalDateTime.of(day, time),
        bookingUrl  = Option(fragment.selectFirst("a.bt[href]")).map(_.attr("abs:href")).filter(_.nonEmpty),
        posterUrl   = Option(fragment.selectFirst(".poster img[src]")).map(_.attr("abs:src")).filter(_.nonEmpty)
      )
    }
  }
}
