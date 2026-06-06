package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Zorza (Rzeszów) — run by Regionalna Fundacja Filmowa. The cinema
 * publishes a server-rendered weekly repertoire at `/repertuar`, listing ALL
 * days on a single page (no per-day pagination).
 *
 * Each date block is a pair of sibling elements:
 *   - `div.fullrepertoire-date > h3` — date in `DD.MM` format (no year).
 *   - `div.fullrepertoire-content` — the films for that day.
 *
 * Within a content block each film occupies a wrapper `<div>` containing:
 *   - `div.repertoire-title a.on-repertoire[title]` — the film title and
 *     the cinema-relative film URL (`/film/<slug>`).
 *   - `div.repertoire-hall:not(.repertoire-mobile)` — a three-column hall
 *     grid (Sala widowiskowa / Sala czerwona / Sala niebieska). Each column
 *     child `<div>` holds zero or more `span.timeItem` elements (`HH:MM`).
 *
 * Booking is via the cinema's generic MSI portal (`https://bilety.kinozorza.pl/MSI`);
 * there are no per-screening URLs on the repertoire page, so `bookingUrl` is
 * always `None`.
 *
 * The year is inferred: if the page's `DD.MM` date falls before `today`, the
 * date belongs to next year — this handles the January page-turn correctly.
 */
class KinoZorzaClient(
  http:             HttpFetch,
  override val cinema: Cinema = KinoZorza,
  today:            LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import KinoZorzaClient._

  def fetch(): Seq[CinemaMovie] = {
    val html = http.get(RepertoireUrl)
    val doc  = Jsoup.parse(html)
    val slots = parseDoc(doc, today)

    val byFilmUrl = slots.groupBy(_.filmUrl)
    byFilmUrl.toSeq.flatMap { case (_, group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, bookingUrl = None, room = Some(s.hall)))
        .distinctBy(s => (s.dateTime, s.room))
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else {
        val head = group.head
        Some(CinemaMovie(
          movie     = Movie(head.title),
          cinema    = cinema,
          posterUrl = None,
          filmUrl   = Some(BaseUrl + head.filmUrl),
          synopsis  = None,
          cast      = Seq.empty,
          director  = Seq.empty,
          showtimes = showtimes
        ))
      }
    }.sortBy(_.movie.title)
  }
}

object KinoZorzaClient {

  val BaseUrl        = "https://www.kinozorza.pl"
  val RepertoireUrl  = s"$BaseUrl/repertuar"

  /** The three hall columns, in document order. */
  private val Halls = IndexedSeq("Sala widowiskowa", "Sala czerwona", "Sala niebieska")

  private val DayMonthPat = """(\d{1,2})\.(\d{1,2})""".r

  private[cinemas] case class RawSlot(title: String, filmUrl: String, dateTime: LocalDateTime, hall: String)

  private[cinemas] def parseDoc(doc: Document, today: LocalDate): Seq[RawSlot] = {
    // The date blocks and content blocks are adjacent siblings inside the same
    // container. We walk them in pairs: fullrepertoire-date → fullrepertoire-content.
    val dateDivs    = doc.select("div.fullrepertoire-date").asScala.toSeq
    val contentDivs = doc.select("div.fullrepertoire-content").asScala.toSeq

    dateDivs.zip(contentDivs).flatMap { case (dateDiv, contentDiv) =>
      parseDate(dateDiv, today) match {
        case None       => Seq.empty
        case Some(date) => parseContent(contentDiv, date)
      }
    }
  }

  /** Extract a `LocalDate` from `<h3>DD.MM …</h3>`, guessing the year from
    * `today`. A date that would be more than 60 days in the past relative to
    * today is assumed to belong to next year (handles the December→January turn). */
  private def parseDate(dateDiv: Element, today: LocalDate): Option[LocalDate] = {
    val h3 = Option(dateDiv.selectFirst("h3")).map(_.text.trim).getOrElse("")
    DayMonthPat.findFirstMatchIn(h3).flatMap { m =>
      Try {
        val day   = m.group(1).toInt
        val month = m.group(2).toInt
        val candidate = LocalDate.of(today.getYear, month, day)
        if (candidate.isBefore(today.minusDays(60)))
          candidate.plusYears(1)
        else
          candidate
      }.toOption
    }
  }

  /** Parse all film rows in one day's `div.fullrepertoire-content`. */
  private def parseContent(contentDiv: Element, date: LocalDate): Seq[RawSlot] = {
    // Each direct child <div> that contains a repertoire-title is a film row.
    // The first child is fullrepertoire-desktop (the header); skip divs without
    // a repertoire-title inside.
    contentDiv.children.asScala.toSeq.flatMap { row =>
      val titleEl = Option(row.selectFirst("div.repertoire-title a.on-repertoire"))
      titleEl match {
        case None => Seq.empty
        case Some(anchor) =>
          val title   = anchor.attr("title").trim
          val filmUrl = anchor.attr("href").trim
          if (title.isEmpty || filmUrl.isEmpty) Seq.empty
          else {
            // The non-mobile hall grid: the last div.repertoire-hall child.
            // (The mobile version has class "repertoire-hall repertoire-mobile" and
            // lists only room names, not times; we want the plain one.)
            val hallDivs = row.select("div.repertoire-hall:not(.repertoire-mobile)").asScala.toSeq
            hallDivs.flatMap { hallDiv =>
              val columns = hallDiv.children.asScala.toSeq
              columns.zipWithIndex.flatMap { case (col, idx) =>
                val hallName = if (idx < Halls.length) Halls(idx) else s"Sala ${idx + 1}"
                col.select("span.timeItem").asScala.toSeq.flatMap { span =>
                  ScraperParse.parseHHmm(span.text.trim).map { time =>
                    RawSlot(title, filmUrl, LocalDateTime.of(date, time), hallName)
                  }
                }
              }
            }
          }
      }
    }
  }
}
