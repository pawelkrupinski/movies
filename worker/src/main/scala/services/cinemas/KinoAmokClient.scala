package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Amok (Gliwice) — studyjne cinema run by CSW Górnośląskie Centrum Kultury.
 * The repertoire is a server-rendered WordPress page at `/repertuar/`, listing
 * all days for the coming week.
 *
 * Layout:
 *  - Each day is introduced by `div.ribbon.rep-ribbon` containing an `<h3>`
 *    with the Polish date ("7 czerwca") and a weekday name.
 *  - Within that day's `div.post-list`, each screening is a `div.post` with:
 *      - `div.margin-b` → start time (`HH:MM`).
 *      - sibling `div` → room name ("Duża sala" / "Mała sala" / …).
 *      - `div.rep-title a strong` → film title.
 *      - `a.more-link[href=/repertuar/kup-bilet?id=N]` → booking URL.
 *        (Some screenings have no ticket button when sold out or not yet bookable.)
 *
 * Showtimes are scraped directly from the listing; no per-film detail fetch is
 * needed for our surface (title + time + room + booking URL).
 *
 * The year is inferred from `today`: if the "D miesiąca" date would be more
 * than 60 days in the past, it belongs to next year (handles the
 * December → January page-turn).
 */
class KinoAmokClient(
  http:             HttpFetch,
  override val cinema: Cinema,
  today:            LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import KinoAmokClient._

  def fetch(): Seq[CinemaMovie] = {
    val html = http.get(RepertoireUrl)
    val doc  = Jsoup.parse(html)
    val slots = parseDoc(doc, today)

    val byTitle = slots.groupBy(_.title)
    byTitle.toSeq.flatMap { case (title, group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, s.booking, s.room))
        .distinctBy(s => (s.dateTime, s.room))
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      ))
    }.sortBy(_.movie.title)
  }
}

object KinoAmokClient {

  val BaseUrl        = "https://amok.gliwice.pl"
  val RepertoireUrl  = s"$BaseUrl/repertuar/"

  // "7 czerwca" — day number + genitive month name.
  private val DayMonthPat = """(\d{1,2})\s+([\p{L}]+)""".r

  private[cinemas] case class RawSlot(
    title:    String,
    dateTime: LocalDateTime,
    room:     Option[String],
    booking:  Option[String]
  )

  private[cinemas] def parseDoc(doc: Document, today: LocalDate): Seq[RawSlot] = {
    // Split the page by ribbon divs (each day section).  We use Jsoup's
    // own select rather than splitting raw HTML.
    val container = doc.selectFirst("div.repertoire_list_f")
    if (container == null) return Seq.empty

    var currentDate: Option[LocalDate] = None
    val slots = scala.collection.mutable.Buffer.empty[RawSlot]

    container.children.asScala.foreach { child =>
      if (child.hasClass("ribbon") && child.hasClass("rep-ribbon")) {
        // Date header — extract "DD miesiąca"
        val h3 = Option(child.selectFirst("h3")).map(_.text.trim).getOrElse("")
        currentDate = parsePolishDate(h3, today)
      } else if (child.hasClass("post-list")) {
        // Film list for the current date
        currentDate.foreach { date =>
          child.select("div.post").asScala.foreach { post =>
            parsePost(post, date, BaseUrl).foreach(slots += _)
          }
        }
      }
    }
    slots.toSeq
  }

  /** Parse a single screening `div.post` into a `RawSlot`. */
  private def parsePost(post: org.jsoup.nodes.Element, date: LocalDate, baseUrl: String): Option[RawSlot] = {
    val timeText = Option(post.selectFirst("div.margin-b")).map(_.text.trim).getOrElse("")
    val time     = ScraperParse.parseHHmm(timeText)

    // The room sits in the sibling column to the time — the first bare `<div>`
    // after the time cell that has text.
    val room = post.select("div.col-sm-3 div.row div.col-xs-9").asScala.toSeq
      .flatMap(d => Option(d.selectFirst("div")).map(_.text.trim).filter(_.nonEmpty))
      .headOption

    val title = Option(post.selectFirst("div.rep-title a strong")).map(_.text.trim).filter(_.nonEmpty)

    val booking = Option(post.selectFirst("a.more-link[href]"))
      .map(_.attr("href")).filter(_.contains("kup-bilet"))
      .map(href => if (href.startsWith("http")) href else baseUrl + href)

    for {
      t    <- title
      lt   <- time
    } yield RawSlot(t, LocalDateTime.of(date, lt), room, booking)
  }

  /** "7 czerwca" / "13 stycznia" → LocalDate.
   *  If the resulting date would be more than 60 days in the past, assume next year. */
  private[cinemas] def parsePolishDate(text: String, today: LocalDate): Option[LocalDate] =
    DayMonthPat.findFirstMatchIn(text).flatMap { m =>
      ScraperParse.PolishMonths.get(m.group(2).toLowerCase).flatMap { month =>
        Try {
          val day  = m.group(1).toInt
          val year = today.getYear
          val candidate = LocalDate.of(year, month, day)
          if (candidate.isBefore(today.minusDays(60))) candidate.plusYears(1) else candidate
        }.toOption
      }
    }
}
