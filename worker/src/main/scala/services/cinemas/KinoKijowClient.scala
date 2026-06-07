package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.{LocalDate, LocalDateTime, YearMonth, ZoneId}
import java.time.format.DateTimeFormatter
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Kijów (Kraków) — independent cinema at al. Krasińskiego 34. The venue
 * uses the Apollo Film MSI ticketing portal at `kupbilet.kijow.pl`. The portal
 * renders a server-side HTML schedule at `/MSI/mvc/pl?sort=Date&date=YYYY-MM`,
 * one page per calendar month.
 *
 * Each screening on the page appears as a `div.cd-timeline-block` containing:
 *   - `div.cd-timeline-content.eventlist`:
 *       - `span.cd-date` — date and time in the format "DD mmm HH:MM" (Polish
 *         3-letter month abbreviation: sty, lut, mar, kwi, maj, cze, lip, sie,
 *         wrz, paź, lis, gru), e.g. "07 cze 10:30".
 *       - `h2` — title text preceded by a hidden inline date repeat; the title
 *         is the text after the ` - ` separator, e.g.
 *         "07 cze 10:30 - Film Title".
 *       - `a.btn-badge2[href^="/MSI/Default.aspx?event_id="]` — the booking
 *         link.
 *
 * The scraper fetches the current month and, when today is in the last two
 * weeks of the month, also the following month — so the fixture for a mid-month
 * capture only needs the one month page. The year is read from the `date=`
 * query parameter rather than inferred from `today`, so month-turns are
 * handled without guesswork.
 */
class KinoKijowClient(
  http:             HttpFetch,
  override val cinema: Cinema,
  today:            LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import KinoKijowClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val months = monthsToFetch(today)
    val pages  = ParallelDetailFetch.keyed("kino-kijow-months", months, 1.minute)(m => monthUrl(m)) { url =>
      Try(http.get(url)).toOption
    }
    val slots = months.flatMap(m => pages.getOrElse(m, None).toSeq.flatMap(html => parseDoc(html, m)))

    val byTitle = slots.groupBy(_.title)
    byTitle.toSeq.flatMap { case (title, group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, Some(s.bookingUrl)))
        .distinctBy(s => (s.dateTime, s.bookingUrl))
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

  /** Current month, plus next month when today is in the final 14 days so
    * near-boundary screenings aren't missed. */
  private def monthsToFetch(today: LocalDate): Seq[YearMonth] = {
    val cur  = YearMonth.from(today)
    val daysLeft = today.lengthOfMonth - today.getDayOfMonth
    if (daysLeft <= 14) Seq(cur, cur.plusMonths(1)) else Seq(cur)
  }

  private def monthUrl(ym: YearMonth): String =
    s"$BaseUrl/MSI/mvc/pl?sort=Date&date=${ym.format(MonthFmt)}&datestart=0"
}

object KinoKijowClient {

  val BaseUrl = "https://kupbilet.kijow.pl"

  private val MonthFmt = DateTimeFormatter.ofPattern("yyyy-MM")

  // Polish 3-letter month abbreviations as they appear in the MSI portal
  private val PolishMonthAbbr: Map[String, Int] = Map(
    "sty" -> 1, "lut" -> 2, "mar" -> 3, "kwi" -> 4, "maj" -> 5, "cze" -> 6,
    "lip" -> 7, "sie" -> 8, "wrz" -> 9, "paź" -> 10, "lis" -> 11, "gru" -> 12
  )

  // "07 cze 10:30" — day, Polish month abbr, time
  private val DateTimePat = """(\d{1,2})\s+(\w+)\s+(\d{2}:\d{2})""".r

  private[cinemas] case class RawSlot(title: String, dateTime: LocalDateTime, bookingUrl: String)

  private[cinemas] def parseDoc(html: String, month: YearMonth): Seq[RawSlot] = {
    val doc = Jsoup.parse(html)
    doc.select("div.cd-timeline-content.eventlist").asScala.toSeq.flatMap { block =>
      // Prefer the desktop hidden span; fallback to h2 text which repeats the date
      val cdDate  = Option(block.selectFirst("span.cd-date")).map(_.text.trim).getOrElse("")
      val h2Text  = Option(block.selectFirst("h2")).map(_.text.trim).getOrElse("")

      // Parse date/time from the span; title from h2 after the " - " separator
      val dateTime = parseDateTimePat(cdDate, month)
      val title    = h2Title(h2Text).filter(_.nonEmpty)

      val bookingUrl = Option(block.selectFirst("a.btn-badge2[href^=\"/MSI/Default.aspx\"]"))
        .map(_.attr("href").trim)
        .filter(_.nonEmpty)
        .map(h => if (h.startsWith("http")) h else s"$BaseUrl$h")

      for {
        dt <- dateTime
        t  <- title
        bu <- bookingUrl
      } yield RawSlot(t, dt, bu)
    }
  }

  /** Extract film title from h2 text like "07 cze 10:30 - Film Title".
    * Returns text after the first " - " separator, or the whole string if
    * no separator is found. */
  private def h2Title(h2: String): Option[String] = {
    val idx = h2.indexOf(" - ")
    if (idx >= 0) Some(h2.substring(idx + 3).trim)
    else if (h2.nonEmpty) Some(h2.trim)
    else None
  }

  /** Parse "DD mmm HH:MM" against the given `month` for the year, returning
    * `None` on any parse failure. */
  private def parseDateTimePat(s: String, month: YearMonth): Option[LocalDateTime] =
    DateTimePat.findFirstMatchIn(s).flatMap { m =>
      val day   = m.group(1).toInt
      val abbr  = m.group(2).toLowerCase
      val hhmm  = m.group(3)
      for {
        mo <- PolishMonthAbbr.get(abbr)
        t  <- ScraperParse.parseHHmm(hhmm)
        // Use the month's year; if the day's month doesn't match the page's month
        // (e.g. a "30 lip" date on a "2026-06" page), still accept it — the MSI
        // occasionally bleeds a few cross-month screenings onto the neighbouring page.
        yr = month.getYear
        d  <- Try(LocalDate.of(yr, mo, day)).toOption
      } yield LocalDateTime.of(d, t)
    }
}
