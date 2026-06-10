package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.HttpFetch

import java.time.LocalDateTime
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Generic client for cinemas ticketed through the systembiletowy.pl platform
 * (each venue gets its own subdomain, e.g. `shd.systembiletowy.pl` for the
 * Suchedniów cultural centre's Kino Kuźnica). The instance homepage
 * (`<base>/index.php`) is server-rendered: one `table.tbl_repertoire` whose
 * rows each carry a single screening —
 *   - `td.title a`      → film title (a trailing `dubbing`/`napisy` version tag)
 *   - `td.date span.day`  → Polish-text date ("12 czerwca 2026")
 *   - `td.date span.hour` → time ("16:00")
 *   - `td.link a`       → the per-screening booking link (`repertoire.html?id=N`)
 *
 * One instance per venue, captured by its `baseUrl` + `cinema`, so adding a
 * systembiletowy-hosted cinema is a catalog line, not a new client (OCP).
 *
 * Previously scraped from Filmweb, which had silently gone empty for the venue
 * (every poll returned `[]`) though the cinema is open and screening.
 */
class SystemBiletowyClient(http: HttpFetch, baseUrl: String, override val cinema: Cinema)
    extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(baseUrl)

  def fetch(): Seq[CinemaMovie] =
    SystemBiletowyClient.parse(http.get(s"$baseUrl/index.php"), cinema, baseUrl)
}

object SystemBiletowyClient {

  // "12 czerwca 2026" — day, Polish genitive month, year (all present).
  private val DatePat = """(\d{1,2})\s+(\p{L}+)\s+(\d{4})""".r

  private case class RawSlot(title: String, dateTime: LocalDateTime, booking: Option[String])

  def parse(html: String, cinema: Cinema, baseUrl: String): Seq[CinemaMovie] = {
    val doc = Jsoup.parse(html, baseUrl)

    val slots = doc.select("table.tbl_repertoire tr").asScala.toSeq.flatMap { tr =>
      // Only rows that are a real screening carry a repertoire/booking link.
      if (tr.selectFirst("a[href*=repertoire.html]") == null) None
      else for {
        titleEl <- Option(tr.selectFirst("td.title a"))
        title    = cleanTitle(titleEl.text) if title.nonEmpty
        dayText <- Option(tr.selectFirst("td.date span.day")).map(_.text)
        d       <- DatePat.findFirstMatchIn(dayText)
        month   <- ScraperParse.PolishMonths.get(d.group(2).toLowerCase)
        time    <- Option(tr.selectFirst("td.date span.hour")).flatMap(h => ScraperParse.parseHHmm(h.text))
        dt      <- Try(LocalDateTime.of(d.group(3).toInt, month, d.group(1).toInt, time.getHour, time.getMinute)).toOption
      } yield RawSlot(
        title    = title,
        dateTime = dt,
        booking  = Option(tr.selectFirst("td.link a[href]")).map(_.attr("abs:href"))
                     .filter(_.nonEmpty).orElse(Option(titleEl.attr("abs:href")).filter(_.nonEmpty))
      )
    }

    slots.groupBy(_.title).toSeq.flatMap { case (title, group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, s.booking))
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

  /** Drop the trailing `dubbing`/`napisy`/… version tag (so the same film's
   *  dubbed and subtitled screenings merge into one row) and sentence-case the
   *  result. Tag stripping is shared with the other portal clients. */
  private[cinemas] def cleanTitle(raw: String): String =
    ScraperParse.sentenceCase(ScraperParse.stripFormatTags(raw))
}
