package services.cinemas.pl

import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.LocalDateTime
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Sokół (Brzozowski Dom Kultury, Brzozów). Its repertoire at
 * `bdk.brzozow.pl/kino/` is a WordPress "Theatre" (WPT) plugin page — one
 * `div.wp_theatre_event` per screening, each self-contained:
 *   - `.wp_theatre_event_title a` → title (the poster `alt` is empty — use the
 *     title anchor)
 *   - `.wp_theatre_event_startdate` → full Polish date with year ("12 czerwca
 *     2026"), so no year inference
 *   - `.wp_theatre_event_starttime` → "16:45"
 *   - `.wp_theatre_event_venue` → format tag ("2D dubbing pl")
 *   - `a.wp_theatre_event_tickets_url` → a fixed ekobilet landing link
 *
 * Served over plain HTTP (the HTTPS cert altname is invalid). Previously
 * scraped from Filmweb, which had silently gone empty for the venue.
 */
class KinoSokolBrzozowClient(http: HttpFetch, override val cinema: Cinema = KinoSokolBrzozow)
    extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoSokolBrzozowClient.RepertoireUrl)
  override def sourceUrl: Option[String] = Some(KinoSokolBrzozowClient.BaseUrl)

  def fetch(): Seq[CinemaMovie] =
    KinoSokolBrzozowClient.parse(http.get(KinoSokolBrzozowClient.RepertoireUrl), cinema)
}

object KinoSokolBrzozowClient {

  val BaseUrl       = "http://bdk.brzozow.pl"
  val RepertoireUrl = s"$BaseUrl/kino/"

  // "12 czerwca 2026" → day, Polish genitive month, year.
  private val DatePat = """(\d{1,2})\s+(\p{L}+)\s+(\d{4})""".r

  private case class RawSlot(title: String, dateTime: LocalDateTime, booking: Option[String], filmUrl: Option[String])

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)

    val slots = document.select("div.wp_theatre_event").asScala.toSeq.flatMap(parseEvent)

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking)) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = group.flatMap(_.filmUrl).headOption,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }

  private def parseEvent(ev: Element): Option[RawSlot] =
    for {
      titleElement <- Option(ev.selectFirst(".wp_theatre_event_title a"))
      title    = titleElement.text.trim if title.nonEmpty
      dateText <- Option(ev.selectFirst(".wp_theatre_event_startdate")).map(_.text)
      d        <- DatePat.findFirstMatchIn(dateText)
      month    <- ScraperParse.PolishMonths.get(d.group(2).toLowerCase)
      time     <- Option(ev.selectFirst(".wp_theatre_event_starttime")).flatMap(t => ScraperParse.parseHHmm(t.text))
      dt       <- Try(LocalDateTime.of(d.group(3).toInt, month, d.group(1).toInt, time.getHour, time.getMinute)).toOption
    } yield RawSlot(
      title    = title,
      dateTime = dt,
      booking  = Option(ev.selectFirst("a.wp_theatre_event_tickets_url")).map(_.attr("abs:href")).filter(_.nonEmpty),
      filmUrl  = Option(titleElement.attr("abs:href")).filter(_.nonEmpty)
    )
}
