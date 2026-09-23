package services.cinemas.pl

import services.cinemas.common.ScraperParse
import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneId, ZoneOffset}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Bielski Dom Kultury's cinema page ("Kino Bielsk", `bdkbielsk.pl/kino/`) is
 * a bespoke static (w3.css) page listing one `div.w3-large` per day
 * ("Poniedziałek 28 wrzesień", no year) followed by one `div.w3-padding`
 * per screening that day. Each screening carries its start time twice — a
 * yearless `<b>HH:MM</b>` in the day text, and (crucially, WITH the year) as
 * the UTC `dates=YYYYMMDDTHHMMSSZ/…` query param on its "Dodaj do Kalendarza
 * Google" link — so the calendar link, not the day header, is the
 * authoritative date+time source here; it's converted from UTC to Warsaw
 * wall-clock time.
 *
 * Two screening shapes share the markup, distinguished by their info `<b>`
 * (document order: `b[0]` is the time already read off the calendar link,
 * `b[1]` is the title/info):
 *   - The venue's own daily commercial repertoire — `<b>Title, Year,
 *     Country, genre/genre, runtime.</b>` — title is the text up to the
 *     first `,` or `-`.
 *   - The free monthly "Klub Filmowy 'Kino Znicz'" classics slot —
 *     `<b>Klub Filmowy "Kino Znicz": "Title" genre Country Year/ runtime'/
 *     …/ wstęp wolny</b>` — title is the second quoted string.
 * A screening whose info text contains "zamknięty" ("seans zamknięty" — a
 * closed session reserved for a school group, not open to the public) is
 * dropped: it is not a screening the public can actually attend.
 */
class KinoBielskClient(http: HttpFetch, override val cinema: Cinema = KinoBielsk)
    extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoBielskClient.RepertoireUrl)
  override def sourceUrl: Option[String] = Some(KinoBielskClient.RepertoireUrl)

  def fetch(): Seq[CinemaMovie] =
    KinoBielskClient.parse(http.get(KinoBielskClient.RepertoireUrl), cinema)
}

object KinoBielskClient {

  val BaseUrl       = "https://www.bdkbielsk.pl"
  val RepertoireUrl = s"$BaseUrl/kino/"

  private val CalendarDate = """dates=(\d{8}T\d{6})Z""".r
  private val UtcStamp     = DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss")
  private val Warsaw       = ZoneId.of("Europe/Warsaw")

  private val KlubFilmowyTitle = """Klub Filmowy\s*"Kino Znicz":\s*"([^"]+)"""".r
  // First delimiter (comma, or a hyphen with optional surrounding space) in
  // the venue's own "Title, Year, Country, genre, runtime." info line.
  private val OwnTitle = """^(.+?)(?:,|\s*-\s*)""".r

  private case class RawSlot(title: String, dateTime: LocalDateTime, year: Option[Int])

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)
    val slots = document.select("div.w3-padding").asScala.toSeq.flatMap(parseScreening)

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, None)) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title, releaseYear = group.head.year),
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

  private def parseScreening(block: Element): Option[RawSlot] = {
    val infoBold = block.select("b").asScala.toSeq.lift(1).map(_.text.trim)
    for {
      info     <- infoBold if info.nonEmpty && !info.toLowerCase.contains("zamknięty")
      calendar <- Option(block.selectFirst("a[href*=calendar.google.com/calendar/render]"))
      dateTime <- CalendarDate.findFirstMatchIn(calendar.attr("href")).flatMap { m =>
        Try(LocalDateTime.parse(m.group(1), UtcStamp).atZone(ZoneOffset.UTC).withZoneSameInstant(Warsaw).toLocalDateTime).toOption
      }
    } yield {
      val (title, year) = KlubFilmowyTitle.findFirstMatchIn(info) match {
        case Some(m) => (m.group(1), ScraperParse.productionMeta(info)._2)
        case None    => (OwnTitle.findFirstMatchIn(info).map(_.group(1)).getOrElse(info), ScraperParse.productionMeta(info)._2)
      }
      RawSlot(title.trim, dateTime, year)
    }
  }
}
