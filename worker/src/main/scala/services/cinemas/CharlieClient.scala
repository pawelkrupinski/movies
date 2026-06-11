package services.cinemas

import models._
import org.jsoup.Jsoup
import play.api.libs.json._
import tools.HttpFetch

import java.nio.charset.Charset
import java.time.OffsetDateTime
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Charlie (Łódź, ul. Piotrkowska 203/205) — an arthouse cinema whose
 * homepage at `charlie.pl` is server-rendered with one
 * `<script type="application/ld+json">` block per screening: a schema.org
 * `ScreeningEvent` carrying the title, ISO start time, film-page URL, and a
 * nested `workPresented` Movie with the poster and director. The homepage
 * lists the CURRENT day's full programme (there's no public multi-day route
 * that changes the rendered schedule), which is enough for a daily scrape.
 *
 * Two quirks the parser handles:
 *   - **Charset.** The page is raw ISO-8859-2 with NO charset declared
 *     (`Content-Type: text/html`, no `<meta charset>`). The default UTF-8
 *     decode mangles every Polish character in the titles, so we fetch the
 *     undecoded bytes via `HttpFetch.getBytes` and decode ISO-8859-2 here.
 *   - **Booking link.** The ticketing link (`bilety.charlie.pl/MSI/Default.aspx
 *     ?event_id=N`) lives in the HTML row immediately BEFORE each
 *     `ScreeningEvent` block, not inside the JSON-LD — every screening's
 *     booking `<a>` is the nearest such link preceding its block, so we pair
 *     them by document order.
 */
class CharlieClient(http: HttpFetch, override val cinema: Cinema = KinoCharlie) extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(CharlieClient.PageUrl, CharlieClient.BookingHost)
  override def sourceUrl: Option[String] = Some(CharlieClient.PageUrl)

  def fetch(): Seq[CinemaMovie] =
    CharlieClient.parse(new String(http.getBytes(CharlieClient.PageUrl), CharlieClient.PageCharset), cinema)
}

object CharlieClient {

  val PageUrl     = "https://www.charlie.pl/"
  // The page ships no charset; it is raw ISO-8859-2 (Polish Latin-2).
  val PageCharset: Charset = Charset.forName("ISO-8859-2")

  private val BookingHost = "https://bilety.charlie.pl/MSI/Default.aspx?event_id="
  private val BookingLink = """bilety\.charlie\.pl/MSI/Default\.aspx\?event_id=(\d+)""".r

  // schema.org `startDate` here is ISO-8601 with a zone offset but NO seconds
  // ("2026-06-06T12:10+01:00"); the offset-with-minutes formatter parses it,
  // and we keep only the wall-clock LocalDateTime (the app reasons in Warsaw
  // local time).
  private val IsoOffset = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mmXXX")

  private case class RawSlot(
    title:    String,
    dateTime: java.time.LocalDateTime,
    filmUrl:  Option[String],
    booking:  Option[String],
    poster:   Option[String],
    director: Option[String]
  )

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val doc = Jsoup.parse(html)
    val slots = doc.select("script[type=application/ld+json]").asScala.toSeq
      .map(_.data())
      .flatMap(block => parseEvent(block).map(slot => slot -> block))
      .map { case (slot, block) =>
        // The booking link precedes the block in the source; recover its
        // position via the raw HTML so we can pair the nearest preceding
        // `event_id` link with this screening.
        slot.copy(booking = bookingBefore(html, block))
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
        posterUrl = group.flatMap(_.poster).headOption,
        filmUrl   = group.flatMap(_.filmUrl).headOption,
        synopsis  = None,
        cast      = Seq.empty,
        director  = group.flatMap(_.director).distinct,
        showtimes = showtimes
      ))
    }.sortBy(_.movie.title)
  }

  private def parseEvent(block: String): Option[RawSlot] =
    Try(Json.parse(block)).toOption
      .filter(js => (js \ "@type").asOpt[String].contains("ScreeningEvent"))
      .flatMap { js =>
        for {
          title <- (js \ "name").asOpt[String].map(_.trim).filter(_.nonEmpty)
          start <- (js \ "startDate").asOpt[String]
          dt    <- Try(OffsetDateTime.parse(start, IsoOffset).toLocalDateTime).toOption
        } yield RawSlot(
          title    = title,
          dateTime = dt,
          filmUrl  = (js \ "url").asOpt[String].filter(_.nonEmpty),
          booking  = None,
          poster   = (js \ "workPresented" \ "image").asOpt[String].filter(_.nonEmpty),
          director = (js \ "workPresented" \ "director").asOpt[String].map(_.trim).filter(_.nonEmpty)
        )
      }

  /** The nearest `bilety.charlie.pl` booking link appearing BEFORE the given
    * JSON-LD block in the source HTML. Each screening's `<a>` row precedes its
    * `ScreeningEvent` script, so the last match before the block is this
    * screening's. */
  private def bookingBefore(html: String, block: String): Option[String] = {
    val at = html.indexOf(block)
    if (at < 0) None
    else BookingLink.findAllMatchIn(html.substring(0, at)).map(_.group(1)).toSeq.lastOption
      .map(id => s"$BookingHost$id&typetran=0")
  }
}
