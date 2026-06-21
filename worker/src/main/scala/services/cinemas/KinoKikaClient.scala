package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Kika (Kraków) — a four-screen kinokawiarnia at ul. Krasickiego 18, run
 * by Fundacja Wspierania Kultury Filmowej Cyrk Edison (the same foundation that
 * operates Kino Agrafka). The venue does not publish a standalone day-by-day
 * schedule on its own domain; screenings are managed through the shared
 * ticketing portal at `bilety.kinokika.pl`.
 *
 * The portal renders a full week's screenings in server-side HTML. Each
 * screening is a `div.repertoire-once[class~="row YYYY-MM-DD"]` element
 * (the date is encoded as a CSS class). Within each element:
 *   - `div.title a[href]` — the film title (anchor text, ALL-CAPS on the page,
 *     normalised downstream) and the booking URL relative to the portal
 *     (`/index.php/repertoire.html?id=N`).
 *   - `div.date` — location, human-readable date ("niedziela, 7 czerwca 2026"),
 *     and start time ("godz. HH:MM").
 *
 * The room name is extracted from the location div (e.g. "sala KIKA",
 * "sala PUFA") and stored in `Showtime.room`.
 */
class KinoKikaClient(http: HttpFetch, override val cinema: Cinema) extends CinemaScraper {

  import KinoKikaClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val html  = http.get(RepertoireUrl)
    val slots = parseDocument(html)

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, Some(s.bookingUrl), s.room)) { (title, _, showtimes) =>
      CinemaMovie(
        movie     = Movie(tools.TextNormalization.titleCaseIfAllLower(title)),
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
}

object KinoKikaClient {

  val BaseUrl       = "https://bilety.kinokika.pl"
  val RepertoireUrl = s"$BaseUrl/"

  // CSS class on row divs: "row 2026-06-07" (date embedded as a class token)
  private val DateClassPat = """(\d{4}-\d{2}-\d{2})""".r
  // "godz. 16:45"
  private val GodzPat = """godz\.\s*(\d{1,2}:\d{2})""".r
  // "sala KIKA", "sala PUFA" etc. — the room name after "sala"
  private val SalaPat = """sala\s+(\S+)""".r

  private[cinemas] case class RawSlot(
    title:      String,
    dateTime:   LocalDateTime,
    bookingUrl: String,
    room:       Option[String]
  )

  private[cinemas] def parseDocument(html: String): Seq[RawSlot] = {
    val document = Jsoup.parse(html)
    document.select("div.repertoire-once").asScala.toSeq.flatMap { row =>
      val classes = row.attr("class")
      val date = DateClassPat.findFirstIn(classes).flatMap(s => Try(LocalDate.parse(s)).toOption)

      val titleElement = Option(row.selectFirst("div.title a[href]"))
      val title   = titleElement.map(_.text.trim.replaceAll("\\s+", " ")).filter(_.nonEmpty)

      // Booking link is the anchor in div.title itself (the one inside div.link
      // is a duplicate "kup bilet" button)
      val bookingHref = titleElement.map(_.attr("href").trim).filter(_.nonEmpty)
      val bookingUrl  = bookingHref.map(h => if (h.startsWith("http")) h else s"$BaseUrl$h")

      val dateDiv = Option(row.selectFirst("div.date"))
      val timeText = dateDiv.flatMap(d => GodzPat.findFirstMatchIn(d.text)).map(_.group(1))
      val time    = timeText.flatMap(ScraperParse.parseHHmm)
      val room    = dateDiv.flatMap(d => SalaPat.findFirstMatchIn(d.text)).map(_.group(1))

      for {
        d  <- date
        t  <- time
        n  <- title
        bu <- bookingUrl
      } yield RawSlot(n, LocalDateTime.of(d, t), bu, room)
    }
  }
}
