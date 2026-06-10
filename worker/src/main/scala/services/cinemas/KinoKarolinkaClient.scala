package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Karolinka (CK Karolinka, Lubliniec) — a mixed cultural venue whose
 * events list at `karolinka.art.pl/wydarzenia` is server-rendered, one
 * `div.event-item` per event (films AND concerts/theatre share the markup).
 * A FILM is identified by its heading containing "| Kino Karolinka" and a
 * single screening time (`…, godz. HH:MM`); non-film events carry neither.
 * Per event:
 *   - `h2[class*=--heading]` → "<Film Title> | Kino Karolinka [| <series>]";
 *     the title is the first pipe-segment
 *   - `time[class*=--date]` `datetime` attr → ISO date "YYYY-MM-DD" (year present)
 *   - the `time`'s span text → "…, godz. HH:MM"
 *   - `a[class*=--buy-ticket-button]` → booking link
 *
 * Previously scraped from Filmweb, which had silently gone empty for the venue.
 */
class KinoKarolinkaClient(http: HttpFetch, override val cinema: Cinema = KinoKarolinka)
    extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoKarolinkaClient.RepertoireUrl)

  def fetch(): Seq[CinemaMovie] =
    KinoKarolinkaClient.parse(http.get(KinoKarolinkaClient.RepertoireUrl), cinema)
}

object KinoKarolinkaClient {

  val BaseUrl       = "https://karolinka.art.pl"
  val RepertoireUrl = s"$BaseUrl/wydarzenia"

  private val FilmMarker  = "Kino Karolinka"
  private val TimePat     = """godz\.?\s*(\d{1,2}:\d{2})""".r

  private case class RawSlot(title: String, dateTime: LocalDateTime, booking: Option[String], filmUrl: Option[String])

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val doc = Jsoup.parse(html, BaseUrl)

    val slots = doc.select("div.event-item").asScala.toSeq.flatMap(parseEvent)

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
        filmUrl   = group.flatMap(_.filmUrl).headOption,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      ))
    }.sortBy(_.movie.title)
  }

  /** A card is a film only when its heading carries the "Kino Karolinka" tag
   *  and it has a concrete `godz. HH:MM` time — that excludes concerts,
   *  theatre and multi-day festivals. */
  private def parseEvent(card: Element): Option[RawSlot] =
    for {
      heading <- Option(card.selectFirst("h2[class*=--heading]")).map(_.text.trim)
      if heading.contains(FilmMarker)
      timeEl  <- Option(card.selectFirst("time[class*=--date]"))
      date    <- Try(LocalDate.parse(timeEl.attr("datetime"))).toOption
      time    <- TimePat.findFirstMatchIn(timeEl.text).flatMap(m => ScraperParse.parseHHmm(m.group(1)))
      // Headings are "<Film Title> | Kino Karolinka [| <series>]" — the film
      // title is the first pipe-segment; the rest are venue/series tags.
      title    = heading.split("\\|").head.trim if title.nonEmpty
    } yield RawSlot(
      title    = title,
      dateTime = LocalDateTime.of(date, time),
      booking  = Option(card.selectFirst("a[class*=--buy-ticket-button]")).map(_.attr("abs:href")).filter(_.nonEmpty),
      filmUrl  = Option(card.selectFirst("a[class*=--main-link]")).map(_.attr("abs:href")).filter(_.nonEmpty)
    )
}
