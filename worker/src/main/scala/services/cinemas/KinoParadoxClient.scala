package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Paradox (Kraków) — independent arthouse cinema. The venue publishes a
 * server-rendered weekly repertoire at `/repertuar/` built on WordPress.
 *
 * Each screening is a `div.list-item__content__row[data-date="DD.MM.YYYY"]`
 * element. Within it:
 *   - `div.item-time` — start time, e.g. "16:45".
 *   - `a.item-title` — the film title (as the anchor's own text node, before
 *     the nested `div.item-photo` child) and a cinema-relative film URL
 *     (`/naekranie/<slug>`).
 *   - `a.btn[href]` — a "kup bilet" booking URL on
 *     `bilety.kinoparadox.pl/index.php/repertoire.html?id=N`.
 *
 * The date is parsed from the `data-date` attribute (`DD.MM.YYYY`), so the
 * year is unambiguous and no inference is needed.
 */
class KinoParadoxClient(http: HttpFetch, override val cinema: Cinema) extends CinemaScraper {

  import KinoParadoxClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val html  = http.get(RepertoireUrl)
    val slots = parseDoc(html)

    val byFilmUrl = slots.groupBy(s => (s.title, s.filmUrl))
    byFilmUrl.toSeq.flatMap { case ((title, filmUrl), group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, s.bookingUrl))
        .distinctBy(s => (s.dateTime, s.bookingUrl))
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = filmUrl,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      ))
    }.sortBy(_.movie.title)
  }
}

object KinoParadoxClient {

  val BaseUrl       = "https://kinoparadox.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar/"

  private val DateFmt = DateTimeFormatter.ofPattern("dd.MM.yyyy")

  private[cinemas] case class RawSlot(
    title:      String,
    filmUrl:    Option[String],
    dateTime:   LocalDateTime,
    bookingUrl: Option[String]
  )

  private[cinemas] def parseDoc(html: String): Seq[RawSlot] = {
    val doc = Jsoup.parse(html)
    doc.select("div.list-item__content__row[data-date]").asScala.toSeq.flatMap { row =>
      val dateAttr = row.attr("data-date").trim
      val date     = Try(LocalDate.parse(dateAttr, DateFmt)).toOption
      val time     = Option(row.selectFirst("div.item-time")).flatMap(e => ScraperParse.parseHHmm(e.text.trim))

      // The title anchor contains both the text title and a nested img div;
      // ownText() returns only the direct text node, which is the film title.
      val titleAnchor = Option(row.selectFirst("a.item-title"))
      val title       = titleAnchor.map(_.ownText().trim).filter(_.nonEmpty)
      val filmHref    = titleAnchor.map(_.attr("href").trim).filter(_.nonEmpty)
      val filmUrl     = filmHref.map(h => if (h.startsWith("http")) h else s"$BaseUrl$h")

      val bookingUrl = Option(row.selectFirst("a.btn[href]"))
        .map(_.attr("href").trim).filter(_.nonEmpty)
        .map(h => if (h.startsWith("http")) h else s"$BaseUrl$h")

      for {
        d <- date
        t <- time
        n <- title
      } yield RawSlot(n, filmUrl, LocalDateTime.of(d, t), bookingUrl)
    }
  }
}
