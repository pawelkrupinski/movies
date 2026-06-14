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
    val slots = parseDocument(html)

    val byFilmUrl = slots.groupBy(s => (s.title, s.filmUrl))
    byFilmUrl.toSeq.flatMap { case ((title, filmUrl), group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, s.bookingUrl))
        .distinctBy(s => (s.dateTime, s.bookingUrl))
        .sortBy(_.dateTime)
      val directors = group.map(_.directors).find(_.nonEmpty).getOrElse(Seq.empty)
      val year      = group.flatMap(_.year).headOption
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title, releaseYear = year),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = filmUrl,
        synopsis  = None,
        cast      = Seq.empty,
        director  = directors,
        showtimes = showtimes
      ))
    }.sortBy(_.movie.title)
  }
}

object KinoParadoxClient {

  val BaseUrl       = "https://kinoparadox.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar/"

  private val DateFmt = DateTimeFormatter.ofPattern("dd.MM.yyyy")
  private val YearPat = """\b(?:19|20)\d{2}\b""".r

  private[cinemas] case class RawSlot(
    title:      String,
    filmUrl:    Option[String],
    dateTime:   LocalDateTime,
    bookingUrl: Option[String],
    directors:  Seq[String],
    year:       Option[Int]
  )

  /** The `div.item-director` line is slash-delimited:
   *  `reż. <dirs> / <countries> / <year> / <runtime>’`. Each part is optional —
   *  some films carry no `reż.` prefix and some no year — so pick the director
   *  part by its marker and the year by the only 4-digit token. */
  private[cinemas] def parseMeta(itemDirector: String): (Seq[String], Option[Int]) = {
    val parts = itemDirector.split("/").iterator.map(_.trim).filter(_.nonEmpty).toSeq
    val directors = parts.find(_.toLowerCase.startsWith("reż"))
      .map(_.replaceFirst("(?i)^reż\\.?\\s*", "").split(",").map(_.trim).filter(_.nonEmpty).toSeq)
      .getOrElse(Seq.empty)
    val year = parts.iterator.flatMap(p => YearPat.findFirstIn(p)).map(_.toInt).nextOption()
    (directors, year)
  }

  private[cinemas] def parseDocument(html: String): Seq[RawSlot] = {
    val document = Jsoup.parse(html)
    document.select("div.list-item__content__row[data-date]").asScala.toSeq.flatMap { row =>
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

      val (directors, year) = Option(row.selectFirst("div.item-director"))
        .map(e => parseMeta(e.text.trim)).getOrElse((Seq.empty, None))

      for {
        d <- date
        t <- time
        n <- title
      } yield RawSlot(n, filmUrl, LocalDateTime.of(d, t), bookingUrl, directors, year)
    }
  }
}
