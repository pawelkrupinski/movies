package services.cinemas

import scala.math.Ordering.Implicits.infixOrderingOps

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, ZoneId}
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Kinematograf w Łodzi — the cinema inside the Muzeum Kinematografii
 * w Łodzi (Palace of Poznański at pl. Zwycięstwa 1). The repertoire at
 * `/repertuar/` is a server-rendered WordPress page listing the coming
 * weeks' screenings, one `article.cwb-movie-item` per showing.
 *
 * Each `cwb-movie-item` carries:
 *   - `a.cwb-movie-card-link[title="Przejdź do seansu: <raw>"]` — the raw
 *     title, which follows the pattern `Film Name (Year), reż. Director` or
 *     `Programme Prefix: Film Name (Year)`. The director suffix is stripped
 *     so TMDB can match the bare title; programme prefixes (e.g. "Klasyk w
 *     kinie:", "Mały Kinematograf:", "Federico Fellini: ciao a tutti! –")
 *     are kept verbatim and handled by downstream `TitleNormalizer`.
 *   - `a.cwb-movie-card-link[href]` — the film's detail-page URL, used as
 *     `filmUrl`.
 *   - `div.date-time` — date + time in `DD.MM.YYYY HH:MM` format (rendered
 *     with extra whitespace between the two). The year is always present, so
 *     no year-inference is needed.
 *   - `img.wp-post-image[data-src]` — the poster image (lazy-loaded).
 *
 * Booking is via `sklep.kinomuzeum.pl/MSI/mvc/pl/` (general page) — the
 * listing does not carry per-screening booking URLs, so `bookingUrl` is
 * always `None`. TMDB enriches runtime, genres and synopsis downstream.
 *
 * The listing page is filtered to screenings on or after `today` so that
 * past events (which the CMS keeps in the listing for archive purposes)
 * are not included.
 */
class KinematografLodzClient(
  http:             HttpFetch,
  override val cinema: Cinema,
  today:            LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import KinematografLodzClient._

  def fetch(): Seq[CinemaMovie] = parseHtml(http.get(RepertoireUrl), today, cinema)
}

object KinematografLodzClient {

  val BaseUrl       = "https://muzeumkinematografii.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar/"

  private val DateTimePat = """(\d{2}\.\d{2}\.\d{4})\s+(\d{2}:\d{2})""".r
  private val DateFmt     = DateTimeFormatter.ofPattern("dd.MM.yyyy")

  /** Strips the `", reż. Director Name"` or `", Director Name"` suffix that
    * the museum appends to the raw title attribute. Examples:
    *   "Znaki Pana Śliwki (2025), reż. Urszula Morga, Bartosz Mikołajczyk"
    *     → "Znaki Pana Śliwki (2025)"
    *   "Zawieście czerwone latarnie (1991), Zhang Yimou"
    *     → "Zawieście czerwone latarnie (1991)"
    *   "Klasyk w kinie: Rozmowa (1973)" → unchanged
    */
  private[cinemas] def cleanTitle(raw: String): String = {
    // Strip ", reż. …" (everything from the last comma + "reż." onward).
    val noRez = """,\s*reż\.\s*.+$""".r.replaceFirstIn(raw.trim, "").trim
    // Strip a trailing ", Firstname Lastname" (one comma + exactly two capitalised words)
    // to catch bare foreign director names not preceded by "reż.".
    """,\s+\p{Lu}\S+\s+\p{Lu}\S+$""".r.replaceFirstIn(noRez, "").trim
  }

  private[cinemas] def parseHtml(html: String, today: LocalDate, cinema: Cinema): Seq[CinemaMovie] = {
    val doc = Jsoup.parse(html)
    doc.select("article.cwb-movie-item").asScala.toSeq.flatMap(parseItem(_, today, cinema))
  }

  private def parseItem(item: Element, today: LocalDate, cinema: Cinema): Option[CinemaMovie] = {
    val link = Option(item.selectFirst("a.cwb-movie-card-link[href]"))
    val rawTitle = link.map(_.attr("title"))
                     .map(_.replaceFirst("^Przejdź do seansu:\\s*", "").trim)
                     .filter(_.nonEmpty)
    val title   = rawTitle.map(cleanTitle).filter(_.nonEmpty)
    val filmUrl = link.map(_.attr("href")).filter(_.nonEmpty)

    val dtText = Option(item.selectFirst("div.date-time")).map(_.text.trim).getOrElse("")
    val dtOpt  = DateTimePat.findFirstMatchIn(dtText).flatMap { m =>
      Try {
        val date = LocalDate.parse(m.group(1), DateFmt)
        val time = ScraperParse.parseHHmm(m.group(2))
        time.map(LocalDateTime.of(date, _))
      }.toOption.flatten
    }

    // Filter out past events (the CMS keeps the full year in the listing).
    val dtFiltered = dtOpt.filter(_.toLocalDate >= today)

    val poster = Option(item.selectFirst("img.wp-post-image[data-src]"))
                   .map(_.attr("data-src"))
                   .filter(_.nonEmpty)
                   .orElse(Option(item.selectFirst("img.wp-post-image[src]"))
                     .map(_.attr("src")).filter(_.startsWith("http")))

    for {
      t  <- title
      dt <- dtFiltered
    } yield CinemaMovie(
      movie     = Movie(title = t),
      cinema    = cinema,
      posterUrl = poster,
      filmUrl   = filmUrl,
      synopsis  = None,
      cast      = Seq.empty,
      director  = Seq.empty,
      showtimes = Seq(Showtime(dt, bookingUrl = None))
    )
  }
}
