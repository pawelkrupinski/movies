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

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = parseHtml(http.get(RepertoireUrl), today, cinema)
}

object KinematografLodzClient {

  val BaseUrl       = "https://muzeumkinematografii.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar/"

  private val DateTimePat = """(\d{2}\.\d{2}\.\d{4})\s+(\d{2}:\d{2})""".r
  private val DateFmt     = DateTimeFormatter.ofPattern("dd.MM.yyyy")
  // The raw title carries `(YYYY)` and, for most films, a `, reż. Director`
  // suffix that `cleanTitle` strips for display. Both are TMDB-identity hints,
  // so extract them before the strip.
  private val ParenYearPat = """\((?:19|20)\d{2}\)""".r
  // Director list after the `reż.` marker, bounded by the first `(` (a trailing
  // `(YYYY)`), `•` (an event/discussion suffix), or the string end. The bare
  // `, Director` form some rows use (no `reż.`) is deliberately not matched —
  // it's indistinguishable from a subtitle and risks false positives.
  private val DirectorPat  = """(?i)reż\.\s*([^(•]+)""".r

  /** The `(YYYY)` production year in the raw title, if present. */
  def parseYear(raw: String): Option[Int] =
    ParenYearPat.findFirstMatchIn(raw).map(_.matched.filter(_.isDigit).toInt)

  /** Director(s) from the `reż. …` suffix, comma-split, with any trailing
   *  sentence punctuation (`Maciej Drygas.`) and empty fragments dropped. Empty
   *  when the title carries no `reż.` marker. */
  def parseDirectors(raw: String): Seq[String] =
    DirectorPat.findFirstMatchIn(raw).map(_.group(1)).toSeq
      .flatMap(_.split(","))
      .map(_.trim.stripSuffix(".").trim)
      .filter(_.nonEmpty)

  /** Strips the `", reż. Director Name"` / `", Director Name"` director suffix
    * and the trailing `" (YYYY)"` release-year suffix the museum appends to the
    * raw title — now via the editable "kino-kinematograf" rules. Delegates so it
    * stays unit-testable here. Examples:
    *   "Znaki Pana Śliwki (2025), reż. Urszula Morga, …" → "Znaki Pana Śliwki"
    *   "Zawieście czerwone latarnie (1991), Zhang Yimou" → "Zawieście czerwone latarnie"
    *   "Klasyk w kinie: Rozmowa (1973)" → "Klasyk w kinie: Rozmowa"
    */
  private[cinemas] def cleanTitle(raw: String): String =
    services.movies.TitleNormalizer.cinemaClean("kino-kinematograf", raw)

  private[cinemas] def parseHtml(html: String, today: LocalDate, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html)
    document.select("article.cwb-movie-item").asScala.toSeq.flatMap(parseItem(_, today, cinema))
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
      movie     = Movie(title = t, rawTitle = rawTitle, releaseYear = rawTitle.flatMap(parseYear)),
      cinema    = cinema,
      posterUrl = poster,
      filmUrl   = filmUrl,
      synopsis  = None,
      cast      = Seq.empty,
      director  = rawTitle.toSeq.flatMap(parseDirectors),
      showtimes = Seq(Showtime(dt, bookingUrl = None))
    )
  }
}
