package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Scena Kultura (Knurów). Its repertoire at
 * `www.kinoscenakultura.pl/repertuar` is a server-rendered page — one
 * `div.col-md-6.col-lg-4` block per screening, grouped under day headers. Each
 * block carries everything we need:
 *   - `h3 > a[href^=/repertuar/]` → the film title (link text), whose `href`
 *     ends in a `/YYYY-MM-DD-HH-mm` path segment encoding the LOCAL screening
 *     date+time directly (no timezone math — the day header and the
 *     `event-date` span both confirm the same instant, so the path is the
 *     single source of truth).
 *   - `div.event-attribute` → "Genre, Genre | 12+ | 124 min." — genres are the
 *     comma-list before the first `|`.
 *   - `div.event-img img` → poster.
 *   - `div.event-desc` → synopsis.
 *
 * Tickets are sold via biletyna.pl, but biletyna's events array is currently
 * empty for this venue, so we scrape the cinema's OWN site. The `buy-btn` link
 * is a per-screening biletyna anchor whose `href` is an ISO UTC instant rather
 * than a real booking URL, so no booking link is surfaced.
 */
class KinoScenaKulturaClient(
  http:        HttpFetch,
  override val cinema: Cinema = KinoScenaKultura
) extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoScenaKulturaClient.BaseUrl)
  override def sourceUrl: Option[String] = Some(KinoScenaKulturaClient.RepertoireUrl)

  def fetch(): Seq[CinemaMovie] =
    KinoScenaKulturaClient.parse(http.get(KinoScenaKulturaClient.RepertoireUrl), cinema)
}

object KinoScenaKulturaClient {

  val BaseUrl       = "https://www.kinoscenakultura.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar"

  /** The `/YYYY-MM-DD-HH-mm` tail of a screening's `/repertuar/<slug>/…` link. */
  private val DateTimePat = """/(\d{4})-(\d{2})-(\d{2})-(\d{2})-(\d{2})$""".r

  private case class RawSlot(
    title:     String,
    dateTime:  LocalDateTime,
    posterUrl: Option[String],
    synopsis:  Option[String],
    genres:    Seq[String]
  )

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)
    val slots    = document.select("div.col-md-6.col-lg-4").asScala.toSeq.flatMap(parseBlock)

    slots.groupBy(_.title).toSeq.flatMap { case (title, group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, None))
        .distinctBy(_.dateTime)
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title, genres = group.flatMap(_.genres).distinct),
        cinema    = cinema,
        posterUrl = group.flatMap(_.posterUrl).headOption,
        filmUrl   = None,
        synopsis  = group.flatMap(_.synopsis).headOption,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      ))
    }.sortBy(_.movie.title)
  }

  private def parseBlock(block: Element): Option[RawSlot] =
    for {
      link  <- Option(block.selectFirst("h3 a[href*=/repertuar/]"))
      title  = link.text.trim if title.nonEmpty
      dt    <- DateTimePat.findFirstMatchIn(link.attr("href")).flatMap(dateTimeOf)
    } yield RawSlot(
      title     = title,
      dateTime  = dt,
      posterUrl = Option(block.selectFirst("div.event-img img")).map(_.attr("abs:src")).filter(_.nonEmpty),
      synopsis  = Option(block.selectFirst("div.event-desc")).map(ScraperParse.blockText(_).trim).filter(_.length > 20),
      genres    = genresOf(block)
    )

  /** The local `LocalDateTime` from a `(yyyy, MM, dd, HH, mm)` match. */
  private def dateTimeOf(m: scala.util.matching.Regex.Match): Option[LocalDateTime] =
    Try(LocalDateTime.of(
      LocalDate.of(m.group(1).toInt, m.group(2).toInt, m.group(3).toInt),
      LocalTime.of(m.group(4).toInt, m.group(5).toInt))).toOption

  /** Genres are the comma-list before the first `|` in the
   *  "Thriller, Sci-Fi | 12+ | 124 min." attribute line. */
  private def genresOf(block: Element): Seq[String] =
    Option(block.selectFirst("div.event-attribute")).map(_.text).toSeq
      .flatMap(_.split("\\|").headOption)
      .flatMap(_.split(",").map(_.trim))
      .filter(_.nonEmpty)
}
