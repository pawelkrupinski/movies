package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.HttpFetch

import java.time.{LocalDate, ZoneId}
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Ursynowskie Centrum Kultury „Alternatywy" (Warszawa). Filmweb carries no
 * programme for this venue, so we scrape its own WordPress/Elementor repertoire
 * page directly.
 *
 * Each auditorium screening renders as four heading widgets in document order —
 * day / Polish-genitive month / `HH:MM` / `sala: <room>` — followed by a
 * featured-image link whose `<img alt="Okładka „<title>"" data-src="<poster>">`
 * carries the title and poster. We pair each image with the four headings
 * immediately before it. Two kinds of noise are dropped:
 *
 *   - Non-auditorium rows: the page also lists `Galeria` (exhibition) and `inne`
 *     (other) events; only `sala:` rows are real auditorium screenings.
 *   - A slider re-renders the same cards lower down as bare images with no
 *     preceding date headings — an image with fewer than four headings buffered
 *     before it is a slider tile and is skipped.
 *
 * Dates carry no year on the page; the year is inferred from `today` (rolling to
 * next year for a month already well in the past). TMDB enriches
 * synopsis/runtime/genres downstream, so this leaves those empty and carries
 * only the title, poster, detail-page URL, and showtimes (with room).
 */
class AlternatywyClient(
  http:  HttpFetch,
  today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper with OnlyMovieEventsFilter {
  import AlternatywyClient._

  val cinema: Cinema = KinoAlternatywy

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(RepertoireUrl)
  override def sourceUrl: Option[String] = Some(RepertoireUrl)

  protected def fetchUnfiltered(): Seq[CinemaMovie] =
    SlotsToMovies.fold(
      parseRepertoire(http.get(RepertoireUrl)).filter(_.title.nonEmpty),
      titleOf    = _.title,
      showtimeOf = _.showtime,
      distinctBy = s => (s.dateTime, s.room)
    ) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title, rawTitle = group.map(_.rawTitle).headOption),
        cinema    = cinema,
        posterUrl = group.flatMap(_.posterUrl).headOption,
        filmUrl   = group.flatMap(_.filmUrl).headOption,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }

  private case class Screening(title: String, rawTitle: String, showtime: Showtime, posterUrl: Option[String], filmUrl: Option[String])

  /** Parse the repertoire page into auditorium screenings. */
  private def parseRepertoire(html: String): Seq[Screening] = {
    val nodes = Jsoup.parse(html)
      .select("span.elementor-heading-title, div.elementor-widget-theme-post-featured-image img").asScala
    val headings = mutable.ArrayBuffer.empty[String]
    val out      = mutable.ArrayBuffer.empty[Screening]
    nodes.foreach { element =>
      if (element.tagName == "span") headings += element.text().trim
      else {
        if (headings.length >= 4)
          screeningFrom(headings.takeRight(4).toSeq, element).foreach(out += _)
        headings.clear()
      }
    }
    out.toSeq
  }

  private def screeningFrom(quad: Seq[String], img: Element): Option[Screening] = {
    val Seq(dayStr, monthStr, timeStr, roomStr) = quad
    if (!roomStr.toLowerCase.startsWith("sala:")) return None
    val rawTitle = img.attr("alt")
    val title    = cleanTitle(rawTitle)
    if (title.isEmpty) return None
    for {
      day        <- Try(dayStr.toInt).toOption
      month      <- Months.get(monthStr.trim.toLowerCase)
      (hh, mm)   <- parseTime(timeStr)
      dateTime   <- Try(inferYear(day, month).atTime(hh, mm)).toOption
    } yield {
      val room      = Some(roomStr.split(":", 2).last.trim).filter(_.nonEmpty)
      val posterUrl = Some(img.attr("data-src")).map(_.trim).filter(_.nonEmpty)
      val filmUrl   = img.parents().asScala.find(_.tagName == "a").map(_.attr("href")).filter(_.nonEmpty)
      Screening(title, rawTitle, Showtime(dateTime, bookingUrl = None, room = room), posterUrl, filmUrl)
    }
  }

  /** The page omits the year; pick the one that makes the date upcoming, rolling
   *  to next year only for a month already more than a week past. */
  private def inferYear(day: Int, month: Int): LocalDate = {
    val candidate = LocalDate.of(today.getYear, month, day)
    if (candidate.isBefore(today.minusWeeks(1))) candidate.plusYears(1) else candidate
  }
}

object AlternatywyClient {
  val RepertoireUrl = "https://alternatywy.art/repertuar/"

  /** Polish genitive month names as they appear in the date headings. */
  private val Months: Map[String, Int] = Map(
    "stycznia" -> 1, "lutego" -> 2, "marca" -> 3, "kwietnia" -> 4, "maja" -> 5, "czerwca" -> 6,
    "lipca" -> 7, "sierpnia" -> 8, "września" -> 9, "października" -> 10, "listopada" -> 11, "grudnia" -> 12
  )

  private val TimePat = """^(\d{1,2}):(\d{2})$""".r

  private def parseTime(s: String): Option[(Int, Int)] = s.trim match {
    case TimePat(h, m) =>
      val hh = h.toInt; val mm = m.toInt
      if (hh < 24 && mm < 60) Some(hh -> mm) else None
    case _ => None
  }

  /** Titles arrive in the image alt as `Okładka „Title"`, `Okładka Title`, or
   *  with a trailing event subtitle (`„Flying Lion"  Adam Święs Trio`). The
   *  `Okładka` / quotes / whitespace cleanup now lives in the editable
   *  "kino-alternatywy" rules (see TitleRuleDefaults); this delegates so it stays
   *  unit-testable here. */
  def cleanTitle(alt: String): String =
    services.movies.TitleNormalizer.cinemaClean("kino-alternatywy", alt)
}
