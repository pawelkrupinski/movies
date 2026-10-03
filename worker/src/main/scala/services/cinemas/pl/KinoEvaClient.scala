package services.cinemas.pl

import services.cinemas.common.{CinemaScraper, ScraperParse, SlotsToMovies}
import models._
import tools.{HttpFetch, HttpRead}
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

import java.time.{LocalDate, LocalDateTime}
import scala.jdk.CollectionConverters._

/**
 * Kino Eva (Międzyzdroje). `kino-eva.com.pl/repertuar/` (a BeTheme WordPress page)
 * is the whole advance programme on one page: per day an `h2.title` header
 * (`Piątek,  25 września 2026r.` — sometimes suffixed `- KINO NIECZYNNE` with no
 * table after it, or with an occasion like `- DZIEŃ CHŁOPAKA`), followed by a
 * `table.table_theater` with one row per screening:
 *
 *   - `td.c1` → `<h4 class="themecolor">Title <h4>genres` (the markup never closes
 *     its `h4`s, so the title is the first heading's own text and the genres the
 *     rest of the cell)
 *   - `td.c2` → `HH:MM`
 *   - `td.c3` → `od N lat` (age), then a second `td.c3` → `1h 30min` (runtime)
 *
 * The same film is typed in two casings across days (`MISTYCZKA` / `Mistyczka`),
 * so a shouted title is sentence-cased before grouping. Booking is by phone only,
 * so no booking URL is surfaced.
 *
 * The site's HTTPS certificate is issued for another host name (it fails
 * validation for `kino-eva.com.pl`), while plain HTTP serves the page without a
 * redirect — so the client reads it over `http://`.
 */
class KinoEvaClient(http: HttpFetch, override val cinema: Cinema = KinoEva) extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoEvaClient.RepertoireUrl)
  override def sourceUrl: Option[String] = Some(KinoEvaClient.RepertoireUrl)

  def fetch(): Seq[CinemaMovie] = KinoEvaClient.parse(HttpRead.page(http, KinoEvaClient.RepertoireUrl), cinema)
}

object KinoEvaClient {

  val RepertoireUrl = "http://kino-eva.com.pl/repertuar/"

  private val DayHeader = """(\d{1,2})\s+(\p{L}+)\s+(\d{4})""".r
  private val Runtime   = """(?:(\d+)\s*h)?\s*(?:(\d+)\s*min)?""".r
  private val Age       = """(?i)od\s+(\d+)\s+lat""".r

  private case class RawSlot(
    title:    String,
    rawTitle: String,
    dateTime: LocalDateTime,
    genres:   Seq[String],
    age:      Option[String],
    runtime:  Option[Int]
  )

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, RepertoireUrl)
    // Headers and tables in document order; each table belongs to the header before it.
    var currentDate: Option[LocalDate] = None
    val slots = document.select("h2.title, table.table_theater").asScala.toSeq.flatMap { el =>
      if (el.tagName == "h2") {
        currentDate = DayHeader.findFirstMatchIn(el.text).flatMap(m => ScraperParse.parseDayMonthYear(m.matched))
        Seq.empty
      } else currentDate.toSeq.flatMap(date => el.select("td.c1").asScala.toSeq.flatMap(slotOf(_, date)))
    }

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, None)) { (title, group, showtimes) =>
      val head = group.head
      CinemaMovie(
        movie     = Movie(
          title          = title,
          runtimeMinutes = group.flatMap(_.runtime).headOption,
          genres         = head.genres,
          rawTitle       = Option(head.rawTitle).filter(_ != title)
        ),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes,
        ageRating = group.flatMap(_.age).headOption
      )
    }
  }

  private def slotOf(titleCell: Element, date: LocalDate): Option[RawSlot] = {
    val cells = titleCell.parent.children.asScala.toSeq.dropWhile(_ ne titleCell).drop(1)
    for {
      heading  <- Option(titleCell.selectFirst("h4.themecolor"))
      rawTitle  = heading.ownText.trim if rawTitle.nonEmpty
      time     <- cells.find(_.hasClass("c2")).flatMap(c => ScraperParse.parseHHmm(c.text.trim))
    } yield {
      val details = cells.filter(_.hasClass("c3")).map(_.text.trim)
      RawSlot(
        title    = titleOf(rawTitle),
        rawTitle = rawTitle,
        dateTime = LocalDateTime.of(date, time),
        genres   = titleCell.text.trim.stripPrefix(heading.ownText.trim).split(",").toSeq.map(_.trim).filter(_.nonEmpty),
        age      = details.flatMap(Age.findFirstMatchIn(_)).headOption.map(m => s"${m.group(1)}+"),
        runtime  = details.flatMap(runtimeOf).headOption
      )
    }
  }

  /** A shouted title (`SPA WEEKEND`) is sentence-cased so it groups with the
   *  same film typed normally on another day; a mixed-case title is kept. */
  private def titleOf(raw: String): String =
    if (raw.exists(_.isLower)) raw else ScraperParse.sentenceCase(raw)

  /** "1h 30min" → 90; "95min" → 95; anything without a number → None. */
  private def runtimeOf(text: String): Option[Int] =
    Runtime.findAllMatchIn(text).find(m => m.group(1) != null || m.group(2) != null).map { m =>
      Option(m.group(1)).map(_.toInt * 60).getOrElse(0) + Option(m.group(2)).map(_.toInt).getOrElse(0)
    }
}
