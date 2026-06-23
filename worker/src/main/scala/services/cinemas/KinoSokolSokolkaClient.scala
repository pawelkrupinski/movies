package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Sokół (Sokółka). Its repertoire at `kinosokolka.pl/repertuar/` is a
 * WordPress page built with the "MEC" (Modern Events Calendar) plugin, rendered
 * as a monthly calendar grid. Each day cell (`dt.mec-calendar-day[data-mec-cell=
 * "YYYYMMDD"]`) holds one anchor per screening series:
 *   - `a.event-single-link-novel[href]` → the film's MEC detail page, reused
 *     across every date the series runs
 *   - `h4.mec-event-title`              → the (ALL-CAPS) film title
 *   - `span.mec-label-normal`           → version labels ("2D", "Dubbing",
 *     "Napisy", an age tag) → [[Showtime.format]] tokens
 *
 * The grid carries the DATE but NOT the time — MEC only renders `HH:MM` on each
 * series' detail page (`<abbr class="mec-events-abbr">18:00 - 19:50</abbr>`), so
 * we fetch the listing, then each detail page in parallel and read the start
 * time off it. One detail page = one start time, applied to every grid date of
 * that series. (The detail-page JSON-LD `startDate` is date-only, so it can't
 * supply the time; the grid's per-date coverage plus the detail page's time is
 * the only complete source.)
 *
 * Previously scraped from biletyna.pl, which no longer carries the venue's film
 * repertoire. A municipal venue whose listing can also carry non-film events,
 * so the non-movie title filter is mixed in.
 */
class KinoSokolSokolkaClient(http: HttpFetch, override val cinema: Cinema = KinoSokolSokolka)
    extends CinemaScraper with OnlyMovieEventsFilter {

  import KinoSokolSokolkaClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(RepertoireUrl)
  override def sourceUrl: Option[String] = Some(RepertoireUrl)

  protected def fetchUnfiltered(): Seq[CinemaMovie] = {
    val series = parseGrid(http.get(RepertoireUrl))
    val urls   = series.map(_.detailUrl).distinct

    val timeByUrl = ParallelDetailFetch("kino-sokolka", urls, 1.minute) { url =>
      Try(http.get(url)).toOption.flatMap(parseStartTime)
    }

    val slots = series.flatMap { s =>
      timeByUrl.get(s.detailUrl).flatten.toSeq.flatMap { time =>
        s.dates.map(date => RawSlot(s.title, LocalDateTime.of(date, time), s.detailUrl, s.format))
      }
    }

    SlotsToMovies.fold(slots, _.title, sl => Showtime(sl.dateTime, Some(sl.url), format = sl.format)) {
      (title, group, showtimes) =>
        CinemaMovie(
          movie     = Movie(title),
          cinema    = cinema,
          posterUrl = None,
          filmUrl   = group.map(_.url).headOption,
          synopsis  = None,
          cast      = Seq.empty,
          director  = Seq.empty,
          showtimes = showtimes
        )
    }
  }
}

object KinoSokolSokolkaClient {

  val BaseUrl       = "https://kinosokolka.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar/"

  /** "YYYYMMDD" as the calendar cell stamps the day. */
  private val CellDate = """(\d{4})(\d{2})(\d{2})""".r

  /** One screening series off the grid: a single detail page run across one or
   *  more calendar dates, all sharing the labels and (detail-page) time. */
  private case class Series(title: String, dates: Seq[LocalDate], detailUrl: String, format: List[String])

  private case class RawSlot(title: String, dateTime: LocalDateTime, url: String, format: List[String])

  /**
   * Collapse the calendar grid into one [[Series]] per detail page. A series'
   * anchor repeats across every day cell it runs in (same `href`); we gather
   * those cells' dates under the URL and keep the title/labels from the first.
   */
  private def parseGrid(html: String): Seq[Series] = {
    val document = Jsoup.parse(html, BaseUrl)

    case class Cell(date: LocalDate, title: String, url: String, format: List[String])

    val cells = document.select("dt.mec-calendar-day[data-mec-cell]").asScala.toSeq.flatMap { cell =>
      val date = CellDate.findFirstMatchIn(cell.attr("data-mec-cell"))
        .flatMap(m => Try(LocalDate.of(m.group(1).toInt, m.group(2).toInt, m.group(3).toInt)).toOption)
      date.toSeq.flatMap { d =>
        cell.select("a.event-single-link-novel").asScala.toSeq.flatMap { a =>
          for {
            title <- Option(a.selectFirst("h4.mec-event-title")).map(_.text.trim).filter(_.nonEmpty)
            url   <- Option(a.attr("abs:href")).filter(_.nonEmpty)
          } yield Cell(d, title, url, formatTokens(a))
        }
      }
    }

    cells.groupBy(_.url).toSeq.flatMap { case (url, group) =>
      group.headOption.map(first =>
        Series(first.title, group.map(_.date).distinct.sorted, url, first.format))
    }
  }

  /** The version labels MEC stamps on a screening ("2D", "Dubbing", "Napisy",
   *  plus a "Od N lat" age rating) mapped to the app's [[Showtime.format]]
   *  tokens. Unknown labels (the age tag) yield nothing. */
  private def formatTokens(anchor: Element): List[String] =
    anchor.select("span.mec-label-normal").asScala.toList
      .flatMap(l => ScraperParse.FormatToken.get(l.text.trim.toLowerCase))
      .distinct

  /** The screening start time off a MEC detail page. The "Godzina" row renders
   *  `<abbr class="mec-events-abbr">18:00 - 19:50</abbr>`; we keep the first
   *  `HH:mm` (the start). `None` when the row is absent or unparseable. */
  private def parseStartTime(html: String): Option[LocalTime] =
    Jsoup.parse(html, BaseUrl)
      .select("abbr.mec-events-abbr").asScala
      .flatMap(el => ScraperParse.parseHHmm(el.text))
      .headOption
}
