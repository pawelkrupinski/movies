package services.cinemas.pl

import models._
import org.jsoup.Jsoup
import play.api.libs.json.{JsValue, Json}
import services.cinemas.common.{CinemaScraper, ListingPages, ScrapeHorizon, ScraperParse, SlotsToMovies}
import tools.{HttpFetch, HttpRead}

import java.time.{LocalDate, LocalDateTime, YearMonth}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * The iKsoris booking platform's "Moduł Internet STARTER" skin (SoftCOM
 * Wrocław), whose `rezerwacja/termin.html?idg=N` page is only a month calendar:
 * the screenings of a clicked day are pulled in by `starter/js/modules/Termin.js`
 * from the site's own JSON endpoint, so the page HTML alone lists none. This
 * client calls that endpoint directly — Wejherowskie Centrum Kultury
 * (`bilety.wck.org.pl`, KINO group `idg=1`; its other groups are concerts,
 * theatre and education) is the venue it was built for. Its WordPress site bills
 * only concerts under "Repertuar", and Filmweb carries a week at most.
 *
 * Two calls, both `index/ajax.html`:
 *   - `ajax=pobierzKalendarz&idg=N&year=Y&month=M` → `{"status":"complete",
 *     "kalendarzHtml":…}`; the month's `button.kalendarz-terminow-dzien` cells
 *     carry `data-day="YYYY-MM-DD"`, and a day with screenings is classed
 *     `dzien-z-terminami`. The next-month arrow is never disabled, so the walk
 *     runs from `today`'s month until the first later month with no scheduled day.
 *   - `ajax=pobierzTerminy&idg=N&selectedDate=YYYY-MM-DD` → `{"data":[…]}`, one
 *     object per showing: `wydarzenie` (the title, version word appended —
 *     "HOT SPOT napisy"), `czas` ("2026-09-27 (niedziela) 14:30"),
 *     `terminUrl` (the `rezerwacja/miejsca.html` seat picker, `#` when not
 *     bookable), `wydarzenieOpis` (the plain-text description) and `jezyk`
 *     (usually empty). No runtime, year, director, poster or hall is published.
 */
class IksorisCalendarClient(
  http:  HttpFetch,
  page:  IksorisBookingPage,
  override val cinema: Cinema,
  today: => LocalDate
) extends CinemaScraper {

  import IksorisCalendarClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(page.origin.value)
  override def sourceUrl: Option[String] = Some(page.url)

  // This month's calendar failing propagates — without it no day is known, so a
  // swallowed failure would read as a white "0 films". The months after it are
  // walked by [[ScrapeHorizon.liveMonths]], so a dark month (a summer break) does
  // not hide the programme that resumes after it. A day failing drops only that
  // day, unless every one did.
  def fetch(): Seq[CinemaMovie] = {
    val first     = YearMonth.from(today)
    val thisMonth = scheduledDays(HttpRead.page(http, calendarUrl(page, first)))
    val later     = Seq.newBuilder[LocalDate]
    ScrapeHorizon.liveMonths(first.plusMonths(1)) { month =>
      val days = scheduledDays(HttpRead.page(http, calendarUrl(page, month)))
      later ++= days
      days.nonEmpty
    }
    val days = (thisMonth ++ later.result()).filterNot(_.isBefore(today))
    // Parsed inside the day's Try: a day answering 200 with an HTML session page
    // or a `{"status":"error"}` without its `data` drops that day (and marks the
    // listing incomplete), not the whole scrape — and is never read as empty.
    val attempts = days.map(day => Try(dayShowings(HttpRead.page(http, dayUrl(page, day)))))
    ListingPages.requireAnyReached(attempts)
    parse(attempts.flatMap(_.toOption), cinema)
  }
}

object IksorisCalendarClient {

  def calendarUrl(page: IksorisBookingPage, month: YearMonth): String =
    s"${page.origin.value}/index/ajax.html?ajax=pobierzKalendarz&idg=${page.eventGroup}&year=${month.getYear}&month=${month.getMonthValue}"

  def dayUrl(page: IksorisBookingPage, day: LocalDate): String =
    s"${page.origin.value}/index/ajax.html?ajax=pobierzTerminy&idg=${page.eventGroup}&selectedDate=$day"

  // "2026-09-27 (niedziela) 14:30" — the weekday in between is ignored.
  private val WhenPat = raw"""^(\d{4}-\d{2}-\d{2})\b.*?(${ScraperParse.ClockText})\s*$$""".r
  // "Bez znieczulenia/„WAJDA: re- wizje. Przegląd filmów Andrzeja Wajdy" — a
  // film cycle's name glued on after a slash and an opening Polish quote.
  private val CycleSuffix = """\s*/\s*„.*$""".r

  private case class RawSlot(
    title:    String,
    rawTitle: String,
    dateTime: LocalDateTime,
    booking:  Option[String],
    format:   List[String],
    synopsis: Option[String]
  )

  /** The days a month's calendar marks as having screenings. A reply with no
   *  calendar at all (`{"status":"error"}`) throws: read as a month without
   *  screenings, this month's would turn a failed scrape into a white one. */
  private[pl] def scheduledDays(calendarJson: String): Seq[LocalDate] = {
    val html = (Json.parse(calendarJson) \ "kalendarzHtml").asOpt[String].getOrElse(
      throw new IllegalStateException(s"iKsoris calendar reply carries no kalendarzHtml: ${calendarJson.take(200)}"))
    Jsoup.parse(html).select("button.kalendarz-terminow-dzien.dzien-z-terminami[data-day]").asScala.toSeq
      .flatMap(button => Try(LocalDate.parse(button.attr("data-day"))).toOption).distinct
  }

  /** A day reply's showings; a reply without its `data` array throws — the
   *  calendar marked this day as scheduled, so "no data" is a failed read. */
  private[pl] def dayShowings(dayJson: String): Seq[JsValue] =
    (Json.parse(dayJson) \ "data").asOpt[Seq[JsValue]].getOrElse(
      throw new IllegalStateException(s"iKsoris day reply carries no data: ${dayJson.take(200)}"))

  def parse(days: Seq[Seq[JsValue]], cinema: Cinema): Seq[CinemaMovie] = {
    val slots = days.flatten.flatMap(showing)
    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking, None, s.format)) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title, rawTitle = Some(group.head.rawTitle).filter(_ != title)),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = None,
        synopsis  = group.flatMap(_.synopsis).headOption,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }

  private def showing(js: JsValue): Option[RawSlot] = {
    def field(key: String) = (js \ key).asOpt[String].map(_.trim).filter(_.nonEmpty)
    for {
      rawTitle        <- field("wydarzenie")
      (title, format) <- Some(ScraperParse.extractFormatTags(CycleSuffix.replaceFirstIn(rawTitle, ""))).filter(_._1.nonEmpty)
      dateTime        <- field("czas").collect { case WhenPat(date, time) =>
                           ScraperParse.parseHHmm(time).map(LocalDateTime.of(LocalDate.parse(date), _)) }.flatten
    } yield RawSlot(
      title    = title,
      rawTitle = rawTitle,
      dateTime = dateTime,
      booking  = field("terminUrl").filter(_ != "#"),
      format   = (format ++ field("jezyk").toList.flatMap(ScraperParse.formatTokensIn)).distinct,
      synopsis = field("wydarzenieOpis").map(_.replace("\r\n", "\n").replaceAll("\n{3,}", "\n\n"))
    )
  }
}
