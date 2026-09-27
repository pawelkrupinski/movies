package services.cinemas.pl

import models._
import org.jsoup.Jsoup
import play.api.libs.json.{JsValue, Json}
import services.cinemas.common.{CinemaScraper, ListingPages, ScraperParse, SlotsToMovies}
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, YearMonth, ZoneId}
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
  today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import IksorisCalendarClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(page.origin.value)
  override def sourceUrl: Option[String] = Some(page.url)

  // A calendar failing propagates — without it no day is known, so a swallowed
  // failure would read as a white "0 films". A day failing drops only that day,
  // unless every one did.
  def fetch(): Seq[CinemaMovie] = {
    val first = YearMonth.from(today)
    val days = Iterator.iterate(first)(_.plusMonths(1))
      .take(RunawayMonths)
      .map(month => month -> scheduledDays(http.get(calendarUrl(page, month))))
      .takeWhile { case (month, days) => month == first || days.nonEmpty }
      .flatMap(_._2)
      .filterNot(_.isBefore(today))
      .toSeq
    val attempts = days.map(day => Try(http.get(dayUrl(page, day))))
    ListingPages.requireAnyReached(attempts)
    parse(attempts.flatMap(_.toOption), cinema)
  }
}

object IksorisCalendarClient {

  /** Only a guard against a calendar that marks every month (a markup change
   *  read wrong) looping forever — two years is past any venue's real horizon. */
  private val RunawayMonths = 24

  def calendarUrl(page: IksorisBookingPage, month: YearMonth): String =
    s"${page.origin.value}/index/ajax.html?ajax=pobierzKalendarz&idg=${page.eventGroup}&year=${month.getYear}&month=${month.getMonthValue}"

  def dayUrl(page: IksorisBookingPage, day: LocalDate): String =
    s"${page.origin.value}/index/ajax.html?ajax=pobierzTerminy&idg=${page.eventGroup}&selectedDate=$day"

  // "2026-09-27 (niedziela) 14:30" — the weekday in between is ignored.
  private val WhenPat = """^(\d{4}-\d{2}-\d{2})\b.*?(\d{1,2}:\d{2})\s*$""".r
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

  /** The days a month's calendar marks as having screenings. */
  private[pl] def scheduledDays(calendarJson: String): Seq[LocalDate] =
    (Json.parse(calendarJson) \ "kalendarzHtml").asOpt[String].toSeq.flatMap { html =>
      Jsoup.parse(html).select("button.kalendarz-terminow-dzien.dzien-z-terminami[data-day]").asScala.toSeq
        .flatMap(button => Try(LocalDate.parse(button.attr("data-day"))).toOption)
    }.distinct

  def parse(dayJsons: Seq[String], cinema: Cinema): Seq[CinemaMovie] = {
    val slots = dayJsons.flatMap(body => (Json.parse(body) \ "data").asOpt[Seq[JsValue]].getOrElse(Seq.empty)).flatMap(showing)
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
