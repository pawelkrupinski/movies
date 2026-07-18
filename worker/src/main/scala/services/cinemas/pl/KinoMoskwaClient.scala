package services.cinemas.pl

import org.jsoup.nodes.{Document, Element}
import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper, SlotsToMovies}

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Moskwa (Kielce) — a WordPress site using the My Calendar plugin. The
 * venue publishes a day-view calendar at
 * `http://kinomoskwa.pl/kalendarz/?yr=YYYY&month=MM&dy=D&time=day&mcat=all`
 * (day of month is unpadded).
 *
 * Each day page contains two useful structures:
 *   - `table.my-calendar-table` mini-calendar: `<td id='mini-YYYY-MM-DD'
 *     class='… has-events …'>` marks which days have screenings. We extract
 *     the event-days within the current calendar month from this block.
 *   - `div#mc-day`: the day's film list. Each `div.list-event.mc_repertuar`
 *     child carries:
 *       - `h3.event-title` — title with a leading `N. ` ordinal prefix that is
 *         stripped before storage.
 *       - `div.shortdesc.description` — a `<ul>` with screening times as
 *         `<a href='https://…bilety24…'>HH:MM/S/</a>` or `/D/` links (where
 *         `/S/` = sala mała Studyjna, `/D/` = sala duża Moskwa). Events that
 *         carry only a plain time range (e.g. "Pokazy grupowe": "8:00-14:30")
 *         with no room-code links are non-public group screenings and are
 *         silently dropped.
 *
 * The scraper strategy:
 *   1. Fetch today's day-view page to seed the mini-calendar and parse today's
 *      events.
 *   2. Collect all event-days visible in the mini-calendar that are not in the
 *      past relative to `today`.
 *   3. Fetch each future event-day's page in parallel; parse events tolerantly
 *      (a failed fetch just contributes no screenings).
 *   4. Aggregate all showtimes by title across days.
 */
class KinoMoskwaClient(
  http:             HttpFetch,
  override val cinema: Cinema = KinoMoskwa,
  today:            LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends ChunkedCinemaScraper {

  import KinoMoskwaClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  /** The mini-calendar on today's page lists the event-days (today inclusive) =
   *  one chunk per day. Fetching today's page here is the nav fetch. */
  def planChunks(): Seq[String] =
    miniCalendarEventDays(Jsoup.parse(http.get(dayUrl(today))), today).map(_.toString)

  /** One day's page → that day's films (folded by title). A throw reschedules
   *  just this day's chunk. */
  def fetchChunk(date: String): Seq[CinemaMovie] = {
    val d = LocalDate.parse(date)
    moviesFrom(parseDayDocument(Jsoup.parse(http.get(dayUrl(d))), d))
  }

  private def moviesFrom(slots: Seq[RawSlot]): Seq[CinemaMovie] =
    SlotsToMovies.fold(
      slots,
      titleOf    = _.title,
      showtimeOf = s => Showtime(s.dateTime, None, room = Some(s.room)),
      distinctBy = s => (s.dateTime, s.room)
    ) { (title, _, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
}

object KinoMoskwaClient {

  val BaseUrl = "http://kinomoskwa.pl"

  // `getDayOfMonth` returns an unpadded int (7, not "07") — consistent with
  // the mini-calendar anchor format and therefore with the fixture filename.
  def dayUrl(date: LocalDate): String =
    s"$BaseUrl/kalendarz/?yr=${date.getYear}&month=${date.getMonthValue}&dy=${date.getDayOfMonth}&time=day&mcat=all"

  // Room suffix: /S/ = sala Studyjna (small), /D/ = sala Moskwa (large).
  private val TimeRoomPat = """(\d{1,2}:\d{2})/([DS])/""".r
  // Strip leading ordinal "N. " added by My Calendar.
  private val OrdinalPat  = """^\d+\.\s+""".r

  private[cinemas] case class RawSlot(title: String, dateTime: LocalDateTime, room: String)

  /** Dates that carry events in the mini-calendar, on or after `today`. */
  private[cinemas] def miniCalendarEventDays(document: Document, today: LocalDate): Seq[LocalDate] = {
    document.select("td[id^='mini-'][class*='has-events']").asScala.toSeq.flatMap { td =>
      val id = td.attr("id").stripPrefix("mini-")
      Try(LocalDate.parse(id)).toOption.filter(d => !d.isBefore(today))
    }.sorted
  }

  /** Parse all public film screenings from a single day-view page. */
  private[cinemas] def parseDayDocument(document: Document, date: LocalDate): Seq[RawSlot] =
    document.select("div#mc-day div.list-event.mc_repertuar").asScala.toSeq.flatMap { eventDiv =>
      parseEvent(eventDiv, date)
    }

  private def parseEvent(eventDiv: Element, date: LocalDate): Seq[RawSlot] = {
    val parsed = for {
      rawTitle  <- Option(eventDiv.selectFirst("h3.event-title")).map(_.text.trim).filter(_.nonEmpty)
      shortdesc <- Option(eventDiv.selectFirst("div.shortdesc.description"))
    } yield {
      val title = OrdinalPat.replaceFirstIn(rawTitle, "")
      TimeRoomPat.findAllMatchIn(shortdesc.text).flatMap { m =>
        ScraperParse.parseHHmm(m.group(1)).map { time =>
          val room = if (m.group(2) == "S") "Sala Studyjna" else "Sala Moskwa"
          RawSlot(title, LocalDateTime.of(date, time), room)
        }
      }.toSeq
    }
    parsed.getOrElse(Seq.empty)
  }
}
