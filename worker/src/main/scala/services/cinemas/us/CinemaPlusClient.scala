package services.cinemas.us

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import services.cinemas.common.{CinemaScraper, ScraperParse, SlotsToMovies}
import services.cinemas.pl.OnlyMovieEventsFilter
import tools.{HttpFetch, HttpRead, ReadOutcome}

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.jdk.CollectionConverters._

/**
 * A venue whose site runs on CinemaPlus (cinemaplus.com), the ticketing platform small US
 * circuits sell through — Barry Cinemas' three Wyoming houses among them. Flicks dropped
 * one of them (Acme Theatre, Riverton: its flicks.us page has answered 404 since
 * 2026-08-31 while barrycinemas.com sold its October screenings), so the venue's own site
 * is the only source.
 *
 * The venue page (`https://barrycinemas.com/acme-theater-riverton`) is server-rendered: a
 * calendar strip with one `div.cin-calendar-item#js-YYYY-MM-DD` per day the venue has
 * published, the selected one marked `cin-calendar-item-link-selected` and every other
 * linking `?date=YYYY-MM-DD`, then one card per film for the selected day:
 *
 * {{{
 * <div class="flex flex-col rounded-xl …">
 *   <img src="https://images.cinemaplus.com/ticketing/posters/….webp">
 *   <h3><a href="…/acme-theater-riverton/movie/27321">Angel and the Badman</a></h3>
 *   <div>Cast: Zachary Levi, …</div><div>Director: Julio Quintana</div>
 *   <div class="mt-6 border-t">
 *     <div class="mt-3 mb-3 …">General Admission</div>
 *     <div class="-mt-4 …"><a class="cin-showtimes-button …" href="…/tickets/<uuid>">6:00p</a></div>
 * }}}
 *
 * So a scrape is the venue page plus one `?date=` page per other day it advertises, from
 * `today` on — the whole published horizon, never capped here. A day's times are its own
 * wall clock ("6:00p"); the label over a group of buttons is a seating class ("General
 * Admission", "Reserved Seating") or a presentation ("3D"), kept as the showtime's format
 * unless it is a seating class.
 *
 * @param siteUrl   the circuit's origin, no trailing slash (`https://barrycinemas.com`).
 * @param venuePath the venue's path on it (`/acme-theater-riverton`).
 */
class CinemaPlusClient(
  http:      HttpFetch,
  siteUrl:   String,
  venuePath: String,
  override val cinema: Cinema,
  today:     => LocalDate
) extends CinemaScraper with OnlyMovieEventsFilter {

  import CinemaPlusClient._

  private val pageUrl = s"$siteUrl$venuePath"

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(siteUrl)
  override def sourceUrl: Option[String] = Some(pageUrl)

  protected def fetchUnfiltered(): Seq[CinemaMovie] = {
    val from  = today
    val venue = read(pageUrl)
    val shown = selectedDay(venue)
    val days  = advertisedDays(venue).filterNot(_.isBefore(from))
    val pages = days.map(day => day -> (if (shown.contains(day)) venue else read(dayUrl(pageUrl, day))))
    toMovies(pages.flatMap { case (day, page) => slots(page, day, cinema) }, cinema)
  }

  private def read(url: String): Document =
    HttpRead.html(http, url, Marker)(body => ReadOutcome.Answered(Jsoup.parse(body, url))).required
}

object CinemaPlusClient {

  /** Every CinemaPlus page loads the platform's own bundle (`/build/assets/cinemaplus-….js`);
   *  a page without it is not a CinemaPlus venue page (or the platform changed). */
  val Marker: HttpRead.PageMarker = HttpRead.PageMarker("cinemaplus")

  /** The seating classes a house sells every showing under ("General Admission" at the
   *  Acme, "Reserved Seating" at the Gem), which say nothing about how the film is shown. */
  val SeatingClasses: Set[String] = Set("general admission", "reserved seating")

  private def formatOf(label: Option[String]): List[String] =
    label.filterNot(l => SeatingClasses.contains(l.toLowerCase(java.util.Locale.ROOT))).toList

  def dayUrl(pageUrl: String, day: LocalDate): String = s"$pageUrl?date=$day"

  private val DayId = """js-(\d{4}-\d{2}-\d{2})""".r
  private val ShowtimeButton = raw"""(?i)^${ScraperParse.ClockParts}\s*([ap])\.?m?\.?$$""".r

  final case class Slot(title: String, card: Element, showtime: Showtime)

  /** The days the venue's calendar strip advertises, in its order. */
  def advertisedDays(page: Document): Seq[LocalDate] =
    page.select("div.cin-calendar-item").asScala.toSeq.flatMap(e => dayOf(e.id()))

  /** The day the page's film cards are for: the calendar item marked selected. */
  def selectedDay(page: Document): Option[LocalDate] =
    page.select("div.cin-calendar-item").asScala
      .find(e => !e.select(".cin-calendar-item-link-selected").isEmpty)
      .flatMap(e => dayOf(e.id()))

  private def dayOf(id: String): Option[LocalDate] = id match {
    case DayId(d) => Some(LocalDate.parse(d))
    case _        => None
  }

  /** "6:00p" / "11:30a" → the time of day; anything else is not a showtime button. */
  def timeOf(label: String): Option[LocalTime] =
    ShowtimeButton.findFirstMatchIn(label.trim).flatMap(ScraperParse.meridiemClockAt(_, 1, 3))

  /** One day page's film cards → a slot per showtime button, on `day`. */
  def slots(page: Document, day: LocalDate, cinema: Cinema): Seq[Slot] =
    page.select("div.cin-showtimes-grid > div").asScala.toSeq.flatMap { card =>
      val title = Option(card.selectFirst("h3 a")).map(_.text.trim).filter(_.nonEmpty)
      title.toSeq.flatMap { t =>
        // The groups alternate: a label, then the buttons it heads. Each button takes the
        // label above it.
        val (_, found) = card.select("div.border-t > div").asScala.toSeq
          .foldLeft((Option.empty[String], Vector.empty[Slot])) { case ((label, acc), group) =>
            val buttons = group.select("a.cin-showtimes-button").asScala.toSeq
            if (buttons.isEmpty) (Some(group.text.trim).filter(_.nonEmpty), acc)
            else (label, acc ++ buttons.flatMap { b =>
              timeOf(b.text).map { time =>
                Slot(t, card, Showtime(
                  dateTime   = LocalDateTime.of(day, time),
                  bookingUrl = Option(b.absUrl("href")).filter(_.nonEmpty),
                  room       = None,
                  format     = formatOf(label)
                ))
              }
            })
          }
        found
      }
    }

  def toMovies(slots: Seq[Slot], cinema: Cinema): Seq[CinemaMovie] =
    SlotsToMovies.fold(slots, _.title, _.showtime) { (title, group, showtimes) =>
      val card = group.head.card
      CinemaMovie(
        movie     = Movie(title = title),
        cinema    = cinema,
        posterUrl = Option(card.selectFirst("img[src]")).map(_.absUrl("src")).filter(_.nonEmpty),
        filmUrl   = Option(card.selectFirst("h3 a")).map(_.absUrl("href")).filter(_.nonEmpty),
        synopsis  = Option(card.selectFirst("div.text-sm > div > div")).map(_.text.trim).filter(_.nonEmpty),
        cast      = credited(card, "Cast:"),
        director  = credited(card, "Director:"),
        showtimes = showtimes,
        ageRating = Option(card.selectFirst("div.rounded-xs")).map(_.text.trim).filter(_.nonEmpty)
      )
    }

  /** The names on the card's "Cast: a, b" / "Director: c" line. */
  private def credited(card: Element, prefix: String): Seq[String] =
    card.select("div").asScala.map(_.ownText.trim).find(_.startsWith(prefix)).toSeq
      .flatMap(line => ScraperParse.commaItems(line.stripPrefix(prefix)))
}
