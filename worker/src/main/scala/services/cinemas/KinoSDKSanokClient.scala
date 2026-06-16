package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Sanocki Dom Kultury (Kino SDK, Sanok) — ticketed via the MSI / Bilety24
 * culture-house portal at `bilety.sdksanok.pl`. This is NOT the standard MSI
 * `movies-movie__single` template the [[MsiClient]] parses; it is the generic
 * "lista wydarzeń" (events list) view, which mixes FILMS with non-film events
 * (concerts, opera/ballet broadcasts).
 *
 * The listing at `/MSI/mvc/pl?sort=Flow&date=YYYY-MM-DD&datestart=0` is scoped
 * to ONE calendar day — the `&date=` query param picks the day, and the events
 * shown carry no inline human date string. The day each screening belongs to
 * therefore comes from the URL, not the markup. To cover the upcoming weeks we:
 *   1. fetch a single entry listing (today) and read its date picker
 *      (`ul.options-list time[datetime]`), which enumerates exactly the dates
 *      that have events;
 *   2. fetch each of those dates and union the results.
 *
 * Per day the page renders each event TWICE — a desktop card
 * (`.list-group-item.visible-md.visible-lg`) and a mobile card
 * (`.visible-xs.visible-sm`) — so we read only the desktop cards to avoid
 * double-counting. Within a card:
 *   - `div.event-title` → event name;
 *   - `ul.repo-event-dates-group li a.badge-circle` → one anchor per screening,
 *     its text the `HH:mm` time and its `href` the booking link;
 *   - `a[href*=/details/]` → the event detail page (used as `filmUrl`).
 *
 * FILM-vs-EVENT LIMITATION: the portal exposes NO reliable signal to tell a
 * film from a concert/broadcast — the markup, CSS classes and detail pages are
 * structurally identical for "Gwiezdne wojny: Mandalorian i Grogu" (a film) and
 * "90. urodziny Pavarottiego" (an opera broadcast). Rather than risk dropping
 * real films we capture ALL listed events; the central enrichment (TMDB /
 * Filmweb matching) is the backstop that fails to resolve a non-film and lets
 * it fall away. Previously scraped from Filmweb (id 2118).
 */
class KinoSDKSanokClient(
  http:   HttpFetch,
  override val cinema: Cinema = KinoSDK,
  today:  LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoSDKSanokClient.BaseUrl)
  override def sourceUrl: Option[String] = Some(KinoSDKSanokClient.listingUrl(today))

  def fetch(): Seq[CinemaMovie] = {
    val entry = Jsoup.parse(http.get(KinoSDKSanokClient.listingUrl(today)), KinoSDKSanokClient.BaseUrl)
    val dates = (today +: KinoSDKSanokClient.pickerDates(entry)).distinct.sorted

    val slots = dates.flatMap { date =>
      // The entry listing is already fetched; re-parse it rather than re-fetch.
      val doc = if (date == today) entry
                else Jsoup.parse(http.get(KinoSDKSanokClient.listingUrl(date)), KinoSDKSanokClient.BaseUrl)
      KinoSDKSanokClient.parseDay(doc, date)
    }

    KinoSDKSanokClient.group(slots, cinema)
  }
}

object KinoSDKSanokClient {

  val BaseUrl = "https://bilety.sdksanok.pl"

  def listingUrl(date: LocalDate): String =
    s"$BaseUrl/MSI/mvc/pl?sort=Flow&date=$date&datestart=0"

  private case class RawSlot(title: String, dateTime: LocalDateTime, booking: Option[String], filmUrl: Option[String])

  /** The dates the listing's date picker enumerates — exactly the days that
   *  have events. Each is an ISO `datetime` attr on a `time` inside the picker;
   *  the trailing "więcej dni / KALENDARZ" entry has an empty `datetime` we skip. */
  private def pickerDates(doc: Document): Seq[LocalDate] =
    doc.select("ul.options-list time[datetime]").asScala.toSeq
      .flatMap(el => Try(LocalDate.parse(el.attr("datetime"))).toOption)

  /** Parse one day's listing into raw slots, stamping `date` (the day the
   *  listing was scoped to) onto each screening time. Reads only the desktop
   *  cards so the duplicated mobile cards don't double the showtimes. */
  private def parseDay(doc: Document, date: LocalDate): Seq[RawSlot] =
    doc.select("div.list-group-item.visible-md.visible-lg").asScala.toSeq.flatMap(parseEvent(_, date))

  private def parseEvent(card: Element, date: LocalDate): Seq[RawSlot] = {
    val title   = Option(card.selectFirst("div.event-title")).map(_.text.trim).filter(_.nonEmpty)
    val filmUrl = Option(card.selectFirst("a[href*=/details/]")).map(_.attr("abs:href")).filter(_.nonEmpty)
    title.toSeq.flatMap { t =>
      card.select("ul.repo-event-dates-group li a.badge-circle").asScala.toSeq.flatMap { a =>
        ScraperParse.parseHHmm(a.text).map { time =>
          RawSlot(
            title    = t,
            dateTime = LocalDateTime.of(date, time),
            booking  = Option(a.attr("abs:href")).filter(_.nonEmpty),
            filmUrl  = filmUrl
          )
        }
      }
    }
  }

  /** Group raw slots by title into one `CinemaMovie` each, de-duplicating
   *  identical showtimes (the same event can appear on several picker days)
   *  and sorting both the showtimes and the films deterministically. */
  private def group(slots: Seq[RawSlot], cinema: Cinema): Seq[CinemaMovie] =
    slots.groupBy(_.title).toSeq.flatMap { case (title, group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, s.booking))
        .distinctBy(s => (s.dateTime, s.bookingUrl))
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = group.flatMap(_.filmUrl).headOption,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      ))
    }.sortBy(_.movie.title)
}
