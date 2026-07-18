package services.cinemas.pl

import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.LocalDateTime
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kinoteatr Zbyszek (Dzierżoniowski Ośrodek Kultury, Dzierżoniów). Its
 * repertoire at `dok.pl/kino` is a server-rendered Drupal calendar block
 * (`#block-calendarblock`): one `div.day` per calendar day carrying the date
 * on its cell anchor (`a[data-date]`, e.g. "26 Czerwiec 2026" — a Polish
 * *nominative* month name, with the year, so no inference), and a
 * `ul.events-bubble` of `li.event` rows, one per screening. Each event row is
 * self-contained in `data-*`:
 *   - `data-title`      → film title (ALL-CAPS — sentence-cased here)
 *   - `data-hours`      → the single screening time ("16:00")
 *   - `data-ticket`     → biletyna booking link (tickets are still sold there;
 *                          only the SCHEDULE moved off biletyna onto dok.pl)
 *   - `data-link`       → the dok.pl film page, reused as the film URL
 *   - `data-poster`     → poster image
 *   - `data-soundtrack` → "dubbing"/"napisy"/"oryginalny", surfaced as the
 *                          screening's `format` badge (DUB/NAP)
 *
 * There is no JSON-LD. Previously scraped via biletyna.pl, which no longer
 * carries the film repertoire for this venue.
 *
 * The calendar also lists a "KINO NIECZYNNE" (cinema-closed) placeholder on
 * dark days — a `data-hours="00:00"` row with no ticket/genre — which is not a
 * film and is dropped here. No live stage/music events appear in the feed (all
 * `data-genre`s are film genres), so `OnlyMovieEventsFilter` isn't needed.
 */
class KinoZbyszekClient(http: HttpFetch, override val cinema: Cinema = KinoZbyszek)
    extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoZbyszekClient.RepertoireUrl)
  override def sourceUrl: Option[String] = Some(KinoZbyszekClient.RepertoireUrl)

  def fetch(): Seq[CinemaMovie] =
    KinoZbyszekClient.parse(http.get(KinoZbyszekClient.RepertoireUrl), cinema)
}

object KinoZbyszekClient {

  val BaseUrl       = "https://dok.pl"
  val RepertoireUrl = s"$BaseUrl/kino"

  // The day-cell anchor carries the date as "26 Czerwiec 2026" — day, Polish
  // NOMINATIVE month name (capitalised), year.
  private val DatePat = """(\d{1,2})\s+(\p{L}+)\s+(\d{4})""".r

  private case class RawSlot(
    title:     String,
    dateTime:  LocalDateTime,
    booking:   Option[String],
    filmUrl:   Option[String],
    poster:    Option[String],
    formats:   List[String]
  )

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)

    val slots = document.select("#block-calendarblock div.day").asScala.toSeq.flatMap { day =>
      parseDate(day).toSeq.flatMap(date => day.select("li.event").asScala.toSeq.flatMap(parseEvent(_, date)))
    }

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking, format = s.formats)) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = group.flatMap(_.poster).headOption,
        filmUrl   = group.flatMap(_.filmUrl).headOption,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }

  // The day's (year, month, day) from the cell anchor's `data-date`.
  private def parseDate(day: Element): Option[(Int, Int, Int)] =
    for {
      raw   <- Option(day.selectFirst("a[data-date]")).map(_.attr("data-date"))
      d     <- DatePat.findFirstMatchIn(raw)
      month <- ScraperParse.PolishMonthsAnyCase.get(d.group(2).toLowerCase)
    } yield (d.group(3).toInt, month, d.group(1).toInt)

  private def parseEvent(ev: Element, date: (Int, Int, Int)): Option[RawSlot] = {
    val (year, month, dom) = date
    val rawTitle = ev.attr("data-title").trim
    // "KINO NIECZYNNE" is the cinema-closed placeholder (always `data-hours=00:00`,
    // no ticket); it isn't a film, so drop it.
    if (rawTitle.isEmpty || rawTitle.equalsIgnoreCase("KINO NIECZYNNE")) None
    else
      for {
        time <- ScraperParse.parseHHmm(ev.attr("data-hours"))
        dt   <- Try(LocalDateTime.of(year, month, dom, time.getHour, time.getMinute)).toOption
      } yield RawSlot(
        title    = ScraperParse.sentenceCase(rawTitle),
        dateTime = dt,
        booking  = Some(ev.attr("data-ticket").trim).filter(_.nonEmpty),
        filmUrl  = Some(ev.attr("abs:data-link")).filter(_.nonEmpty),
        poster   = Some(ev.attr("data-poster").trim).filter(_.nonEmpty),
        formats  = ScraperParse.FormatToken.get(ev.attr("data-soundtrack").trim.toLowerCase).toList
      )
  }
}
