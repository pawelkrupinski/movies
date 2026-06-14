package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.{LocalDate, LocalDateTime, ZoneId}
import java.time.format.DateTimeFormatter
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * MCSW Elektrownia cinema (Radom) — the film screen of the Mazowieckie
 * Centrum Sztuki Współczesnej "Elektrownia".  The site uses an ASP.NET MSI
 * ticketing system that renders its schedule as a static HTML page per day at:
 *
 *   https://kino.mcswelektrownia.pl/MSI/mvc/pl?sort=Date&date=YYYY-MM-DD&datestart=0
 *
 * Seven days (today + 6) are scraped in parallel and merged.  Each day page
 * lists all currently-running films as `div.js-event-details-filter.movies-movie__single`
 * blocks.  Within each block:
 *
 *   - `h2.movies-movie__single__title` — a composite string:
 *     "CLEAN TITLE, Country, genres, rating   INTERNAL-CODE".
 *     The clean film title is the segment before the FIRST comma (the rest
 *     carries locale / genre metadata that we discard).
 *   - `li[event-filter]` / `a[href^="/MSI/Default.aspx?event_id="]` — each
 *     list item is ONE screening occurrence; the anchor text is its time
 *     ("HH:MM") and the href is the per-occurrence booking URL.
 *     The list is rendered twice (desktop + mobile), so deduplicate by
 *     (time, event_id) before emitting showtimes.
 *   - `img[src^="/MSI/ImageData.ashx"]` — poster thumbnail.
 *
 * Films that appear on multiple days are aggregated by their normalised title
 * (trimmed, lowercased, before the first comma) so that the same film shown
 * on Tuesday and Thursday appears as one `CinemaMovie` with multiple
 * showtimes.
 */
class McswElektrowniaCinemaClient(
  http:              HttpFetch,
  override val cinema: Cinema = McswElektrowniaCinema,
  today:             LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import McswElektrowniaCinemaClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    // Scrape today and the next 6 days in parallel.
    val dates = (0 until DaysAhead).map(today.plusDays(_))

    val pageHtml = ParallelDetailFetch.keyed("mcsw-days", dates.map(_.toString), 1.minute) { dateStr =>
      dayUrl(LocalDate.parse(dateStr))
    } { url =>
      Try(http.get(url)).toOption
    }

    val slots: Seq[RawSlot] = dates.flatMap { date =>
      pageHtml.getOrElse(date.toString, None) match {
        case None       => Seq.empty
        case Some(html) => parseDayPage(html, date)
      }
    }

    // Group by normalised title and merge showtimes across days.
    val byTitle = slots.groupBy(_.normTitle)
    byTitle.toSeq.flatMap { case (_, group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, Some(BookingBase + s.eventPath)))
        .distinctBy(s => (s.dateTime, s.bookingUrl))
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else {
        val head = group.head
        Some(CinemaMovie(
          movie     = Movie(head.displayTitle),
          cinema    = cinema,
          posterUrl = head.posterUrl,
          filmUrl   = None,
          synopsis  = None,
          cast      = Seq.empty,
          director  = Seq.empty,
          showtimes = showtimes
        ))
      }
    }.sortBy(_.movie.title)
  }
}

object McswElektrowniaCinemaClient {

  val BaseUrl    = "https://kino.mcswelektrownia.pl"
  val BookingBase = BaseUrl

  private val DaysAhead = 7

  private val DateFmt = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  def dayUrl(date: LocalDate): String =
    s"$BaseUrl/MSI/mvc/pl?sort=Date&date=${date.format(DateFmt)}&datestart=0"

  /** A raw (date + time) screening extracted from one day page. */
  private[cinemas] case class RawSlot(
    displayTitle: String,  // cleaned title for user display
    normTitle:    String,  // lowercased key for cross-day grouping
    posterUrl:    Option[String],
    dateTime:     LocalDateTime,
    eventPath:    String   // e.g. "/MSI/Default.aspx?event_id=14125&typetran=0&..."
  )

  /** Extract the user-facing film title: the first segment before the first
   *  ", " separator.  The MSI system embeds country / genre / rating metadata
   *  as comma-delimited tail segments, and sometimes appends an internal
   *  catalogue code ("2026D2D2251"). */
  private[cinemas] def extractTitle(raw: String): String = {
    val trimmed = raw.trim
    val commaIndex = trimmed.indexOf(", ")
    val base = if (commaIndex > 0) trimmed.substring(0, commaIndex)
               else {
                 // No ", " separator — try a bare comma
                 val bare = trimmed.indexOf(',')
                 if (bare > 0) trimmed.substring(0, bare) else trimmed
               }
    base.trim
  }

  private[cinemas] def parseDayPage(html: String, date: LocalDate): Seq[RawSlot] = {
    val doc = Jsoup.parse(html)
    doc.select("div.js-event-details-filter.movies-movie__single").asScala.toSeq.flatMap { block =>
      val rawTitle = Option(block.selectFirst("h2.movies-movie__single__title"))
        .map(_.text.trim).getOrElse("")
      if (rawTitle.isEmpty) Seq.empty
      else {
        val displayTitle = extractTitle(rawTitle)
        val normTitle    = displayTitle.trim.toLowerCase

        val posterUrl = Option(block.selectFirst("img[src]"))
          .map(_.attr("src").trim)
          .filter(_.startsWith("/MSI/ImageData.ashx"))
          .map(BaseUrl + _)

        // Each `li[event-filter]` is one screening slot; the anchor carries the
        // time as its visible text and the booking path as href.  The list is
        // rendered twice (desktop + mobile), so deduplicate by (eventPath, timeStr).
        val seenKeys = collection.mutable.Set.empty[(String, String)]
        block.select("li[event-filter] a[href^=\"/MSI/Default.aspx?event_id=\"]").asScala.toSeq.flatMap { anchor =>
          val timeStr  = anchor.text.trim
          val timeOpt  = ScraperParse.parseHHmm(timeStr)
          val path     = anchor.attr("href").trim
          val key      = (path, timeStr)
          if (timeOpt.isEmpty || !seenKeys.add(key)) Nil
          else {
            val dateTime = LocalDateTime.of(date, timeOpt.get)
            Seq(RawSlot(displayTitle, normTitle, posterUrl, dateTime, path))
          }
        }
      }
    }
  }
}
