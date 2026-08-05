package services.cinemas.pl

import tools.HttpFetch
import models._
import org.jsoup.Jsoup
import services.cinemas.common.{CinemaScraper, ScrapeHorizon, SlotsToMovies}

import java.time.{LocalDate, LocalDateTime, ZoneId}
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * MCSW Elektrownia cinema (Radom) — the film screen of the Mazowieckie
 * Centrum Sztuki Współczesnej "Elektrownia".  The site uses an ASP.NET MSI
 * ticketing system that renders its schedule as a static HTML page per day at:
 *
 *   https://kino.mcswelektrownia.pl/MSI/mvc/pl?sort=Date&date=YYYY-MM-DD&datestart=0
 *
 * The day pages are walked forward for as long as the programme lasts and
 * merged (see `fetch`).  Each day page lists all currently-running films as
 * `div.js-event-details-filter.movies-movie__single` blocks.  Within each block:
 *
 *   - `.movies-movie__single__title` (an `h2` or, since 2026-06-16, an `h3`) —
 *     a composite string:
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
 *
 * This is an MSI portal, so [[MsiClient]]'s month route serves it too — but not
 * as well: measured 2026-08-05, `?sort=Name&date=2026-08` listed only 05–11
 * while the day route had four films on both the 13th and the 16th.  The
 * per-day walk stays.
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
    // Follow the programme rather than assume a week of it: on 2026-08-05 the
    // day route had films on the 13th and the 16th, both past the today+6 window
    // this used to ask for, so a fortnight of the schedule was invisible.
    // See [[ScrapeHorizon.liveDays]] — same walk as the other per-day clients.
    val byDate = scala.collection.mutable.LinkedHashMap.empty[LocalDate, Seq[RawSlot]]
    ScrapeHorizon.liveDays(today) { date =>
      byDate.getOrElseUpdate(date,
        Try(http.get(dayUrl(date))).toOption.toSeq.flatMap(parseDayPage(_, date))).nonEmpty
    }
    val slots: Seq[RawSlot] = byDate.values.toSeq.flatten

    // Group by normalised title and merge showtimes across days.
    SlotsToMovies.fold(slots, _.normTitle, s => Showtime(s.dateTime, Some(BookingBase + s.eventPath))) {
      (_, group, showtimes) =>
        val head = group.head
        CinemaMovie(
          movie     = Movie(head.displayTitle),
          cinema    = cinema,
          posterUrl = head.posterUrl,
          filmUrl   = None,
          synopsis  = None,
          cast      = Seq.empty,
          director  = Seq.empty,
          showtimes = showtimes
        )
    }
  }
}

object McswElektrowniaCinemaClient {

  val BaseUrl    = "https://kino.mcswelektrownia.pl"
  val BookingBase = BaseUrl

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
    val document = Jsoup.parse(html)
    document.select("div.js-event-details-filter.movies-movie__single").asScala.toSeq.flatMap { block =>
      // The title sits on `.movies-movie__single__title`; the site has rendered
      // this as both `h2` (2026-06 capture) and `h3` (2026-06-16 onward), so
      // match on the class alone rather than pinning the heading level.
      val rawTitle = Option(block.selectFirst(".movies-movie__single__title"))
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
