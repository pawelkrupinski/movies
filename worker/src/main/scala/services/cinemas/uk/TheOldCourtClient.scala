package services.cinemas.uk

import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import models._
import tools.HttpFetch
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.{LocalDate, LocalDateTime, LocalTime}
import java.time.format.TextStyle
import java.util.Locale
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * The Old Court, Windsor — an arts centre whose cinema strand we could not read.
 *
 * It was wired to the flicks.co.uk slug `the-screen-cinema-windsor`, which is a
 * DIFFERENT Windsor venue: The Old Court is absent from Flicks' own
 * `sitemap-cinemas.xml` (848 entries; no `oldcourt` slug exists), so the venue
 * scraped to zero forever — a white uptime bar caused entirely by our own
 * mapping. Its own site carries the programme, so we read that instead.
 *
 * `oldcourt.org.uk/events` is one flat, chronological list mixing the cinema in
 * with quiz nights, DJ sets, ballet screenings and workshops. The page is
 * server-rendered but carries NO class names (it is styled with CSS `@scope`),
 * so every hook here is structural:
 *
 * {{{
 * <!-- events -->
 * <div>                                          <- one event
 *   <a href="/event/11283"><img src="…"></a>      <- id + poster
 *   <div>
 *     <div>Whilst she was gone (Independent film)</div>
 *     <hr>
 *     <!-- bookings --><span>Fri 7th Aug 20:30-21:15
 *       <a href="https://tickets.oldcourt.org/sales/the-old-court-cinema/…">(tickets)</a>
 *     </span>
 *   </div>
 * </div>
 * }}}
 *
 * **What makes an event a film** is the booking link's path: the box office files
 * cinema tickets under `/sales/the-old-court-cinema/`, everything else under
 * `/sales/events/` or its own slug. That single discriminator separates the 14
 * film entries from the 28 non-film ones cleanly, and it beats reading the title —
 * "Rocky Horror Night" and "Disclosure Day" are films here, while "Alice In
 * Wonderland - The Ballet" is not.
 *
 * Dates are plain prose with NO year ("Fri 7th Aug 20:30-21:30"), so the year is
 * inferred from `today`: the listing runs forward from today, so a month/day that
 * has already passed belongs to next year.
 */
class TheOldCourtClient(
  http:  HttpFetch,
  override val cinema: Cinema,
  today: LocalDate = LocalDate.now(java.time.ZoneId.of("Europe/London"))
) extends CinemaScraper {

  import TheOldCourtClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(EventsUrl)

  def fetch(): Seq[CinemaMovie] = {
    val slots = Jsoup.parse(http.get(EventsUrl), BaseUrl)
      .select(s"a[href^=/event/]").asScala.toSeq
      .flatMap(anchor => Option(anchor.parent).toSeq.flatMap(parseEvent(_, today)))

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.bookingUrl)) { (title, group, showtimes) =>
      val first = group.minBy(_.dateTime)
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = first.posterUrl,
        filmUrl   = Some(first.eventUrl),
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }
}

object TheOldCourtClient {

  val BaseUrl   = "https://oldcourt.org.uk"
  val EventsUrl = s"$BaseUrl/events"

  /** The box office files cinema tickets under this path and nothing else does. */
  private val CinemaBookingPath = "/sales/the-old-court-cinema/"

  /** "Fri 7th Aug 20:30-21:15" — the end time is ignored; the start is the showtime. */
  private val WhenPat = """\b(\d{1,2})(?:st|nd|rd|th)\s+([A-Za-z]{3,})\s+(\d{1,2}):(\d{2})""".r

  /** Programme-strand labels the venue appends to a real film title. "The Old
   *  Courters" is its seniors' club matinee, so "Tuner (The Old Courters)" and
   *  "Tuner" are one film on two days — stripping is what merges them into a
   *  single card instead of two near-duplicate rows. */
  private val StrandSuffixPat = """\s*\((?:Independent film|The Old Courters)\)\s*$""".r

  private val MonthsByAbbreviation: Map[String, Int] =
    (1 to 12).map { m =>
      java.time.Month.of(m).getDisplayName(TextStyle.SHORT, Locale.UK).toLowerCase.take(3) -> m
    }.toMap

  private case class RawSlot(
    title:      String,
    dateTime:   LocalDateTime,
    bookingUrl: Option[String],
    eventUrl:   String,
    posterUrl:  Option[String]
  )

  /** One event block → one slot per cinema-booking span. Empty for a non-film
   *  event (no booking under the cinema path) or an unparseable date. */
  private def parseEvent(block: Element, today: LocalDate): Seq[RawSlot] = {
    val bookings = block.select("span").asScala.toSeq
      .flatMap { span =>
        Option(span.selectFirst(s"a[href*=$CinemaBookingPath]"))
          .flatMap(link => parseWhen(span.text, today).map(_ -> link.attr("abs:href")))
      }
    if (bookings.isEmpty) Seq.empty
    else {
      // The title is the first child div of the block's inner wrapper, reached by
      // walking children rather than by a CSS query: jsoup's `select` matches the
      // ROOT element too, so `block.selectFirst("div")` returns the block itself and
      // any descendant query lands on the wrapper — whose text is the title with
      // every booking line glued onto the end.
      val title = block.children.asScala.find(_.tagName == "div")
        .flatMap(_.children.asScala.find(_.tagName == "div"))
        .map(_.text.trim).map(cleanTitle).filter(_.nonEmpty)
      val eventUrl  = Option(block.selectFirst("a[href^=/event/]")).map(_.attr("abs:href"))
      val posterUrl = Option(block.selectFirst("img")).map(_.attr("abs:src")).filter(_.nonEmpty)
      for {
        t <- title.toSeq
        u <- eventUrl.toSeq
        (when, booking) <- bookings
      } yield RawSlot(t, when, Some(booking), u, posterUrl)
    }
  }

  /** "Fri 7th Aug 20:30-21:15" → 2026-08-07T20:30, taking the year from `today`. */
  private def parseWhen(text: String, today: LocalDate): Option[LocalDateTime] =
    WhenPat.findFirstMatchIn(text).flatMap { m =>
      for {
        month <- MonthsByAbbreviation.get(m.group(2).toLowerCase.take(3))
        time  <- Try(LocalTime.of(m.group(3).toInt, m.group(4).toInt)).toOption
        date  <- Try(LocalDate.of(today.getYear, month, m.group(1).toInt)).toOption
      } yield {
        // The listing only ever runs forward, so a date already behind us is next
        // year's — the December→January rollover, without a year on the page.
        val resolved = if (date.isBefore(today.minusDays(1))) date.plusYears(1) else date
        LocalDateTime.of(resolved, time)
      }
    }

  private[uk] def cleanTitle(raw: String): String = StrandSuffixPat.replaceAllIn(raw, "").trim
}
