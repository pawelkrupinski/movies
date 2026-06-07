package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Żak — the film screen of Klub Żak in Gdańsk (Trójmiasto). The site
 * (klubzak.com.pl, a bespoke CMS) server-renders a calendar at
 * `/pl/kalendarz?category=kino` filtered to film events; each event is a
 * `<a class="box" href="/pl/kalendarz/<slug>~zNNN">` card carrying the title,
 * a poster, and a `date-info` heading.
 *
 * Two screening shapes share that card:
 *   - Single screenings render a concrete `<span class="hour">HH:MM</span>`
 *     plus one `<span class="day">` and an abbreviated month (`Cze`, `Lip`).
 *     That's the whole schedule — no detail fetch needed.
 *   - Multi-day runs render "Od DD Mon Do DD Mon" with NO hour. The listing
 *     can't say when each day's screening starts (and a run can mix times,
 *     e.g. "5-7 czerwca o 18:00 / 8-9 czerwca o 20:00"), so for those we fetch
 *     the event's detail page and read its `<h3>Seans(e)</h3>` block — the
 *     authoritative schedule, in full genitive month names (`PolishMonths`).
 *
 * The calendar carries no year (only day + month); `today` anchors year
 * inference (roll a past month forward to next year). Production passes the
 * real Warsaw date; fixture-replay tests pin it to the capture date.
 */
class KinoZakClient(http: HttpFetch, override val cinema: Cinema,
                    today: LocalDate = LocalDate.now(java.time.ZoneId.of("Europe/Warsaw")))
    extends CinemaScraper {

  import KinoZakClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(ListingUrl)

  def fetch(): Seq[CinemaMovie] = {
    val cards = parseListing(http.get(ListingUrl))

    // Multi-day cards have no listing hour — fetch their detail pages and read
    // the Seans block for the real times. Single cards are already complete.
    val detailUrls = cards.filter(_.listingTimes.isEmpty).map(_.detailUrl).distinct
    val seansByUrl =
      if (detailUrls.isEmpty) Map.empty[String, Seq[Segment]]
      else ParallelDetailFetch.keyed("kino-zak-details", detailUrls, 1.minute)(identity) { url =>
        Try(http.get(url)).toOption.map(parseSeans).getOrElse(Seq.empty)
      }

    cards.flatMap { c =>
      val showtimes =
        if (c.listingTimes.nonEmpty)
          c.listingTimes.flatMap(t => slotsFor(Seq(c.day -> c.month), t))
        else
          seansByUrl.getOrElse(c.detailUrl, Seq.empty)
            .flatMap(seg => slotsFor(seg.days.map(d => d -> seg.month), seg.time))

      val sorted = showtimes
        .map(dt => Showtime(dt, Some(c.detailUrl)))
        .distinctBy(s => (s.dateTime, s.bookingUrl))
        .sortBy(_.dateTime)

      if (sorted.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(c.title),
        cinema    = cinema,
        posterUrl = c.poster,
        filmUrl   = Some(c.detailUrl),
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = sorted
      ))
    }.sortBy(_.movie.title)
  }

  /** Resolve (day, month) pairs at one time into `LocalDateTime`s, inferring
   *  the year from `today`. */
  private def slotsFor(days: Seq[(Int, Int)], time: LocalTime): Seq[LocalDateTime] =
    days.flatMap { case (day, month) =>
      val year = if (month < today.getMonthValue) today.getYear + 1 else today.getYear
      Try(LocalDateTime.of(LocalDate.of(year, month, day), time)).toOption
    }

  // ── Listing ─────────────────────────────────────────────────────────────

  /** One film card off the calendar listing. `listingTimes` is non-empty only
   *  for single screenings (the listing's `<span class="hour">`); multi-day
   *  runs leave it empty and are resolved from the detail page. */
  private case class Card(detailUrl: String, title: String, poster: Option[String],
                          day: Int, month: Int, listingTimes: Seq[LocalTime])

  private def parseListing(html: String): Seq[Card] =
    Jsoup.parse(html).select("a.box[href*=~z]").asScala.toSeq.flatMap { box =>
      val href  = box.attr("href")
      val title = Option(box.selectFirst("h2")).map(_.text.trim).filter(_.nonEmpty)
      val days  = box.select("span.day").asScala.toSeq.flatMap(d => Try(d.text.trim.toInt).toOption)
      // Month tokens excluding the "Od"/"Do" range labels (which use the same
      // `span.month` class). Take the first — single cards have one month, and
      // multi-day cards resolve their real month off the detail page anyway.
      val months = box.select("span.month").asScala.toSeq.map(_.text.trim)
        .flatMap(MonthAbbrev.get)
      val hours  = box.select("span.hour").asScala.toSeq
        .flatMap(h => ScraperParse.parseHHmm(h.text.trim))
      val poster = Option(box.selectFirst("div.img img[src]")).map(_.attr("src")).filter(_.nonEmpty)
        .map(absolute)
      for {
        t <- title
        d <- days.headOption
        m <- months.headOption
      } yield Card(absolute(href), t, poster, d, m, hours)
    }

  // ── Detail (Seans block) ──────────────────────────────────────────────────

  /** One schedule line off a detail page: a day (or day-range) at one time.
   *  e.g. "5-7 czerwca o 18:00" → Segment(Seq(5,6,7), 6, 18:00). */
  private case class Segment(days: Seq[Int], month: Int, time: LocalTime)

  private def parseSeans(html: String): Seq[Segment] = {
    val doc = Jsoup.parse(html)
    val block = doc.select("h3").asScala
      .find(h => SeansHeading.contains(h.text.trim))
      .flatMap(h => Option(h.parent))
    block.toSeq.flatMap { el =>
      // The block holds one or more lines separated by <br>; jsoup's
      // wholeText keeps the line breaks the <br>s introduce.
      el.wholeText.split("\n").map(_.trim).filter(_.nonEmpty).toSeq.flatMap(parseSeansLine)
    }
  }

  /** "13 – 16 lipca o 17:45" / "24 lipca o 20:00" → a Segment, or None. */
  private def parseSeansLine(line: String): Option[Segment] =
    SeansLine.findFirstMatchIn(line).flatMap { m =>
      val from  = m.group(1).toInt
      val to    = Option(m.group(2)).map(_.toInt).getOrElse(from)
      val month = ScraperParse.PolishMonths.get(m.group(3).toLowerCase)
      val time  = ScraperParse.parseHHmm(m.group(4))
      for { mo <- month; t <- time if to >= from }
        yield Segment((from to to).toSeq, mo, t)
    }
}

object KinoZakClient {
  val ListingUrl = "https://klubzak.com.pl/pl/kalendarz?category=kino"

  private val SeansHeading = Set("Seans", "Seanse")

  /** "DD[ - DD] <month-genitive> o HH:MM" — the day-range separator is a hyphen
   *  or an en/em-dash with optional surrounding spaces. */
  private val SeansLine =
    """(\d{1,2})(?:\s*[-–—]\s*(\d{1,2}))?\s+([\p{L}]+)\s+o\s+(\d{1,2}:\d{2})""".r

  /** Polish month abbreviations as the calendar listing spells them
   *  (`Cze`, `Lip`, …) → month number. */
  val MonthAbbrev: Map[String, Int] = Map(
    "Sty" -> 1, "Lut" -> 2, "Mar" -> 3, "Kwi" -> 4, "Maj" -> 5, "Cze" -> 6,
    "Lip" -> 7, "Sie" -> 8, "Wrz" -> 9, "Paź" -> 10, "Lis" -> 11, "Gru" -> 12
  )

  private def absolute(url: String): String =
    if (url.startsWith("http")) url
    else "https://klubzak.com.pl" + (if (url.startsWith("/")) url else "/" + url)
}
