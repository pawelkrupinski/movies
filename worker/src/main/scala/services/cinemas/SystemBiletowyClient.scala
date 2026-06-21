package services.cinemas

import models._
import org.jsoup.Jsoup
import services.movies.TitleNormalizer
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Generic client for cinemas ticketed through the VisualSoft ticketing platform
 * — branded `systembiletowy.pl` when a venue takes the vendor's subdomain (e.g.
 * `shd.systembiletowy.pl` for the Suchedniów cultural centre's Kino Kuźnica),
 * but the SAME software is also white-labelled onto venues' own domains
 * (`bilety.kino.bochnia.pl`, `kgl.systembiletowy.pl`, …). The instance homepage
 * (`<base>/index.php`) is server-rendered in one of three skins:
 *
 *   1. `table.tbl_repertoire` rows — `td.title a` / `td.date span.day|hour` /
 *      `td.link a` (`repertoire.html?id=N` booking link).
 *   2. Bootstrap `div.event-item` rows — `div.title a` / `div.date`
 *      ("… 10 czerwca 2026 … godz. 13:30") with a `repertoire.html` link.
 *   3. The current `/css/visual9` skin — `div.event-item[data-date][data-time]`
 *      carrying the ISO date + time as attributes, an `h3.event-title`, and a
 *      `/index.php/kup-bilet/…` booking link.
 *
 * One instance per venue, captured by its `baseUrl` + `cinema`, so adding a
 * VisualSoft-hosted cinema is a catalog line, not a new client (OCP).
 *
 * Previously scraped from Filmweb, which had silently gone empty for the venue
 * (every poll returned `[]`) though the cinema is open and screening.
 */
class SystemBiletowyClient(http: HttpFetch, baseUrl: String, override val cinema: Cinema)
    extends CinemaScraper with OnlyMovieEventsFilter {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(baseUrl)
  override def sourceUrl: Option[String] = Some(baseUrl)

  protected def fetchUnfiltered(): Seq[CinemaMovie] =
    SystemBiletowyClient.parse(http.get(s"$baseUrl/index.php"), cinema, baseUrl)
}

object SystemBiletowyClient {

  // "12 czerwca 2026" — day, Polish genitive month, year (all present).
  private val DatePat = """(\d{1,2})\s+(\p{L}+)\s+(\d{4})""".r

  private case class RawSlot(title: String, dateTime: LocalDateTime, booking: Option[String])

  def parse(html: String, cinema: Cinema, baseUrl: String): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, baseUrl)
    // Per-cinema title cleanup (PerCinema rules) on top of the shared cleanTitle.
    def clean(raw: String): String = TitleNormalizer.cinemaClean(cinema.slug, cleanTitle(raw))

    val tblSlots = document.select("table.tbl_repertoire tr").asScala.toSeq.flatMap { tr =>
      // Only rows that are a real screening carry a repertoire/booking link.
      if (tr.selectFirst("a[href*=repertoire.html]") == null) None
      else for {
        titleElement <- Option(tr.selectFirst("td.title a"))
        title    = clean(titleElement.text) if title.nonEmpty
        dayText <- Option(tr.selectFirst("td.date span.day")).map(_.text)
        d       <- DatePat.findFirstMatchIn(dayText)
        month   <- ScraperParse.PolishMonths.get(d.group(2).toLowerCase)
        time    <- Option(tr.selectFirst("td.date span.hour")).flatMap(h => ScraperParse.parseHHmm(h.text))
        dt      <- Try(LocalDateTime.of(d.group(3).toInt, month, d.group(1).toInt, time.getHour, time.getMinute)).toOption
      } yield RawSlot(
        title    = title,
        dateTime = dt,
        booking  = Option(tr.selectFirst("td.link a[href]")).map(_.attr("abs:href"))
                     .filter(_.nonEmpty).orElse(Option(titleElement.attr("abs:href")).filter(_.nonEmpty))
      )
    }

    // Alternate Bootstrap-grid skin (Pszczyna, Żory, Oświęcim): one
    // `div.event-item` per screening, with the Polish full date + time mashed
    // into `div.date` ("… 10 czerwca 2026 … godz. 13:30") and the title/booking
    // in `div.title a`.
    val altSlots = document.select("div.event-item:has(a[href*=repertoire.html])").asScala.toSeq.flatMap { item =>
      for {
        titleElement <- Option(item.selectFirst("div.title a"))
        title    = clean(titleElement.text) if title.nonEmpty
        dateText <- Option(item.selectFirst("div.date")).map(_.text)
        d       <- DatePat.findFirstMatchIn(dateText)
        month   <- ScraperParse.PolishMonths.get(d.group(2).toLowerCase)
        time    <- ScraperParse.parseHHmm(dateText)
        dt      <- Try(LocalDateTime.of(d.group(3).toInt, month, d.group(1).toInt, time.getHour, time.getMinute)).toOption
      } yield RawSlot(
        title    = title,
        dateTime = dt,
        booking  = Option(titleElement.attr("abs:href")).filter(_.nonEmpty)
      )
    }

    // Current `/css/visual9` skin (kgl/kck.systembiletowy.pl, bilety.kino.bochnia.pl):
    // one `div.event-item` per screening with the ISO date + time as data
    // attributes, the title in `h3.event-title`, and a `kup-bilet` booking link.
    // The booking-link slug embeds the FIRST screening's date, not this row's, so
    // the showtime is read from the attributes — never parsed out of the href.
    val attrSlots = document.select("div.event-item[data-date]").asScala.toSeq.flatMap { item =>
      for {
        titleElement <- Option(item.selectFirst("h3.event-title"))
        title    = clean(titleElement.text) if title.nonEmpty
        day     <- Try(LocalDate.parse(item.attr("data-date"))).toOption
        time    <- ScraperParse.parseHHmm(item.attr("data-time"))
      } yield RawSlot(
        title    = title,
        dateTime = day.atTime(time),
        booking  = Option(item.selectFirst("a[href*=kup-bilet]")).map(_.attr("abs:href")).filter(_.nonEmpty)
      )
    }

    val slots = (tblSlots ++ altSlots ++ attrSlots).distinctBy(s => (s.title, s.dateTime, s.booking))
    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking)) { (title, _, showtimes) =>
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

  /** Drop the trailing `dubbing`/`napisy`/… version tag (so the same film's
   *  dubbed and subtitled screenings merge into one row) and sentence-case the
   *  result. Tag stripping is shared with the other portal clients. */
  private[cinemas] def cleanTitle(raw: String): String =
    ScraperParse.sentenceCase(ScraperParse.stripFormatTags(raw))
}
