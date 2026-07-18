package services.cinemas.pl

import tools.HttpFetch
import models._
import org.jsoup.nodes.Document
import org.jsoup.Jsoup
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Centrum CSW (Toruń) — the film screen of Centrum Sztuki Współczesnej
 * Znaki Czasu. Screenings are published via the dpProEventCalendar WordPress
 * plugin, which renders a fully static HTML schedule at `/repertuar/`.
 *
 * The page is structured as a sequence of `div.box` elements, each containing
 * an `<h2>` with the date ("7 czerwca, niedziela") and either a
 * `<p class="no-events">` (no screenings that day) or a `<ul class="events">`
 * whose `<li>` children carry:
 *  - `<p class="hour">HH:MM</p>` — the start time.
 *  - `<h3><a href="https://csw.torun.pl/pec-events/<slug>/">Title</a></h3>` — film
 *    title and a link to the event detail page.
 *
 * The event detail pages contain per-date Bilety24 ticket links (different event
 * IDs per date), but resolving them would require fetching each event page per
 * screening. We capture the film URL (pointing to the per-event event page, which
 * aggregates all its dates) and leave bookingUrl as `None`; Bilety24 links for
 * each screening are accessible from the film URL.
 *
 * The year is inferred from `today` using the same 60-day lookback as other
 * clients. The schedule typically covers 1–2 weeks, well within that window.
 */
class KinoCentrumCswClient(
  http:             HttpFetch,
  override val cinema: Cinema,
  today:            LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import KinoCentrumCswClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val html = http.get(RepertoireUrl)
    val document  = Jsoup.parse(html)
    val slots = parseDocument(document, today)

    // Group by title: the same film appears on different days under different
    // per-date event URLs (e.g. "drzewo-magii-14/", "drzewo-magii-15/") — we
    // merge them into one CinemaMovie and use the first seen event URL as the
    // canonical film URL.
    SlotsToMovies.fold(
      slots,
      titleOf    = _.title,
      showtimeOf = s => Showtime(s.dateTime, bookingUrl = None),
      distinctBy = _.dateTime
    ) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = Some(group.head.eventUrl),
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }
}

object KinoCentrumCswClient {

  val BaseUrl       = "https://csw.torun.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar/"

  // "7 czerwca, niedziela" — day + genitive month + optional weekday suffix
  private val DayMonthPat = """(\d{1,2})\s+([\p{L}]+)""".r

  private[cinemas] case class RawSlot(
    title:    String,
    eventUrl: String,
    dateTime: LocalDateTime
  )

  private[cinemas] def parseDocument(document: Document, today: LocalDate): Seq[RawSlot] = {
    // All day boxes are in one wrapping div; select them directly.
    document.select("div.box").asScala.toSeq.flatMap { box =>
      val h2    = Option(box.selectFirst("h2")).map(_.text.trim).getOrElse("")
      val date  = parsePolishDate(h2, today)
      date match {
        case None    => Seq.empty
        case Some(d) =>
          box.select("ul.events li").asScala.toSeq.flatMap { li =>
            val hourText = Option(li.selectFirst("p.hour")).map(_.text.trim).getOrElse("")
            val anchor   = Option(li.selectFirst("h3 a[href]"))
            val title    = anchor.map(_.text.trim).filter(_.nonEmpty)
            val eventUrl = anchor.map(_.attr("href")).filter(_.nonEmpty)

            for {
              t   <- title
              url <- eventUrl
              lt  <- ScraperParse.parseHHmm(hourText)
            } yield RawSlot(t, url, LocalDateTime.of(d, lt))
          }
      }
    }
  }

  /** "7 czerwca, niedziela" → LocalDate.
   *  If the date falls more than 60 days before today, assume next year. */
  private[cinemas] def parsePolishDate(text: String, today: LocalDate): Option[LocalDate] =
    DayMonthPat.findFirstMatchIn(text).flatMap { m =>
      ScraperParse.PolishMonths.get(m.group(2).toLowerCase).flatMap { month =>
        Try {
          val day  = m.group(1).toInt
          val candidate = LocalDate.of(today.getYear, month, day)
          if (candidate.isBefore(today.minusDays(60))) candidate.plusYears(1) else candidate
        }.toOption
      }
    }
}
