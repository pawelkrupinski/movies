package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Pod Baranami (Kraków) — historic arthouse cinema on Rynek Główny.
 * The cinema publishes a self-hosted PHP repertoire at `/repertuar.php`
 * (ISO-8859-2 encoded), listing the full week's schedule on one page.
 *
 * Structure of the page:
 *   - `p.rep_date` — a day header: "Niedziela 7 czerwca // Sunday, June 7".
 *     Contains the day-of-month and a Polish genitive month name; the year is
 *     absent and must be inferred from the current date.
 *   - `ul.program_list > li` — one film entry per `<li>`. Each entry holds:
 *       - `a[href^="film.php"]` — the film title (anchor text) and cinema-relative
 *         film URL.
 *       - `span > a[href^="/rezerwacja_start.php?event_id="]` — one or more
 *         screening time links, with the time as the anchor text (`HH:MM`) and
 *         the booking URL as the href. The `onclick` on each link encodes the
 *         date and time as positional arguments but we read the time directly
 *         from the anchor text.
 *
 * A single `<li>` can carry several screening times across the same day,
 * each as a separate `<a>` inside the `<span>`. The parser emits one
 * `RawSlot` per time × date.
 */
class KinoPodBaranamiClient(
  http:             HttpFetch,
  override val cinema: Cinema,
  today:            LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import KinoPodBaranamiClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val html  = http.getBytes(RepertoireUrl)
    val str   = new String(html, "ISO-8859-2")
    val slots = parseDocument(str, today)

    val byFilmUrl = slots.groupBy(s => (s.title, s.filmUrl))
    byFilmUrl.toSeq.flatMap { case ((title, filmUrl), group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, s.bookingUrl))
        .distinctBy(s => (s.dateTime, s.bookingUrl))
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = filmUrl,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      ))
    }.sortBy(_.movie.title)
  }
}

object KinoPodBaranamiClient {

  val BaseUrl       = "https://kinopodbaranami.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar.php"

  // "7 czerwca" or "10 maja" — day + Polish genitive month name
  private val DayMonthPat = """(\d{1,2})\s+(\w+)""".r

  private[cinemas] case class RawSlot(
    title:      String,
    filmUrl:    Option[String],
    dateTime:   LocalDateTime,
    bookingUrl: Option[String]
  )

  /** Infer year: if the date falls more than 60 days in the past relative to
    * `today`, it must belong to next year (handles the December→January turn). */
  private def guessYear(day: Int, month: Int, today: LocalDate): Int = {
    val candidate = Try(LocalDate.of(today.getYear, month, day)).toOption
    candidate match {
      case Some(d) if d.isBefore(today.minusDays(60)) => today.getYear + 1
      case _ => today.getYear
    }
  }

  private[cinemas] def parseDocument(html: String, today: LocalDate): Seq[RawSlot] = {
    val document = Jsoup.parse(html)
    val slots = collection.mutable.ArrayBuffer.empty[RawSlot]
    var currentDate: Option[LocalDate] = None

    document.select("p.rep_date, ul.program_list").asScala.foreach { el =>
      el.tagName match {
        case "p" if el.hasClass("rep_date") =>
          // "Niedziela 7 czerwca // Sunday, June 7"
          val text = el.text.trim
          currentDate = DayMonthPat.findFirstMatchIn(text).flatMap { m =>
            val day   = m.group(1).toInt
            val month = ScraperParse.PolishMonths.get(m.group(2).toLowerCase)
            month.flatMap { mo =>
              val yr = guessYear(day, mo, today)
              Try(LocalDate.of(yr, mo, day)).toOption
            }
          }

        case "ul" if el.hasClass("program_list") =>
          currentDate.foreach { date =>
            el.select("li").asScala.foreach { li =>
              val titleAnchor = Option(li.selectFirst("a[href^=\"film.php\"]"))
              val title       = titleAnchor.map(_.text.trim).filter(_.nonEmpty).getOrElse("")
              val filmUrl     = titleAnchor.map(a => s"$BaseUrl/${a.attr("href").trim}").filter(_.nonEmpty)

              if (title.nonEmpty) {
                // One or more time links inside <span>
                li.select("span a[href^=\"/rezerwacja_start.php\"]").asScala.foreach { a =>
                  val time       = ScraperParse.parseHHmm(a.text.trim)
                  val bookingUrl = Some(a.attr("href").trim).filter(_.nonEmpty)
                    .map(h => if (h.startsWith("http")) h else s"$BaseUrl$h")
                  time.foreach { t =>
                    slots += RawSlot(title, filmUrl, LocalDateTime.of(date, t), bookingUrl)
                  }
                }
              }
            }
          }

        case _ =>
      }
    }
    slots.toSeq
  }
}
