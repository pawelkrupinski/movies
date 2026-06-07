package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Agrafka (Kraków) — an independent arthouse cinema run by Fundacja
 * Wspierania Kultury Filmowej Cyrk Edison. The venue publishes a
 * server-rendered multi-week schedule at `/rep.php`, covering roughly four
 * weeks from the current date.
 *
 * The schedule is structured as repeating table blocks:
 *   - `<h3>D month YYYY /weekday/</h3>` — a date header for each day, e.g.
 *     "7 czerwca 2026 /niedziela/".
 *   - `table.repertoire > tbody > tr` — rows for that day's screenings.
 *     Each row has:
 *       - `td.hour` — the start time (`H:MM` or `HH:MM`).
 *       - `td.title a[href]` — the film title (anchor text, uppercased by the
 *         page — we normalise to title-case via `TitleCaseIfAllLower` later).
 *         The href is a cinema-relative film URL (`film.php?film_id=N`).
 *       - `td.link a[href]` — a "Kup bilet" link to the ticketing subdomain
 *         (`bilety.kinoagrafka.pl/repertoire.html?id=N`). May be absent for
 *         some screenings.
 *
 * The parser carries the last seen date forward to all rows in a block, just
 * as KinoSfinksClient does. There is no per-film detail page fetch — the
 * schedule is self-contained for showtimes. TMDB enriches the rest downstream.
 */
class KinoAgrafkaClient(http: HttpFetch, override val cinema: Cinema) extends CinemaScraper {

  import KinoAgrafkaClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val html = http.get(RepertoireUrl)
    val slots = parseDoc(html)

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

object KinoAgrafkaClient {

  val BaseUrl        = "https://kinoagrafka.pl"
  val RepertoireUrl  = s"$BaseUrl/rep.php"

  // Matches "7 czerwca 2026 /niedziela/" — day, Polish genitive month, 4-digit year
  private val DatePat = """(\d{1,2})\s+(\w+)\s+(\d{4})""".r

  private[cinemas] case class RawSlot(
    title:      String,
    filmUrl:    Option[String],
    dateTime:   LocalDateTime,
    bookingUrl: Option[String]
  )

  private[cinemas] def parseDoc(html: String): Seq[RawSlot] = {
    val doc = Jsoup.parse(html)
    var currentDate: Option[LocalDate] = None
    val slots = collection.mutable.ArrayBuffer.empty[RawSlot]

    // Each day block is a <div> wrapping an <h3> date header and a <table>.
    // Alternatively the h3 and tables are siblings. We walk ALL elements in
    // document order to carry the last-seen date forward across rows.
    doc.select("h3, table.repertoire tbody tr").asScala.foreach { el =>
      el.tagName match {
        case "h3" =>
          currentDate = parseDate(el.text.trim)
        case "tr" =>
          currentDate.foreach { date =>
            val timeText  = Option(el.selectFirst("td.hour")).map(_.text.trim).getOrElse("")
            val titleEl   = Option(el.selectFirst("td.title a"))
            val time      = ScraperParse.parseHHmm(timeText)
            val rawTitle  = titleEl.map(_.text.trim).getOrElse("")
            // Titles are ALL-CAPS on the page; normalise to title-case.
            val title     = tools.TextNormalization.titleCaseIfAllLower(rawTitle)
            val filmHref  = titleEl.map(_.attr("href").trim).filter(_.nonEmpty)
            val filmUrl   = filmHref.map(h => if (h.startsWith("http")) h else s"$BaseUrl/$h")
            val bookingUrl = Option(el.selectFirst("td.link a[href]"))
              .map(_.attr("href").trim).filter(_.nonEmpty)
              .map(h => if (h.startsWith("http")) h else s"$BaseUrl/$h")

            for {
              t     <- time if title.nonEmpty
            } slots += RawSlot(title, filmUrl, LocalDateTime.of(date, t), bookingUrl)
          }
        case _ =>
      }
    }
    slots.toSeq
  }

  private def parseDate(text: String): Option[LocalDate] =
    DatePat.findFirstMatchIn(text).flatMap { m =>
      val day   = m.group(1).toInt
      val month = ScraperParse.PolishMonths.get(m.group(2).toLowerCase)
      val year  = Try(m.group(3).toInt).toOption
      for (mo <- month; yr <- year; d <- Try(LocalDate.of(yr, mo, day)).toOption) yield d
    }
}
