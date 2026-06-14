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
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val html = http.get(RepertoireUrl)
    val slots = parseDocument(html)

    val byFilmUrl = slots.groupBy(s => (s.title, s.filmUrl))
    byFilmUrl.toSeq.flatMap { case ((title, filmUrl), group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, s.bookingUrl))
        .distinctBy(s => (s.dateTime, s.bookingUrl))
        .sortBy(_.dateTime)
      val meta = group.map(_.meta).find(m => m.directors.nonEmpty || m.year.isDefined).getOrElse(MetaInfo.empty)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title, releaseYear = meta.year),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = filmUrl,
        synopsis  = None,
        cast      = Seq.empty,
        director  = meta.directors,
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

  // A production year inside the country/year block ("Kanada/Belgia/Francja 2024").
  private val YearPat = """\b(?:19|20)\d{2}\b""".r

  /** Per-film metadata carried in the `td.title` cell after the `reż.` marker:
   *  `reż. <directors>, <countries> <year>, <runtime>’`. The country/year/runtime
   *  block is inconsistently delimited — sometimes `Country YEAR` is one segment
   *  ("Kanada/Belgia/Francja 2024"), sometimes the country is its own segment
   *  ("Wlk. Brytania, USA 2026"), and some films carry no year ("Irak/Francja/Katar,
   *  87’"). Directors are therefore the leading comma-segments that look like
   *  person names: no digit, no slash (multi-country marker) and not a recognised
   *  country. The year is the first 4-digit token anywhere in the tail. */
  private[cinemas] case class MetaInfo(directors: Seq[String], year: Option[Int])
  private[cinemas] object MetaInfo { val empty = MetaInfo(Seq.empty, None) }

  private[cinemas] case class RawSlot(
    title:      String,
    filmUrl:    Option[String],
    dateTime:   LocalDateTime,
    bookingUrl: Option[String],
    meta:       MetaInfo
  )

  private val DirectoryMarker = "reż."

  private[cinemas] def parseMeta(cellText: String): MetaInfo = {
    val index = cellText.indexOf(DirectoryMarker)
    if (index < 0) MetaInfo.empty
    else {
      val tail = cellText.substring(index + DirectoryMarker.length)
      val directors = tail.split(",").iterator.map(_.trim)
        .takeWhile(seg => !seg.exists(_.isDigit) && !seg.contains("/") && !CountryNames.isPolish(seg))
        .flatMap(_.split(";")).map(_.trim)   // some films join co-directors with ';'
        .filter(_.nonEmpty).toSeq
      val year = YearPat.findFirstMatchIn(tail).map(_.matched.toInt)
      MetaInfo(directors, year)
    }
  }

  private[cinemas] def parseDocument(html: String): Seq[RawSlot] = {
    val document = Jsoup.parse(html)
    var currentDate: Option[LocalDate] = None
    val slots = collection.mutable.ArrayBuffer.empty[RawSlot]

    // Each day block is a <div> wrapping an <h3> date header and a <table>.
    // Alternatively the h3 and tables are siblings. We walk ALL elements in
    // document order to carry the last-seen date forward across rows.
    document.select("h3, table.repertoire tbody tr").asScala.foreach { el =>
      el.tagName match {
        case "h3" =>
          currentDate = parseDate(el.text.trim)
        case "tr" =>
          currentDate.foreach { date =>
            val timeText  = Option(el.selectFirst("td.hour")).map(_.text.trim).getOrElse("")
            val titleEl   = Option(el.selectFirst("td.title a"))
            val meta      = parseMeta(Option(el.selectFirst("td.title")).map(_.text).getOrElse(""))
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
            } slots += RawSlot(title, filmUrl, LocalDateTime.of(date, t), bookingUrl, meta)
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
