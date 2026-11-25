package services.cinemas.pl

import java.time.{LocalDate, LocalDateTime}

import scala.jdk.CollectionConverters.*

import models.*
import models.CountryNames
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{CreditedScreening, ListingFacts, ListingPageScraper, SlotsToMovies, ScrapeClock, ScrapeDates}
import tools.HttpFetch

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
class KinoAgrafkaClient(http: HttpFetch, override val cinema: Cinema)
    extends ListingPageScraper(http, KinoAgrafkaClient.RepertoireUrl, KinoAgrafkaClient.BaseUrl) {

  protected def parsePage(html: String): Seq[CinemaMovie] =
    SlotsToMovies.byTitleAndFilmPage(KinoAgrafkaClient.parseDocument(html), cinema)
}

object KinoAgrafkaClient {

  val BaseUrl        = "https://kinoagrafka.pl"
  val RepertoireUrl  = s"$BaseUrl/rep.php"

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
  private val DirectoryMarker = "reż."

  private[cinemas] def parseMeta(cellText: String): ListingFacts = {
    val index = cellText.indexOf(DirectoryMarker)
    if (index < 0) ListingFacts()
    else {
      val tail = cellText.substring(index + DirectoryMarker.length)
      val directors = tail.split(",").iterator.map(_.trim)
        .takeWhile(seg => !seg.exists(_.isDigit) && !seg.contains("/") && !CountryNames.isPolish(seg))
        .flatMap(_.split(";")).map(_.trim)   // some films join co-directors with ';'
        .filter(_.nonEmpty).toSeq
      val year = YearPat.findFirstMatchIn(tail).map(_.matched.toInt)
      ListingFacts(directors, year = year)
    }
  }

  // Each day block is a <div> wrapping an <h3> date header and a <table>.
  // Alternatively the h3 and tables are siblings. We walk ALL elements in
  // document order to carry the last-seen date forward across rows.
  private[cinemas] def parseDocument(html: String): Seq[CreditedScreening] =
    ScrapeDates.underDayHeaders(Jsoup.parse(html).select("h3, table.repertoire tbody tr").asScala) {
      case h3 if h3.tagName == "h3" => ScrapeDates.parseDayMonthYear(h3.text)  // "7 czerwca 2026 /niedziela/"
    }(screeningOf)

  private def screeningOf(row: Element, date: LocalDate): Option[CreditedScreening] = {
    val titleElement = Option(row.selectFirst("td.title a"))
    // Titles are ALL-CAPS on the page; normalise to title-case.
    val title        = tools.TextNormalization.titleCaseIfAllLower(titleElement.map(_.text.trim).getOrElse(""))
    for {
      time <- ScrapeClock.parseHHmm(Option(row.selectFirst("td.hour")).map(_.text.trim).getOrElse("")) if title.nonEmpty
    } yield CreditedScreening(
      title,
      filmUrl    = absolute(titleElement.map(_.attr("href").trim)),
      dateTime   = LocalDateTime.of(date, time),
      bookingUrl = absolute(Option(row.selectFirst("td.link a[href]")).map(_.attr("href").trim)),
      facts      = parseMeta(Option(row.selectFirst("td.title")).map(_.text).getOrElse(""))
    )
  }

  private def absolute(href: Option[String]): Option[String] =
    href.filter(_.nonEmpty).map(h => if (h.startsWith("http")) h else s"$BaseUrl/$h")
}
