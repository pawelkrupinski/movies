package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Promień (Dom Kultury, Tuchów) — its repertoire at
 * `kinotuchow.pl/repertuar/` is a two-level WordPress ("Moview" theme) site:
 *   - the listing page is a poster grid, one `h4.movie-title > a[href]` per
 *     film, linking to that film's `kinotuchow.pl/movie/<slug>/` detail page;
 *   - the detail page holds the schedule as FREE TEXT inside the `<ul>` that
 *     follows the `<strong>Seanse:</strong>` heading, e.g.
 *       `12 czerwca godz. 17:00`              (single day, single time)
 *       `14 – 15 czerwca godz. 17:00`         (a day RANGE, single time)
 *       `19 – 23 czerwca godz. 10:00*, 15:00, 17:00`  (range × times)
 *
 * There is no AJAX/JSON/JSON-LD feed and no per-screening booking link — the
 * dates and times are only ever this Polish free text, and the dates carry no
 * YEAR (the page just spells "czerwca"). So we:
 *   1. fetch the listing, collect every detail-page URL;
 *   2. fetch each detail page, parse its `Seanse:` lines into `LocalDate`s
 *      (expanding `14 – 15 czerwca` into both days) × `LocalTime`s (the
 *      comma-separated `godz.` list, ignoring `*` group-screening markers);
 *   3. infer the year from `today` (this year unless that lands over a month
 *      in the past, e.g. a December page seen in January, then roll forward)
 *      and keep only screenings on/after `today`.
 *
 * Synopsis/cast/director/poster are left empty — the detail page's prose body
 * mixes ticket-price and box-office-hours text into the same block, so there's
 * no clean synopsis to lift.
 */
class KinoPromienClient(
  http:        HttpFetch,
  override val cinema: Cinema = KinoPromien,
  today:       LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import KinoPromienClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(RepertoireUrl)

  def fetch(): Seq[CinemaMovie] = {
    val detailUrls = filmUrls(http.get(RepertoireUrl))
    val slots = detailUrls.flatMap { url =>
      val (title, dateTimes) = parseDetail(Try(http.get(url)).getOrElse(""), today)
      dateTimes.map(dt => (title, dt, url))
    }

    SlotsToMovies.fold(
      slots.filter(_._1.nonEmpty),
      titleOf    = _._1,
      showtimeOf = { case (_, dt, _) => Showtime(dt, None) },
      distinctBy = _.dateTime
    ) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = group.map(_._3).headOption,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }
}

object KinoPromienClient {

  val BaseUrl       = "https://kinotuchow.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar/"

  /** A `Seanse:` line: an optional `<day>` (or `<day> – <day>` range), a Polish
   *  genitive month, then `godz.` and a comma-list of times. The time list is
   *  captured raw and re-split on `,` (each time may carry a trailing `*`
   *  group-screening marker, which `parseHHmm` ignores). */
  private val ScreeningLine =
    """(\d{1,2})(?:\s*[–—-]\s*(\d{1,2}))?\s+([a-ząćęłńóśźż]+)\s+godz\.?\s*([\d:*,\s]+)""".r

  /** The trailing ` (YYYY)` release-year tag on a detail-page heading. */
  private val TitleYearSuffix = """\s*\(\d{4}\)\s*$""".r

  /** Every detail-page URL on the listing page (one per film in the grid). */
  private[cinemas] def filmUrls(html: String): Seq[String] =
    Jsoup.parse(html, BaseUrl)
      .select("h4.movie-title a[href]").asScala.toSeq
      .map(_.attr("abs:href"))
      .filter(_.contains("/movie/"))
      .distinct

  /** (film title, screening datetimes) for one detail page. The title is the
   *  hero heading; the datetimes are every `Seanse:` line expanded to
   *  day × time, year inferred from `today`, kept only on/after `today`. */
  private[cinemas] def parseDetail(html: String, today: LocalDate): (String, Seq[LocalDateTime]) = {
    val document = Jsoup.parse(html, BaseUrl)
    val title    = titleOf(document)
    val dateTimes = seanseLines(document)
      .flatMap(line => screeningsOf(line, today))
      .filter(!_.toLocalDate.isBefore(today))
      .distinct
    (title, dateTimes)
  }

  /** The film title from the detail page's hero heading (`div.moview-info >
   *  h1`, e.g. "Toy Story 5 (2026)"); the page's other `<h1>` is the navbar
   *  brand logo. The trailing ` (YYYY)` release-year tag is dropped so the
   *  title matches the other sources. */
  private def titleOf(document: Document): String =
    Option(document.selectFirst("div.moview-info h1"))
      .map(h => TitleYearSuffix.replaceFirstIn(h.text.trim, ""))
      .getOrElse("")

  /** The text of each line under the `<strong>Seanse:</strong>` heading. The
   *  showtimes live in the `<ul>` that follows the `<p>` carrying "Seanse:";
   *  `<br>`-separated lines render as separate text fragments, so we split the
   *  `<li>` text on newlines and let the per-line regex pick out date lines. */
  private def seanseLines(document: Document): Seq[String] = {
    val seanseHeading = document.select("p:contains(Seanse)").asScala.headOption
    val list = seanseHeading.flatMap(p => Option(p.nextElementSibling)).filter(_.tagName == "ul")
    list.toSeq.flatMap { ul =>
      ul.select("li").asScala.toSeq.flatMap { li =>
        li.wholeText.split("\n").map(_.trim).filter(_.nonEmpty)
      }
    }
  }

  /** Expand one `Seanse:` line into every (day in range) × (time in the
   *  `godz.` list) screening, with the year inferred from `today`. */
  private def screeningsOf(line: String, today: LocalDate): Seq[LocalDateTime] =
    ScreeningLine.findFirstMatchIn(line).toSeq.flatMap { m =>
      val startDay = m.group(1).toInt
      val endDay   = Option(m.group(2)).map(_.toInt).getOrElse(startDay)
      val month    = ScraperParse.PolishMonths.get(m.group(3).toLowerCase)
      val times    = m.group(4).split(",").iterator.flatMap(ScraperParse.parseHHmm).toSeq
      for {
        mth  <- month.toSeq
        day  <- startDay to endDay
        date <- inferredDate(day, mth, today).toSeq
        time <- times
      } yield LocalDateTime.of(date, time)
    }

  /** A `LocalDate` for `(day, month)` with the year inferred from `today`: this
   *  year unless that lands more than a month in the past (a December page seen
   *  in January), in which case roll to next year. */
  private def inferredDate(day: Int, month: Int, today: LocalDate): Option[LocalDate] =
    Try(LocalDate.of(today.getYear, month, day)).toOption.map { thisYear =>
      if (thisYear.isBefore(today.minusMonths(1))) thisYear.plusYears(1) else thisYear
    }
}
