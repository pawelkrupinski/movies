package services.cinemas

import models._
import tools.HttpFetch

import java.time.{LocalDate, YearMonth, ZoneId}
import scala.util.Try

/**
 * Generic client for any cinema running on the MSI ticketing platform (the
 * ASP.NET portal whose month route `<base>/MSI/mvc/pl?sort=Name&date=YYYY-MM`
 * renders every screening in a calendar month as a single server-rendered HTML
 * page — no JavaScript, no session nonce). One instance per venue, captured by
 * its `baseUrl` + `cinema`, so adding an MSI-hosted cinema is a new catalog
 * line rather than a new client (OCP). The shared parsing lives in
 * [[MsiScraper]].
 *
 * Known venues on this platform:
 *   - Cinema1 Gdańsk          — bilety.cinemaone.pl
 *   - Kino GOK Tychowo        — bilety.goktychowo.pl
 *   - Kino MOK Nowa Ruda      — bilety.nowaruda.pl
 *   - Kino Warszawa Przeworsk — bilety-kino.przeworsk.um.gov.pl
 *   - Kino Powiśle Sztum      — kinosztumbilety.pl
 * (The last four were previously scraped from Filmweb, which had silently
 * stopped carrying their seances — every poll returned an empty list — so the
 * showtimes are read straight from each venue's own MSI portal instead.)
 *
 * This client fetches the current month and the following month (to cover
 * advance-booking windows), then merges the results by cleaned title.
 *
 * === Title cleaning ===
 * MSI appends a format tag to every title, e.g.
 *   `BACKROOMS. BEZ WYJŚCIA (2D NAPISY)`
 *   `TOY STORY 5 (2D DUBBING DOLBY ATMOS)`
 * The cleaning step strips a trailing `(…)` that contains a screen-technology
 * keyword (`2D`, `3D`, `IMAX`, `DOLBY`, `4DX`) and normalises the title to
 * sentence case so it merges correctly with TMDB and other sources.
 *
 * @param http    HTTP client (swap for `FakeHttpFetch` in tests).
 * @param baseUrl Scheme + host of the venue's MSI portal, no trailing slash
 *                (e.g. `https://bilety.goktychowo.pl`).
 * @param cinema  The [[Cinema]] source tag attached to every [[CinemaMovie]].
 * @param today   Calendar anchor for computing the two months to fetch;
 *                defaults to the current Warsaw clock date.
 */
class MsiClient(
  http:    HttpFetch,
  baseUrl: String,
  override val cinema: Cinema,
  today:   LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw")),
  // The month-page route. Almost every venue serves it at `/MSI/mvc/pl`, but a
  // few VisualTicket installs expose the identical page under a different prefix
  // (e.g. Kino Planeta Brzesko at `/Rezerwacja/mvc/pl`); override for those.
  mvcPath: String = "/MSI/mvc/pl",
  // Some MSI portals host TWO cinemas on one site and disambiguate by prefixing
  // every title with the venue name and a " - " separator ("Chemik - …",
  // "TWIERDZA - …" on bilety.mok.com.pl). When set, this instance keeps only the
  // titles carrying this prefix and strips it before cleaning — so the one
  // shared portal yields a separate per-cinema feed (one MsiClient per venue).
  titlePrefix: Option[String] = None
) extends CinemaScraper {

  import MsiClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(baseUrl)
  override def sourceUrl: Option[String] = Some(baseUrl)

  private val titleCleaner: String => String =
    titlePrefix.map(cleanTitleForVenue).getOrElse(cleanTitle)

  def fetch(): Seq[CinemaMovie] = {
    val thisMonth = YearMonth.from(today)
    val nextMonth = thisMonth.plusMonths(1)

    val slots = Seq(thisMonth, nextMonth).flatMap { ym =>
      val url  = monthUrl(baseUrl, mvcPath, ym)
      val html = Try(http.get(url)).getOrElse("")
      if (html.isEmpty) Seq.empty
      else MsiScraper.parseMonthWithYear(html, ym, baseUrl, titleCleaner)
    }

    MsiScraper.toMovies(slots, cinema)
  }
}

object MsiClient {

  def monthUrl(baseUrl: String, mvcPath: String, ym: YearMonth): String =
    s"$baseUrl$mvcPath?sort=Name&date=${ym.getYear}-${"%02d".format(ym.getMonthValue)}"

  /** Strip the trailing format/version tag MSI appends and sentence-case the
   *  result, so screening variants of one film merge. Examples:
   *    "TOY STORY 5 (2D DUBBING)"   → "Toy story 5"
   *    "STRASZNY FILM - NAPISY"     → "Straszny film"
   *    "90. URODZINY PAVAROTTIEGO (2D NAPISY)" → "90. Urodziny pavarottiego"
   *  (Tag stripping is shared with the other portal clients via
   *  `ScraperParse.stripFormatTags` — not migrated to a per-cinema rule, since
   *  it's a cross-client concern, not MSI-specific.) */
  private[cinemas] def cleanTitle(raw: String): String =
    ScraperParse.sentenceCase(ScraperParse.stripFormatTags(raw))

  /** Title cleaner for one venue of a two-cinema MSI portal whose titles are
   *  prefixed `<venue> - …` (e.g. "Chemik - Dzień objawienia"). Keeps only the
   *  titles carrying this exact prefix (case-insensitive) and strips it before
   *  the normal clean; returns `""` for the other venue's titles so the scraper
   *  drops them. */
  private[cinemas] def cleanTitleForVenue(venue: String)(raw: String): String = {
    val sep = s"$venue - "
    if (raw.regionMatches(true, 0, sep, 0, sep.length)) cleanTitle(raw.substring(sep.length))
    else ""
  }
}
