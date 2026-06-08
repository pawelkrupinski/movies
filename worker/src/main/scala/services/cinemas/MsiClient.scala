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
  today:   LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import MsiClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(baseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val thisMonth = YearMonth.from(today)
    val nextMonth = thisMonth.plusMonths(1)

    val slots = Seq(thisMonth, nextMonth).flatMap { ym =>
      val url  = monthUrl(baseUrl, ym)
      val html = Try(http.get(url)).getOrElse("")
      if (html.isEmpty) Seq.empty
      else MsiScraper.parseMonthWithYear(html, ym, baseUrl, cleanTitle)
    }

    MsiScraper.toMovies(slots, cinema)
  }
}

object MsiClient {

  def monthUrl(baseUrl: String, ym: YearMonth): String =
    s"$baseUrl/MSI/mvc/pl?sort=Name&date=${ym.getYear}-${"%02d".format(ym.getMonthValue)}"

  /** Strip a trailing format-code parenthetical and sentence-case the result.
   *
   * Format tags match `(…)` at the end of the title where the inner text
   * contains one of the screen-technology keywords: `2D`, `3D`, `IMAX`,
   * `DOLBY`, `4DX`.  Examples:
   *   "TOY STORY 5 (2D DUBBING)"          → "Toy story 5"
   *   "OBSESJA (2D NAPISY DOLBY ATMOS)"   → "Obsesja"
   *   "90. URODZINY PAVAROTTIEGO (2D NAPISY)" → "90. Urodziny pavarottiego"
   * Titles that don't end in a format tag are sentence-cased verbatim.
   */
  private[cinemas] val FormatSuffixPat =
    """\s*\((?:2D|3D|IMAX|DOLBY|4DX)[^)]*\)\s*$""".r

  private[cinemas] def cleanTitle(raw: String): String = {
    // Jsoup already decodes HTML entities in attribute values; no need to
    // call Parser.unescapeEntities separately.
    val stripped = FormatSuffixPat.replaceFirstIn(raw.trim, "").trim
    ScraperParse.sentenceCase(stripped)
  }
}
