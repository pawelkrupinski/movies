package services.cinemas

import models._
import tools.HttpFetch

import java.time.{LocalDate, YearMonth, ZoneId}
import scala.util.Try

/**
 * Cinema1 (cinemaone.pl) — the independent art-house multiplex in Gdańsk Old
 * Town (Targ Węglowy 5).  The cinema's own WordPress-based listing page is
 * JavaScript-rendered and requires a session-bound nonce to load per-day
 * showtimes; the underlying MSI ticketing portal at
 * `https://bilety.cinemaone.pl/MSI/mvc/pl?sort=Name&date=YYYY-MM` returns a
 * fully server-rendered HTML page for any given calendar month.  No session,
 * no JS needed.
 *
 * This client fetches the current month and the following month (to cover
 * advance booking windows), then merges the results by cleaned title.
 *
 * === Title cleaning ===
 * Cinema1 appends a format tag to every title, e.g.
 *   `BACKROOMS. BEZ WYJŚCIA (2D NAPISY)`
 *   `TOY STORY 5 (2D DUBBING DOLBY ATMOS)`
 *   `MINIMARATON SUPERGIRL & SUPERMAN (2D NAPISY)`
 * The cleaning step strips trailing `(…)` that contain a screen-technology
 * keyword (`2D`, `3D`, `IMAX`, `DOLBY`, `4DX`) and normalises the title to
 * sentence case so it merges correctly with TMDB and other sources.
 *
 * === Booking URLs ===
 * Each event anchor points to `bilety.cinemaone.pl/MSI/Default.aspx?event_id=N`
 * which is the per-screening booking page.
 *
 * @param http   HTTP client (swap for `FakeHttpFetch` in tests).
 * @param cinema The [[Cinema]] source tag attached to every [[CinemaMovie]]
 *               produced by this client.  Callers supply the concrete case
 *               object defined in [[Cinema.scala]].
 * @param today  Calendar anchor for computing the two months to fetch; defaults
 *               to the current Warsaw clock date.
 */
class Cinema1Client(
  http:             HttpFetch,
  override val cinema: Cinema,
  today:            LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import Cinema1Client._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(MsiBaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val thisMonth = YearMonth.from(today)
    val nextMonth = thisMonth.plusMonths(1)

    val slots = Seq(thisMonth, nextMonth).flatMap { ym =>
      val url  = monthUrl(ym)
      val html = Try(http.get(url)).getOrElse("")
      if (html.isEmpty) Seq.empty
      else MsiScraper.parseMonthWithYear(html, ym, MsiBaseUrl, cleanTitle)
    }

    MsiScraper.toMovies(slots, cinema)
  }
}

object Cinema1Client {

  val MsiBaseUrl = "https://bilety.cinemaone.pl"

  def monthUrl(ym: YearMonth): String =
    s"$MsiBaseUrl/MSI/mvc/pl?sort=Name&date=${ym.getYear}-${"%02d".format(ym.getMonthValue)}"

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
