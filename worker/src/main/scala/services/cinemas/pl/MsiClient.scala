package services.cinemas.pl

import models._
import tools.HttpFetch
import services.cinemas.common.CinemaScraper

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
 * @parameter http    HTTP client (swap for `FakeHttpFetch` in tests).
 * @parameter baseUrl Scheme + host of the venue's MSI portal, no trailing slash
 *                (e.g. `https://bilety.goktychowo.pl`).
 * @parameter cinema  The [[Cinema]] source tag attached to every [[CinemaMovie]].
 * @parameter today   Calendar anchor for computing the two months to fetch;
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
  titlePrefix: Option[String] = None,
  // A few single-venue portals append a constant venue label as a SUFFIX instead
  // ("BACKROOMS BEZ WYJŚCIA NAPISY-KINO WYBRZEŻE", "DRUGIE ŻYCIE-KINO WYBRZEŻE" on
  // bilety.rck.kolobrzeg.pl). When set, the trailing `-<suffix>` is stripped
  // before cleaning — which also re-exposes any format word the suffix had
  // buried (the bare `NAPISY`/`DUBBING` then becomes trailing and the normal
  // format extractor catches it).
  titleSuffix: Option[String] = None
) extends CinemaScraper with OnlyMovieEventsFilter {

  import MsiClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(baseUrl)
  override def sourceUrl: Option[String] = Some(baseUrl)

  private val titleCleaner: String => (String, List[String]) =
    titlePrefix.map(cleanTitleForVenue)
      .orElse(titleSuffix.map(cleanTitleStripSuffix))
      .getOrElse(cleanTitle)

  protected def fetchUnfiltered(): Seq[CinemaMovie] = {
    val thisMonth = YearMonth.from(today)
    val nextMonth = thisMonth.plusMonths(1)

    // Fetch both months, tolerating a per-month failure so one reachable month
    // still contributes its screenings. But if EVERY month fetch fails, the
    // portal itself is down — propagate the error so the scrape surfaces as a
    // failure (red on /uptime) instead of being swallowed into an empty list
    // (white, indistinguishable from a genuinely film-dormant venue). Same guard
    // as KinoAwangarda2Client / KinoPatriaClient.
    val fetched = Seq(thisMonth, nextMonth)
      .map(ym => ym -> Try(http.get(monthUrl(baseUrl, mvcPath, ym))))
    if (fetched.forall(_._2.isFailure)) throw fetched.head._2.failed.get

    val slots = fetched.flatMap { case (ym, html) =>
      html.toOption.filter(_.nonEmpty).toSeq
        .flatMap(MsiScraper.parseMonthWithYear(_, ym, baseUrl, titleCleaner))
    }
    MsiScraper.toMovies(slots, cinema)
  }
}

object MsiClient {

  def monthUrl(baseUrl: String, mvcPath: String, ym: YearMonth): String =
    s"$baseUrl$mvcPath?sort=Name&date=${ym.getYear}-${"%02d".format(ym.getMonthValue)}"

  /** Strip the trailing format/version tag MSI buries in the title, sentence-case
   *  the result (so screening variants of one film merge), AND surface the
   *  version as `Showtime.format` display tokens — MSI is the only source that
   *  hides the dub-vs-subtitles / 2D-vs-3D distinction in the title text, so the
   *  cleaned title alone would lose it. Examples:
   *    "TOY STORY 5 (2D DUBBING)"   → ("Toy story 5", List("2D","DUB"))
   *    "STRASZNY FILM - NAPISY"     → ("Straszny film", List("NAP"))
   *    "DZIEŃ OBJAWIENIA (2D NAPISY DOLBY ATMOS)" → ("Dzień objawienia", List("2D","NAP"))
   *  (The strip+token extraction is shared with the other portal clients via
   *  `ScraperParse.extractFormatTags` — a cross-client concern, not MSI-specific.) */
  private[cinemas] def cleanTitle(raw: String): (String, List[String]) = {
    val (stripped, tokens) = ScraperParse.extractFormatTags(raw)
    (ScraperParse.sentenceCase(stripped), tokens)
  }

  /** Title cleaner for one venue of a two-cinema MSI portal whose titles are
   *  prefixed `<venue> - …` (e.g. "Chemik - Dzień objawienia"). Keeps only the
   *  titles carrying this exact prefix (case-insensitive) and strips it before
   *  the normal clean; returns `("", Nil)` for the other venue's titles so the
   *  scraper drops them. */
  private[cinemas] def cleanTitleForVenue(venue: String)(raw: String): (String, List[String]) = {
    val sep = s"$venue - "
    if (raw.regionMatches(true, 0, sep, 0, sep.length)) cleanTitle(raw.substring(sep.length))
    else ("", Nil)
  }

  /** Title cleaner for a single-venue portal that appends a constant venue label
   *  as a trailing `-<venue>` (e.g. "DRUGIE ŻYCIE-KINO WYBRZEŻE", with or without
   *  spaces around the dash). Strips that suffix first, then runs the normal
   *  clean — so a format word the suffix had buried (e.g. "…NAPISY-KINO WYBRZEŻE")
   *  becomes trailing and `extractFormatTags` recovers it as a version token.
   *  The separator is `[-–—=]`: Kino Wybrzeże is inconsistent and glues the label
   *  on with `=` on some rows ("SUPERGIRL NAPISY=KINO WYBRZEŻE"), which the
   *  dash-only class let through to a no-match key. */
  private[cinemas] def cleanTitleStripSuffix(venue: String)(raw: String): (String, List[String]) = {
    val pat = s"""(?iu)\\s*[-–—=]\\s*${java.util.regex.Pattern.quote(venue)}\\s*$$"""
    cleanTitle(raw.replaceAll(pat, "").trim)
  }
}
