package services.cinemas.pl

import java.util.Locale

import services.cinemas.common.ScraperParse
import services.movies.FormatTags
import models._
import tools.{HttpFetch, HttpRead, ReadOutcome}
import services.cinemas.common.{CinemaScraper, ListingPages, ScrapeHorizon}

import java.time.{LocalDate, YearMonth}
import play.api.Logging
import play.api.libs.json.{JsArray, JsObject, JsValue}
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
 * sentence case so it merges correctly with TMDB and other sources. A version
 * followed by a comma and the distributor (`LALKA 2D PL, DYS. KINO ŚWIAT`) is
 * cut at the format word the same way.
 *
 * === Event labels ===
 * The portal also sells its venue's plays and concerts, often under a title
 * that names no stage word (GOK Tychowo's "Bonobo", Laurent Baffie's comedy).
 * What the box office filed each event under — a category ("Spektakl
 * Teatralny", "koncert", "Teatr") and a sub-category (a genre for a film:
 * "Dramat", "Animacja") — is served by one JSON feed per portal,
 * `<base><mvcPath>/Repertoire/GetShortEventsWithFilters` (the data behind its
 * filter panel), keyed by the booking link's `event_id`. One extra request per
 * scrape; a screening filed under a stage label is dropped
 * ([[MsiClient.isStagedEvent]]). A feed that fails or isn't JSON (Max Kino's
 * older skin) labels nothing, so the scrape degrades to the title filter alone.
 * (The per-event details modal, `ajax.aspx/GetEventDetails`, answers HTTP 500 to
 * any request from outside the page, session cookie or not — 2026-10-13.)
 *
 * @parameter http    HTTP client (swap for `FakeHttpFetch` in tests).
 * @parameter baseUrl Scheme + host of the venue's MSI portal, no trailing slash
 *                (e.g. `https://bilety.goktychowo.pl`). The scheme is load-bearing
 *                twice over: it is what we fetch, and `MsiScraper` resolves every
 *                showtime's `abs:href` booking link against it. Three venues are
 *                wired `http://` rather than `https://` because their leaf
 *                certificate expired and was never renewed — RCK Kołobrzeg
 *                (Kino Wybrzeże, `*.rck.kolobrzeg.pl`, 2026-07-18), Kozienicki
 *                Dom Kultury (`*.dkkozienice.pl`, 2026-08-07), and Nowe Kino
 *                Warszawa (`*.mck-gostynin.pl`, 2026-09-08). In all three cases
 *                the handshake fails validation while plain HTTP serves the
 *                identical page and does not redirect, so an expired certificate
 *                is a *scheme* problem here, not a reachability one.
 * @parameter cinema  The [[Cinema]] source tag attached to every [[CinemaMovie]].
 * @parameter today   Calendar anchor for computing the two months to fetch;
 *                defaults to the current Warsaw clock date.
 */
class MsiClient(
  http:    HttpFetch,
  baseUrl: String,
  override val cinema: Cinema,
  today:   => LocalDate,
  // The month-page route. Almost every venue serves it at `/MSI/mvc/pl`, but a
  // few VisualTicket installs expose the identical page under a different prefix
  // (e.g. Kino Planeta Brzesko at `/Rezerwacja/mvc/pl`); override for those.
  mvcPath: String = MsiClient.DefaultMvcPath,
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
  titleSuffix: Option[String] = None,
  // A few portals stamp a label on their FILM rows only ("FILM: LALKA" on
  // bilety.scksieradz.pl) and leave a classic or a broadcast unlabelled. When
  // set, a leading `<label>:` is stripped where present and no row is dropped —
  // see [[MsiClient.cleanTitleStripLabel]].
  filmLabel: Option[String] = None,
  // A few portals sell a SECOND venue's screenings under the same month pages
  // and tell them apart only by the event-labels feed's "Lokalizacja" filter
  // (bilety.charlie.pl carries Kino Spójnia's programme, filed under "Kino
  // Spójna", beside Kino Charlie's own unlabelled events). The month HTML names
  // no venue, so when set, a screening filed under one of these location labels
  // is dropped — and a feed that cannot be read FAILS the scrape, since the
  // portal then cannot be split and listing the other venue's films here would
  // be wrong rather than merely incomplete.
  otherVenueLocations: Set[String] = Set.empty
) extends CinemaScraper with OnlyMovieEventsFilter with Logging {

  import MsiClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(baseUrl)
  override def sourceUrl: Option[String] = Some(baseUrl)
  // Two venues can share one portal, split by the title prefix (see `titlePrefix`).
  override def sourceKey: Option[String] =
    Some(CinemaScraper.urlKey(baseUrl) + titlePrefix.fold("")(p => s"#$p"))

  private val titleCleaner: String => (String, List[String]) =
    titlePrefix.map(cleanTitleForVenue)
      .orElse(titleSuffix.map(cleanTitleStripSuffix))
      .orElse(filmLabel.map(cleanTitleStripLabel))
      .getOrElse(cleanTitle)

  protected def fetchUnfiltered(): Seq[CinemaMovie] = {
    // Walk forward a month at a time instead of fetching a fixed two. MSI venues
    // usually publish only 1–2 months ahead, but nothing in the portal promises
    // that, and a hard two-month window silently hides every screening past it —
    // the exact horizon cap `ScrapeHorizon` exists to forbid ("we want ALL future
    // screenings"). The typical venue still costs three or four requests rather
    // than twenty-four. See [[ScrapeHorizon.liveMonths]] for the stop rule, and
    // why a month that FAILS to fetch counts as blank.
    val attempts = Seq.newBuilder[Try[String]]
    val slots    = Seq.newBuilder[MsiScraper.RawSlot]

    ScrapeHorizon.liveMonths(YearMonth.from(today)) { month =>
      val body = Try(HttpRead.page(http, monthUrl(baseUrl, mvcPath, month)))
      attempts += body
      val monthSlots = body.toOption.filter(_.nonEmpty).toSeq
        .flatMap(MsiScraper.parseMonthWithYear(_, month, baseUrl, titleCleaner))
      slots ++= monthSlots
      monthSlots.nonEmpty
    }

    // Tolerate a per-month failure so one reachable month still contributes its
    // screenings. But if EVERY month we attempted failed, the portal itself is
    // down — propagate the error so the scrape surfaces as a failure (red on
    // /uptime) instead of being swallowed into an empty list (white,
    // indistinguishable from a genuinely film-dormant venue). Same guard as
    // KinoAwangarda2Client / KinoPatriaClient.
    ListingPages.requireAnyReached(attempts.result())

    val found  = slots.result()
    // Only a portal with screenings is asked for their labels, so a dormant venue
    // still costs just its month walk.
    val labels: EventLabels =
      if (found.isEmpty) EventLabels.Empty
      else HttpRead.jsonObject(http, eventLabelsUrl(baseUrl, mvcPath))(feed => ReadOutcome.Answered(parseEventLabels(feed))) match {
        case ReadOutcome.Answered(byEvent) => byEvent
        case unread if otherVenueLocations.nonEmpty =>
          throw new IllegalStateException(
            s"${cinema.displayName}: no event labels, so the shared portal cannot be split from ${otherVenueLocations.mkString(", ")}: ${unread.explain}")
        // Unread labels leave every screening's kind unknown, and an unknown kind
        // is kept — what the scrape did before it read labels — since dropping a
        // film costs more than listing a play. Logged, so a portal that stops
        // serving the feed is seen rather than silently unfiltered.
        case unread =>
          logger.info(s"${cinema.displayName}: no event labels, so only titles filter its stage events: ${unread.explain}")
          EventLabels.Empty
      }
    val ownSlots = found.filterNot(slot => slot.eventId.exists(id => labels.locations.getOrElse(id, Nil).exists(otherVenueLocations)))
    MsiScraper.toMovies(ownSlots.filterNot(slot => isStagedSlot(slot, labels.kinds)), cinema)
  }
}

object MsiClient {

  /** The month-page route almost every install serves; the constructor's default,
    * named so the catalog's per-venue table can spell "no override" without a
    * second copy of the path. */
  val DefaultMvcPath = "/MSI/mvc/pl"

  def monthUrl(baseUrl: String, mvcPath: String, ym: YearMonth): String =
    s"$baseUrl$mvcPath?sort=Name&date=${ym.getYear}-${"%02d".format(ym.getMonthValue)}"

  /** The portal's event-labels feed: every upcoming event's filter-panel labels. */
  def eventLabelsUrl(baseUrl: String, mvcPath: String): String =
    s"$baseUrl$mvcPath/Repertoire/GetShortEventsWithFilters"

  /** The labels a box office filed each event under — its category and
   *  sub-category names, by event id — read off the event-labels feed
   *  (`{"filtersForEvent":[{"eventId":10959,"filters":[{"filterName":
   *  "category-filter","filterItems":[{"name":"Spektakl Teatralny"}]},…]}]}`).
   *  An event missing a field simply carries fewer labels. */
  private[cinemas] def parseEventLabels(feed: JsObject): EventLabels = {
    def array(value: play.api.libs.json.JsLookupResult): Seq[JsValue] = value.asOpt[JsArray].toSeq.flatMap(_.value)
    def labelsOf(event: JsValue, filterNames: Set[String]): Seq[String] =
      array(event \ "filters")
        .filter(filter => (filter \ "filterName").asOpt[String].exists(filterNames.contains))
        .flatMap(filter => array(filter \ "filterItems"))
        .flatMap(item => (item \ "name").asOpt[String])
    val events = array(feed \ "filtersForEvent").flatMap(event => (event \ "eventId").asOpt[Long].map(_ -> event))
    EventLabels(
      kinds     = events.map { case (id, event) => id -> labelsOf(event, KindFilters) }.toMap,
      locations = events.map { case (id, event) => id -> labelsOf(event, Set(LocationFilter)).filter(_.nonEmpty) }.toMap)
  }

  /** What the event-labels feed says of each event, by event id: its `kinds`
   *  (category and sub-category, read by [[isStagedEvent]]) and its `locations`
   *  (the portal's "Lokalizacja" — empty for a single-venue portal). */
  private[cinemas] case class EventLabels(kinds: Map[Long, Seq[String]], locations: Map[Long, Seq[String]])
  private[cinemas] object EventLabels { val Empty: EventLabels = EventLabels(Map.empty, Map.empty) }

  /** The two filters that say what an event is; the others are its audience, price and location. */
  private val KindFilters    = Set("category-filter", "event-details-type-filter")
  private val LocationFilter = "localization-group-filter"

  /** Whether an event filed under `labels` is performed on the venue's stage
   *  rather than screened: a label carrying the stage vocabulary
   *  ([[NonMovieEventClassifier.isPerformance]] — "Spektakl Teatralny", "Teatr",
   *  "koncert", "Stand Up"), unless the title names a screened broadcast
   *  ("RETRANSMISJA ANDRE RIEU", filed as a concert). A label that is only a genre
   *  or the portal's own section ("Dramat", "MARATON FILMOWY", "Kino") names no
   *  stage, and neither does "Opera", which is how the portals file Met
   *  transmissions as well. */
  def isStagedEvent(labels: Seq[String], title: String): Boolean =
    labels.exists(label => NonMovieEventClassifier.isPerformance(label)) &&
      !NonMovieEventClassifier.isScreenedBroadcast(title)

  /** [[isStagedEvent]] for one screening — unless its Description states a
   *  film's record (a director AND a year), the veto the title filter grants
   *  too: a concert FILM filed as "Koncert" is still a film. */
  private def isStagedSlot(slot: MsiScraper.RawSlot, labels: Map[Long, Seq[String]]): Boolean =
    slot.eventId.flatMap(labels.get).exists(isStagedEvent(_, slot.rawTitle)) &&
      !(slot.meta.director.nonEmpty && slot.meta.releaseYear.isDefined)

  /** Strip the trailing format/version tag MSI buries in the title, sentence-case
   *  the result (so screening variants of one film merge), AND surface the
   *  version as `Showtime.format` display tokens — MSI is the only source that
   *  hides the dub-vs-subtitles / 2D-vs-3D distinction in the title text, so the
   *  cleaned title alone would lose it. Examples:
   *    "TOY STORY 5 (2D DUBBING)"   → ("Toy story 5", List("2D","DUB"))
   *    "STRASZNY FILM - NAPISY"     → ("Straszny film", List("NAP"))
   *    "DZIEŃ OBJAWIENIA (2D NAPISY DOLBY ATMOS)" → ("Dzień objawienia", List("2D","NAP"))
   *  (The strip+token extraction is shared with the other portal clients via
   *  `FormatTags.extractFormatTags` — a cross-client concern, not MSI-specific.) */
  private[cinemas] def cleanTitle(raw: String): (String, List[String]) = {
    val (film, tailTokens) = DistributorTail.findFirstMatchIn(raw) match {
      case Some(m) => (raw.substring(0, m.start), (m.group(1) +: Option(m.group(2)).orElse(Option(m.group(3))).toSeq).flatMap(versionToken).toList)
      case None    => (raw, Nil)
    }
    val (stripped, tokens) = FormatTags.extractFormatTags(film)
    (ScraperParse.sentenceCase(stripped), (tokens ++ tailTokens).distinct)
  }

  /** Kino Górnik (Łęczyca) types the version and then the film's DISTRIBUTOR
   *  after a comma: "Avengers: Koniec gry 2D Dubbing, Dys. Disney", "Luna i
   *  rozgadana świnka 2D Dubbing, Kino Świat", "Zapopmniana wyspa-2D  dubbind,
   *  dystybutor". Anchored on the screen format (2D/3D), an optional version
   *  word, then the comma and a comma-free tail to the end — so a title's own
   *  comma ("Powiedz mi, co czujesz") is never cut, and a mistyped version word
   *  ("dubbind") is only tolerated in that slot. An abbreviated version's full
   *  stop stands for the comma ("Baranek Shaun i kudłata bestia 2D Dubb. Kino
   *  Świat", 2026-10-12) — only behind a version word, so a format closing a
   *  sentence of the title is left alone. */
  private val DistributorTail =
    """(?iu)[\s-]+([23]d)(?:(?:\s+(dubb\p{L}*|napis\p{L}*|lektor|pl))?\s*,|\s+(dubb\p{L}*|napis\p{L}*|lektor)\.\s+(?=\S))[^,]*$""".r

  /** The display token a word of [[DistributorTail]] names — the screen format as
   *  typed, the version by its stem (so "dubbind" still reads as a dub). */
  private def versionToken(word: String): Option[String] = word.toLowerCase(Locale.ROOT) match {
    case w if w.startsWith("dubb")  => Some("DUB")
    case w if w.startsWith("napis") => Some("NAP")
    case "lektor"                   => Some("LEK")
    case "pl"                       => None
    case w                          => Some(w.toUpperCase(Locale.ROOT))
  }

  /** Title cleaner for a portal that stamps a label on its FILM rows only — Kino
   *  Teatr Sieradz writes "FILM: LALKA" but "KLASYKA KINA: ORLANDO" and an
   *  unlabelled André Rieu broadcast, both screenings too. So unlike
   *  [[cleanTitleForVenue]] this never drops a row: the leading `<label>:` is
   *  stripped where present and every title then gets the normal clean. */
  private[cinemas] def cleanTitleStripLabel(label: String)(raw: String): (String, List[String]) = {
    val pat = s"""(?iu)^\\s*${java.util.regex.Pattern.quote(label)}\\s*:\\s*"""
    cleanTitle(raw.replaceFirst(pat, ""))
  }

  /** Title cleaner for one venue of a two-cinema MSI portal whose titles are
   *  prefixed `<venue> - …` (e.g. "Chemik - Dzień objawienia"). Keeps only the
   *  titles carrying this prefix (case-insensitive) and strips it before the
   *  normal clean; returns `("", Nil)` for the other venue's titles so the
   *  scraper drops them.
   *
   *  The separator is whatever punctuation the box office typed that week, not a
   *  contract. bilety.mok.com.pl spelled it `Chemik - …` until August 2026 and
   *  `Chemik-…` after — with a stray dot on either side of the dash on some rows
   *  (`.Chemik-.Psi Patrol i dinozaury`) — while the OTHER venue on the same page
   *  kept `TWIERDZA - …`. Matching the spaced form literally therefore dropped
   *  every Chemik row while Twierdza kept parsing off the same fetch, i.e. a
   *  white bar on /uptime that looked exactly like a film-dormant venue. So the
   *  dash is required (it is what separates venue from title) but the spaces and
   *  the surrounding dots are not. */
  private[cinemas] def cleanTitleForVenue(venue: String)(raw: String): (String, List[String]) = {
    val pat = s"""(?iu)^[\\s.]*${java.util.regex.Pattern.quote(venue)}\\s*[-–—]\\s*[.]*\\s*""".r
    pat.findPrefixMatchOf(raw)
      .map(m => cleanTitle(raw.substring(m.end)))
      .getOrElse(("", Nil))
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
