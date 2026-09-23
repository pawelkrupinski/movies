package services.cinemas.pl

import services.cinemas.common.ScraperParse
import models._
import org.jsoup.nodes.Document
import org.jsoup.Jsoup
import tools.HttpFetch
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper, DetailEnricher, DetailFetchOutcome, FilmDetail}

import java.time.{LocalDate, LocalDateTime, Period, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Generic client for cinemas ticketed through ekobilet.pl. The venue landing
 * `ekobilet.pl/<slug>` renders in one of two skins:
 *
 *   1. The card-grid skin — server-rendered but only the *currently selected*
 *      day's films (today) — so on a day the venue is dark it shows "Brak
 *      wydarzeń na dzisiaj" and zero `div.event-card`s. The date strip at the
 *      top lists every upcoming day, each `div.card-date[data-date="DD.MM.YYYY"]`;
 *      days that actually screen carry `available-color` (clickable), dark/past
 *      days carry `pointer-events-none`. Re-requesting the landing with
 *      `?date=YYYY-MM-DD` renders that day's `div.event-card a[href]` cards
 *      (title in a sibling `p.overme`), so we sweep every available day to
 *      discover the full repertoire rather than just today's. Each card links to
 *      a film DETAIL page carrying its dated screenings (all of them, not
 *      date-scoped): one `div.event-buy[data-href]` per slot, with
 *      `strong.primary-color` = "DD <pl-mon abbrev>" (e.g. "10 cze") and a
 *      `span.fw-bold` = "HH:MM …". The year isn't in the row, so it's inferred
 *      from `today` (next occurrence of that month/day).
 *   2. The chrono-row skin (Wąsosz/Milejów/Opole Lubelskie, among others) — the
 *      bare landing itself lists every upcoming SHOWTIME as a flat
 *      `div.event-buy[data-href]` row spanning many days at once, with the
 *      title inline (`div.ps-2.primary-color.fw-bold`) alongside the SAME
 *      date/time markup the detail-page skin uses. No separate per-film detail
 *      page exists for these venues, so there's nothing to enrich a synopsis
 *      from and no date-strip sweep is needed — one fetch has the lot.
 *      [[parseChronoRows]] recognises this skin by the presence of that inline
 *      title div (a detail page's own `event-buy` rows never carry one, since a
 *      detail page is already scoped to a single film).
 *
 * The card-grid skin's detail page also carries a plain-Polish synopsis in an
 * off-canvas info panel (`#offcanvasRightInfo .offcanvas-body`) — and that is
 * the ONLY film-level metadata ekobilet exposes there: no production year,
 * director, cast, country, genre or runtime anywhere on the page (verified
 * across venues: no labelled block, no `Movie` JSON-LD, no OG tags). So the
 * deferred [[fetchFilmDetail]] supplies a synopsis only — pure display
 * enrichment with no TMDB-identity hints — which is why `defersTmdbResolution`
 * is overridden to false: the row resolves immediately off its listing title
 * rather than waiting for a detail that can't disambiguate it. (Resolution
 * disambiguation for yearless arthouse titles still has to come from
 * TMDB/IMDb, not this source.) The chrono-row skin never sets `filmUrl` at all
 * (its only per-row link is a session's booking URL, not a shared film page),
 * so those rows simply carry no synopsis.
 *
 * One instance per venue, captured by its `slug` + `cinema` (OCP). Fetches: the
 * landing (chrono-row: done) or the landing + one page per available day + each
 * film's detail page once, deduped, in parallel (card-grid). Previously scraped
 * from Filmweb.
 */
class EkobiletClient(
  http:   HttpFetch,
  slug:   String,
  override val cinema: Cinema,
  today:  LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends ChunkedCinemaScraper with DetailEnricher {

  import EkobiletClient._


  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  // The venue's public landing page — the same URL fetch() reads its listing from.
  override def sourceUrl: Option[String] = Some(s"$BaseUrl/$slug")

  // Each venue is standalone (no chain), so the dedup/freshness scope is the
  // cinema's own slug. (Two venues never share a film's detail page — the URL is
  // venue-scoped: `ekobilet.pl/<slug>/<film>`.)
  override def detailGroup: String = cinema.slug

  // The synopsis is display-only and supplies no TMDB-identity hint, so the row
  // resolves straight off its listing title and the synopsis merges in later.
  override def defersTmdbResolution: Boolean = false

  /** Deferred per-film detail — the synopsis off the off-canvas info panel, the
   *  only film-level field ekobilet exposes. None on a fetch failure so the task
   *  stays stale and retries rather than recording an empty result as fresh.
   *
   *  A durable 404/410 escapes rather than folding into None, so a page that is
   *  gone for good gets stamped instead of retried every tick — see [[DetailFetchOutcome]]. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.transientToNone(http.get(ref)).map(html => parseDetail(Jsoup.parse(html)))

  // The landing decides the skin. Chrono-row: every showtime is already fully
  // known (title + date/time + booking link) on the ONE landing fetch, so each
  // showtime becomes its own self-contained chunk (no further HTTP call in
  // `fetchChunk`). Card-grid: the listing (landing + dated pages) discovers the
  // films and their detail-page URLs (the PLAN), then each film's detail page
  // yields its showtimes (the per-film CHUNK) — the chunk key there carries
  // `title<US>detailUrl` because the cleaned title comes from the listing, not
  // the detail page.
  def planChunks(): Seq[String] = {
    val landing    = http.get(s"$BaseUrl/$slug")
    val chronoRows = parseChronoRows(landing, today)
    if (chronoRows.nonEmpty)
      chronoRows.map { case (title, dateTime, booking) =>
        s"$ChronoMarker$title$KeySep$dateTime$KeySep${booking.getOrElse("")}"
      }
    else {
      val dates = availableDates(landing)
      // Per-date discovery is best-effort (a failed day just contributes no films),
      // matching the old swallow-and-continue; the landing fetch is essential.
      val films = (parseLanding(landing) ++ dates.flatMap(d =>
        Try(http.get(s"$BaseUrl/$slug?date=$d")).toOption.map(parseLanding).getOrElse(Nil)))
        .distinctBy(_._2)
      films.map { case (title, url) => s"$title$KeySep$url" }
    }
  }

  /** Either a chrono-row showtime (already fully known — no fetch) or one
   *  card-grid film's detail page → its showtimes. A throw reschedules just this
   *  chunk. */
  def fetchChunk(key: String): Seq[CinemaMovie] =
    if (key.startsWith(ChronoMarker)) {
      val Array(title, dateTime, booking) = key.stripPrefix(ChronoMarker).split(KeySep.toString, 3)
      Seq(CinemaMovie(Movie(title), cinema, None, None, None, Seq.empty, Seq.empty,
        Seq(Showtime(LocalDateTime.parse(dateTime), Option(booking).filter(_.nonEmpty)))))
    } else {
      val i     = key.indexOf(KeySep)
      val title = key.substring(0, i)
      val url   = key.substring(i + 1)
      val showtimes = parseShowtimes(http.get(url), today)
      if (showtimes.isEmpty) Seq.empty
      else Seq(CinemaMovie(Movie(title), cinema, None, Some(url), None, Seq.empty, Seq.empty, showtimes))
    }

  /** Merge a film's showtimes across its detail URLs (by title), then drop
   *  non-film live events — the same filter the old `OnlyMovieEventsFilter` mixin
   *  applied, moved here so the queue (reduce) path filters too. */
  override def reduceChunks(chunks: Map[String, Seq[CinemaMovie]]): Seq[CinemaMovie] =
    chunks.toSeq.sortBy(_._1).flatMap(_._2)
      .groupBy(_.movie.title).toSeq.sortBy(_._1)
      .flatMap { case (_, group) =>
        val showtimes = group.flatMap(_.showtimes).distinctBy(s => (s.dateTime, s.bookingUrl)).sortBy(_.dateTime)
        if (showtimes.isEmpty) None else Some(group.head.copy(showtimes = showtimes))
      }
      .filterNot(cm => NonMovieEventClassifier.isLiveEvent(cm.movie.title))
}

object EkobiletClient {

  val BaseUrl = "https://ekobilet.pl"

  /** Separator packing `title` + `detailUrl` (card-grid) or `title` + `dateTime`
   *  + `bookingUrl` (chrono-row) into one chunk key. A unit separator never
   *  appears in a title or URL. */
  private val KeySep = '\u001F'

  /** Prefixes a chrono-row chunk key so `fetchChunk` can tell it apart from a
   *  card-grid `title$KeySep$detailUrl` key without a fetch. A start-of-text
   *  control character never appears in a real title. */
  private val ChronoMarker = "\u0002"

  // "DD.MM.YYYY" — the date strip's `data-date` attribute.
  private val PickerDate = """(\d{2})\.(\d{2})\.(\d{4})""".r

  /** Upcoming days the venue actually screens on — the date-strip cards marked
   *  `available-color` (clickable). Past/closed/eventless days carry
   *  `pointer-events-none` instead. Returned as `yyyy-MM-dd` for the `?date=`
   *  query, de-duplicated. */
  private[cinemas] def availableDates(html: String): Seq[String] =
    Jsoup.parse(html, BaseUrl).select("div.card-date.available-color[data-date]").asScala.toSeq.flatMap { element =>
      element.attr("data-date") match {
        case PickerDate(d, m, y) => Some(s"$y-$m-$d")
        case _                   => None
      }
    }.distinct

  /** (cleaned title, detail-page URL) for each film card on the venue landing,
   *  de-duplicated (cards render twice for desktop/mobile). */
  private[cinemas] def parseLanding(html: String): Seq[(String, String)] = {
    val document = Jsoup.parse(html, BaseUrl)
    document.select("div.event-card a[href]").asScala.toSeq.flatMap { a =>
      val url = a.attr("abs:href").takeWhile(_ != '?')
      // The card's title is the nearest following `p.overme`.
      val titleElement = Option(a.closest("div.event-card")).flatMap(c =>
        Option(c.parent).flatMap(p => Option(p.selectFirst("p.overme"))))
        .orElse(Option(document.selectFirst("p.overme")))
      for {
        t <- titleElement.map(e => ScraperParse.stripFormatTags(e.text)).filter(_.nonEmpty)
        if url.nonEmpty
      } yield (t, url)
    }.distinctBy(_._2)
  }

  /** Dated screenings off a film detail page. */
  private[cinemas] def parseShowtimes(html: String, today: LocalDate): Seq[Showtime] =
    Jsoup.parse(html, BaseUrl).select("div.event-buy[data-href]").asScala.toSeq.flatMap { row =>
      for {
        dateStr <- Option(row.selectFirst("strong.primary-color")).map(_.text.trim)
        dayMonth <- ScraperParse.parseDayMonth(dateStr)  // "10 cze"
        time     <- Option(row.selectFirst("span.fw-bold")).flatMap(s => ScraperParse.parseHHmm(s.text))
        // The next date with that month/day on or after `today`, so a December
        // listing seen in January resolves to this year, not last.
        date     <- ScraperParse.upcomingDate(dayMonth, today, grace = Period.ZERO)
      } yield Showtime(date.atTime(time), Option(row.attr("data-href")).filter(_.nonEmpty))
    }.distinctBy(s => (s.dateTime, s.bookingUrl))

  /** The chrono-row landing skin: every upcoming showtime as a flat
   *  `div.event-buy[data-href]` row carrying its OWN title
   *  (`div.ps-2.primary-color.fw-bold`) alongside the same date/time markup
   *  [[parseShowtimes]] reads off a detail page — a detail-page row never carries
   *  that title div (it's already scoped to one film), which is how this tells
   *  the two skins apart. Empty when the landing is the card-grid skin instead.
   *  Deduplicated for the desktop/mobile double-render. */
  private[cinemas] def parseChronoRows(html: String, today: LocalDate): Seq[(String, LocalDateTime, Option[String])] =
    Jsoup.parse(html, BaseUrl).select("div.event-buy[data-href]").asScala.toSeq.flatMap { row =>
      for {
        titleElement <- Option(row.selectFirst("div.ps-2.primary-color.fw-bold"))
        title = ScraperParse.stripFormatTags(titleElement.text).trim
        if title.nonEmpty
        dateStr  <- Option(row.selectFirst("strong.primary-color")).map(_.text.trim)
        dayMonth <- ScraperParse.parseDayMonth(dateStr)  // "25 wrz"
        time     <- Option(row.selectFirst("span.fw-bold")).flatMap(s => ScraperParse.parseHHmm(s.text))
        date     <- ScraperParse.upcomingDate(dayMonth, today, grace = Period.ZERO)
      } yield (title, date.atTime(time), Option(row.attr("data-href")).filter(_.nonEmpty))
    }.distinctBy(identity)

  /** Parse a film detail page into its (synopsis-only) `FilmDetail`. The synopsis
   *  is the prose paragraph in the off-canvas info panel
   *  (`#offcanvasRightInfo .offcanvas-body p`); that panel's body also embeds the
   *  venue's own boilerplate "about the cinema" blurb after a `.line` divider, so
   *  read only the first `<p>` (the film synopsis) rather than the whole body.
   *  No other film-level field (year, director, cast, country, genre, runtime)
   *  exists on the page, so this is all the detail there is. */
  private[cinemas] def parseDetail(document: Document): FilmDetail =
    FilmDetail(
      synopsis = Option(document.selectFirst("#offcanvasRightInfo .offcanvas-body p"))
        .map(_.text.trim).filter(_.nonEmpty)
    )
}
