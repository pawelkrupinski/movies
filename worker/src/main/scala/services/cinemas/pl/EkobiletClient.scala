package services.cinemas.pl

import models._
import org.jsoup.nodes.Document
import org.jsoup.Jsoup
import tools.{HttpFetch, HttpRead}
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper, DetailEnricher, DetailFetchOutcome, FilmDetail, ListingPages, ScraperParse}

import java.time.{LocalDate, LocalDateTime}
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
 * off-canvas info panel (`#offcanvasRightInfo .offcanvas-body`). Some venues
 * publish nothing else there (no labelled block, no `Movie` JSON-LD, no OG
 * tags), but others (Kino Rejs in Słupsk; Kino Meduza in Opole on some films)
 * open the panel with a hand-typed metadata paragraph — "Francja, Belgia 2026,
 * 88 min / reżyseria: Philippe Riche" — which [[EkobiletClient.parseDetail]]
 * reads into countries, year, runtime and director. Those are TMDB-identity
 * hints the listing never carries, so resolution waits for the detail (the
 * default `defersTmdbResolution`). The chrono-row skin never sets `filmUrl` at
 * all (its only per-row link is a session's booking URL, not a shared film
 * page), so those rows carry no detail and resolve off their title alone.
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
  today:  => LocalDate
) extends ChunkedCinemaScraper with DetailEnricher {

  import EkobiletClient._


  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  // The venue's public landing page — the same URL fetch() reads its listing from.
  override def sourceUrl: Option[String] = Some(s"$BaseUrl/$slug")

  // Each venue is standalone (no chain), so the dedup/freshness scope is the
  // cinema's own slug. (Two venues never share a film's detail page — the URL is
  // venue-scoped: `ekobilet.pl/<slug>/<film>`.)
  override def detailGroup: String = cinema.slug

  /** Deferred per-film detail — the synopsis (plus, where the venue types one,
   *  the metadata line) off the off-canvas info panel. None on a fetch failure so the task
   *  stays stale and retries rather than recording an empty result as fresh.
   *
   *  A durable 404/410 escapes rather than folding into None, so a page that is
   *  gone for good gets stamped instead of retried every tick — see [[DetailFetchOutcome]]. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.transientToNone(HttpRead.page(http, ref)).map(html => parseDetail(Jsoup.parse(html)))

  // The landing decides the skin. Chrono-row: every showtime is already fully
  // known (title + date/time + booking link) on the ONE landing fetch, so each
  // showtime becomes its own self-contained chunk (no further HTTP call in
  // `fetchChunk`). Card-grid: the listing (landing + dated pages) discovers the
  // films and their detail-page URLs (the PLAN), then each film's detail page
  // yields its showtimes (the per-film CHUNK) — the chunk key there carries
  // `title<US>detailUrl` because the title (tags and all) comes from the listing, not
  // the detail page.
  def planChunks(): Seq[String] = {
    val landing    = HttpRead.page(http, s"$BaseUrl/$slug")
    val chronoRows = parseChronoRows(landing, today)
    if (chronoRows.nonEmpty)
      chronoRows.map { case (title, dateTime, booking) =>
        s"$ChronoMarker$title$KeySep$dateTime$KeySep${booking.getOrElse("")}"
      }
    else {
      // Per-date discovery is best-effort — a failed day just contributes no films —
      // unless every day failed: then the strip is down, and the landing alone (often
      // today's films only, or none) must not read as the venue's whole programme.
      val days = availableDates(landing).map(d => Try(parseLanding(HttpRead.page(http, s"$BaseUrl/$slug?date=$d"))))
      ListingPages.requireAnyReached(days)
      val films = (parseLanding(landing) ++ days.flatMap(_.getOrElse(Nil))).distinctBy(_._2)
      films.map { case (title, url) => s"$title$KeySep$url" }
    }
  }

  /** Either a chrono-row showtime (already fully known — no fetch) or one
   *  card-grid film's detail page → its showtimes. A throw reschedules just this
   *  chunk. */
  def fetchChunk(key: String): Seq[CinemaMovie] =
    if (key.startsWith(ChronoMarker)) {
      val Array(rawTitle, dateTime, booking) = key.stripPrefix(ChronoMarker).split(KeySep.toString, 3)
      Seq(film(rawTitle, None, Seq(Showtime(LocalDateTime.parse(dateTime), Option(booking).filter(_.nonEmpty)))))
    } else {
      val i        = key.indexOf(KeySep)
      val rawTitle = key.substring(0, i)
      val url      = key.substring(i + 1)
      val showtimes = parseShowtimes(HttpRead.page(http, url), today)
      if (showtimes.isEmpty) Seq.empty
      else Seq(film(rawTitle, Some(url), showtimes))
    }

  /** One film's row off its listing title as the venue typed it: the tags peeled
   *  off (see [[EkobiletClient.parseTitle]]), the version stamped on every
   *  showtime and the age kept as the rating. */
  private def film(rawTitle: String, filmUrl: Option[String], showtimes: Seq[Showtime]): CinemaMovie = {
    val parsed = parseTitle(rawTitle)
    CinemaMovie(
      Movie(parsed.title, rawTitle = Some(rawTitle).filter(_ != parsed.title)),
      cinema, None, filmUrl, None, Seq.empty, Seq.empty,
      showtimes.map(_.copy(format = parsed.format)),
      ageRating = parsed.ageRating)
  }

  /** Merge a film's showtimes across its detail URLs (by title), then drop what
   *  isn't a film: rows ticketed as a concert or a play ([[isStageTicket]]) and
   *  the live events [[NonMovieEventClassifier]] names — the same filter the old
   *  `OnlyMovieEventsFilter` mixin applied, moved here so the queue (reduce) path
   *  filters too. */
  override def reduceChunks(chunks: Map[String, Seq[CinemaMovie]]): Seq[CinemaMovie] =
    chunks.toSeq.sortBy(_._1).flatMap(_._2)
      .groupBy(_.movie.title).toSeq.sortBy(_._1)
      .flatMap { case (_, group) =>
        val showtimes = group.flatMap(_.showtimes).distinctBy(s => (s.dateTime, s.bookingUrl)).sortBy(_.dateTime)
        if (showtimes.isEmpty) None else Some(group.head.copy(showtimes = showtimes))
      }
      .filterNot(isStageTicket)
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

  /** A listing title as the venue typed it, whitespace collapsed. The tags stay
   *  on until [[parseTitle]], which needs them for the format and the rating. */
  private def listingTitle(text: String): String = text.replaceAll("\\s+", " ").trim

  /** A film's title with its tags peeled off, the versions they named as
   *  `Showtime.format` tokens, and the age they named as its rating. */
  private[cinemas] case class ListingTitle(title: String, format: List[String], ageRating: Option[String])

  // Trailing pipe segments ekobilet venues tag a film with besides its version:
  // the minimum age ("| 13+") and a premiere flag ("| PREMIERA!!!", "| PREMIERA !!!").
  private val AgeTag      = """\s*\|\s*(\d{1,2}\+)\s*$""".r.unanchored
  private val PremiereTag = """(?iu)\s*\|\s*premiera\s*!*\s*$"""

  /** Kino Starówka and Wielicka Mediateka type every listing as
   *  "<film> | <tag> | <tag>" — "Lalka | 13+ | PREMIERA!!!", "Wtorek z klasyką:
   *  Asterix i Obelix: Misja Kleopatra | DUBBING PL | 10+", "Lalka | 2D | PL" —
   *  which reached TMDB whole and never resolved. Peels the trailing age,
   *  premiere and version segments in whatever order they come, until none is
   *  left; a pipe segment that is none of these ("| cykl WAJDA re-wizje") stays
   *  for the title rules. */
  private[cinemas] def parseTitle(raw: String): ListingTitle = {
    @annotation.tailrec
    def peel(t: String, format: List[String], age: Option[String]): ListingTitle = {
      val (stripped, tokens) = ScraperParse.extractFormatTags(t)
      val withFormat = format ++ tokens.filterNot(format.contains)
      stripped match {
        case AgeTag(a) => peel(AgeTag.replaceFirstIn(stripped, ""), withFormat, age.orElse(Some(a)))
        case _ =>
          val unpremiered = stripped.replaceFirst(PremiereTag, "")
          if (unpremiered != stripped) peel(unpremiered, withFormat, age)
          else ListingTitle(stripped, withFormat, age)
      }
    }
    peel(raw, Nil, None)
  }

  /** A row whose every booking link sells a concert or play ticket rather than a
   *  film one. ekobilet names the ticket kind in the link — "…/630621-bilety-na-
   *  koncert", "…-bilety-na-spektakl" beside "…-bilety-na-film" for every film —
   *  which catches a concert whose title carries no event word ("Grzegorz Turnau").
   *  A screened broadcast ticketed as a concert (Wąsosz's André Rieu) is kept, as
   *  [[NonMovieEventClassifier.isScreenedBroadcast]] keeps it everywhere else. */
  private[cinemas] def isStageTicket(cm: CinemaMovie): Boolean = {
    val links = cm.showtimes.flatMap(_.bookingUrl)
    links.nonEmpty && links.forall(StageTicket.matches) &&
      !NonMovieEventClassifier.isScreenedBroadcast(cm.movie.rawTitle.getOrElse(cm.movie.title))
  }
  private val StageTicket = """.*-bilety-na-(?:koncert|spektakl)$""".r

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

  /** (listing title, detail-page URL) for each film card on the venue landing,
   *  de-duplicated (cards render twice for desktop/mobile). */
  private[cinemas] def parseLanding(html: String): Seq[(String, String)] = {
    Jsoup.parse(html, BaseUrl).select("div.event-card a[href]").asScala.toSeq.flatMap { a =>
      val url = a.attr("abs:href").takeWhile(_ != '?')
      // The card's title is the `p.overme` in its own wrapper. A card without one is
      // dropped: falling back to the page's first title lent it another film's name.
      val titleElement = Option(a.closest("div.event-card")).flatMap(c =>
        Option(c.parent).flatMap(p => Option(p.selectFirst("p.overme"))))
      for {
        t <- titleElement.map(e => listingTitle(e.text)).filter(_.nonEmpty)
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
        // This year's date unless it lies past the grace before `today`: "5 sty" seen
        // in December is next January, while a row left on the page the day after it
        // screened stays in the past instead of becoming a phantom a year out.
        date     <- ScraperParse.upcomingDate(dayMonth, today)
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
        title = listingTitle(titleElement.text)
        if title.nonEmpty
        dateStr  <- Option(row.selectFirst("strong.primary-color")).map(_.text.trim)
        dayMonth <- ScraperParse.parseDayMonth(dateStr)  // "25 wrz"
        time     <- Option(row.selectFirst("span.fw-bold")).flatMap(s => ScraperParse.parseHHmm(s.text))
        date     <- ScraperParse.upcomingDate(dayMonth, today)
      } yield (title, date.atTime(time), Option(row.attr("data-href")).filter(_.nonEmpty))
    }.distinctBy(identity)

  /** Parse a film detail page into its `FilmDetail`, off the off-canvas info
   *  panel (`#offcanvasRightInfo .offcanvas-body`). Its `<p>`s are the film's
   *  own; the venue's "about the cinema" blurb sits after a `.line` divider,
   *  outside the body. Usually the only `<p>` is the synopsis. Some venues put
   *  a metadata paragraph first ("Francja, Belgia 2026, 88 min" then
   *  "reżyseria: Philippe Riche"), which becomes countries, year, runtime and
   *  director; the synopsis is then the first paragraph that is NOT that line. */
  private[cinemas] def parseDetail(document: Document): FilmDetail = {
    val paragraphs = document.select("#offcanvasRightInfo .offcanvas-body p").asScala.toSeq
      .map(_.text.trim).filter(_.nonEmpty)
    val metadata = paragraphs.collectFirst(Function.unlift(MetadataLine.findPrefixMatchOf))
    FilmDetail(
      synopsis       = paragraphs.find(p => MetadataLine.findPrefixMatchOf(p).isEmpty),
      director       = metadata.flatMap(m => Option(m.group(DirectorGroup))).toSeq
                         .flatMap(_.split(',')).map(_.trim).filter(_.nonEmpty),
      runtimeMinutes = metadata.flatMap(m => Option(m.group(RuntimeGroup))).flatMap(_.toIntOption),
      releaseYear    = metadata.flatMap(m => m.group(YearGroup).toIntOption),
      countries      = metadata.toSeq.flatMap(_.group(CountriesGroup).split(',')).map(_.trim).filter(_.nonEmpty)
    )
  }

  private val CountriesGroup = "countries"
  private val YearGroup      = "year"
  private val RuntimeGroup   = "runtime"
  private val DirectorGroup  = "director"

  /** The metadata paragraph's text: "<countries> <year>[, <N> min][ reżyseria: <names>]",
   *  e.g. "Francja, Belgia 2026, 88 min reżyseria: Philippe Riche". Anchored at
   *  the paragraph's start and requiring a letters-only country list before the
   *  year, so a synopsis that merely mentions a year never matches. */
  private val MetadataLine =
    raw"""(?i)^(?<$CountriesGroup>\p{L}[\p{L} ,.-]*?)\s+(?<$YearGroup>\d{4})(?:\s*,\s*(?<$RuntimeGroup>\d+)\s*min\.?)?(?:\s*reżyseria:\s*(?<$DirectorGroup>.+))?$$""".r
}
