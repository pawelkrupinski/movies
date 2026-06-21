package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import tools.{CachingDetailFetch, HttpFetch, ParallelDetailFetch}

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Generic client for cinemas ticketed through ekobilet.pl. The venue landing
 * `ekobilet.pl/<slug>` is server-rendered but renders only the *currently
 * selected* day's films (today) — so on a day the venue is dark it shows
 * "Brak wydarzeń na dzisiaj" and zero `div.event-card`s. The date strip at the
 * top lists every upcoming day, each `div.card-date[data-date="DD.MM.YYYY"]`;
 * days that actually screen carry `available-color` (clickable), dark/past days
 * carry `pointer-events-none`. Re-requesting the landing with `?date=YYYY-MM-DD`
 * renders that day's `div.event-card a[href]` cards (title in a sibling
 * `p.overme`), so we sweep every available day to discover the full repertoire
 * rather than just today's.
 *
 * The film detail page carries the dated screenings (all of them, not
 * date-scoped): one `div.event-buy[data-href]` per slot, with
 * `strong.primary-color` = "DD <pl-mon abbrev>" (e.g. "10 cze") and a
 * `span.fw-bold` = "HH:MM …". The year isn't in the row, so it's inferred from
 * `today` (next occurrence of that month/day).
 *
 * The detail page also carries a plain-Polish synopsis in an off-canvas info
 * panel (`#offcanvasRightInfo .offcanvas-body`) — and that is the ONLY
 * film-level metadata ekobilet exposes: there is no production year, director,
 * cast, country, genre or runtime anywhere on the page (verified across venues:
 * no labelled block, no `Movie` JSON-LD, no OG tags). So the deferred
 * [[fetchFilmDetail]] supplies a synopsis only — pure display enrichment with no
 * TMDB-identity hints — which is why `defersTmdbResolution` is overridden to
 * false: the row resolves immediately off its listing title rather than waiting
 * for a detail that can't disambiguate it. (Resolution disambiguation for
 * yearless arthouse titles still has to come from TMDB/IMDb, not this source.)
 *
 * One instance per venue, captured by its `slug` + `cinema` (OCP). Fetches: the
 * landing + one page per available day (to find every film), then each film's
 * detail page once (deduped) in parallel. Previously scraped from Filmweb.
 */
class EkobiletClient(
  http:   HttpFetch,
  slug:   String,
  override val cinema: Cinema,
  today:  LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper with DetailEnricher with OnlyMovieEventsFilter {

  import EkobiletClient._

  // Detail pages are static across passes for a live film, so cache them.
  private val detailHttp = new CachingDetailFetch(http)

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
   *  stays stale and retries rather than recording an empty result as fresh. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    Try(detailHttp.get(ref)).toOption.map(html => parseDetail(Jsoup.parse(html)))

  protected def fetchUnfiltered(): Seq[CinemaMovie] = {
    val landing = http.get(s"$BaseUrl/$slug")
    val dates   = availableDates(landing)

    // Each `?date=` page renders only that day's film cards, so sweep every
    // upcoming day the strip marks as screening. The bare landing covers today.
    val byDate = ParallelDetailFetch.keyed("ekobilet-day", dates, 1.minute)(
      date => s"$BaseUrl/$slug?date=$date"
    )(url => Try(http.get(url)).toOption.map(parseLanding))

    // (cleaned title, detail-page URL) across today's landing + every dated
    // listing, deduped by detail URL (a film recurs across days).
    val films = (parseLanding(landing) ++ dates.flatMap(d => byDate.get(d).flatten.getOrElse(Nil)))
      .distinctBy(_._2)
    val urls  = films.map(_._2).distinct

    val byUrl = ParallelDetailFetch("ekobilet", urls, 1.minute) { url =>
      Try(http.get(url)).toOption.map(html => parseShowtimes(html, today))
    }

    films.groupBy(_._1).toSeq.flatMap { case (title, group) =>
      val showtimes = group
        .flatMap { case (_, url) => byUrl.get(url).flatten.getOrElse(Seq.empty) }
        .distinctBy(s => (s.dateTime, s.bookingUrl))
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = group.headOption.map(_._2),
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      ))
    }.sortBy(_.movie.title)
  }
}

object EkobiletClient {

  val BaseUrl = "https://ekobilet.pl"

  // "10 cze" — day + abbreviated Polish month (shared map with the MSI scraper).
  private val RowDate = """(\d{1,2})\s+(\p{L}+)""".r

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
        m       <- RowDate.findFirstMatchIn(dateStr)
        month   <- MsiScraper.PolishMonthsAbbrev.get(m.group(2).toLowerCase)
        time    <- Option(row.selectFirst("span.fw-bold")).flatMap(s => ScraperParse.parseHHmm(s.text))
        date     = nextOccurrence(today, month, m.group(1).toInt)
        dt      <- Try(LocalDateTime.of(date, time)).toOption
      } yield Showtime(dt, Option(row.attr("data-href")).filter(_.nonEmpty))
    }.distinctBy(s => (s.dateTime, s.bookingUrl))

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

  /** The next date with the given month/day on or after `today` (so a December
   *  listing seen in January resolves to this year, not last). */
  private def nextOccurrence(today: LocalDate, month: Int, day: Int): LocalDate = {
    val year = today.getYear
    val thisYear = Try(LocalDate.of(year, month, day)).getOrElse(today)
    if (thisYear.isBefore(today)) Try(LocalDate.of(year + 1, month, day)).getOrElse(thisYear) else thisYear
  }
}
