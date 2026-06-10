package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Generic client for cinemas ticketed through ekobilet.pl. The venue landing
 * `ekobilet.pl/<slug>` is server-rendered and lists each film as a
 * `div.event-card a[href]` (link to the film's detail page) with the title in a
 * sibling `p.overme`. The film detail page carries the dated screenings: one
 * `div.event-buy[data-href]` per slot, with `strong.primary-color` = "DD <pl-mon
 * abbrev>" (e.g. "10 cze") and a `span.fw-bold` = "HH:MM …". The year isn't in
 * the row, so it's inferred from `today` (next occurrence of that month/day).
 *
 * One instance per venue, captured by its `slug` + `cinema` (OCP). Two fetches:
 * the landing, then each film's detail page in parallel. Previously scraped from
 * Filmweb.
 */
class EkobiletClient(
  http:   HttpFetch,
  slug:   String,
  override val cinema: Cinema,
  today:  LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import EkobiletClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val films = parseLanding(http.get(s"$BaseUrl/$slug"))   // (title, detailUrl)
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

  /** (cleaned title, detail-page URL) for each film card on the venue landing,
   *  de-duplicated (cards render twice for desktop/mobile). */
  private[cinemas] def parseLanding(html: String): Seq[(String, String)] = {
    val doc = Jsoup.parse(html, BaseUrl)
    doc.select("div.event-card a[href]").asScala.toSeq.flatMap { a =>
      val url = a.attr("abs:href").takeWhile(_ != '?')
      // The card's title is the nearest following `p.overme`.
      val titleEl = Option(a.closest("div.event-card")).flatMap(c =>
        Option(c.parent).flatMap(p => Option(p.selectFirst("p.overme"))))
        .orElse(Option(doc.selectFirst("p.overme")))
      for {
        t <- titleEl.map(e => ScraperParse.stripFormatTags(e.text)).filter(_.nonEmpty)
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

  /** The next date with the given month/day on or after `today` (so a December
   *  listing seen in January resolves to this year, not last). */
  private def nextOccurrence(today: LocalDate, month: Int, day: Int): LocalDate = {
    val y = today.getYear
    val thisYear = Try(LocalDate.of(y, month, day)).getOrElse(today)
    if (thisYear.isBefore(today)) Try(LocalDate.of(y + 1, month, day)).getOrElse(thisYear) else thisYear
  }
}
