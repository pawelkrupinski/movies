package services.cinemas.pl

import services.cinemas.common.SlotsToMovies

import services.cinemas.common.{ScraperParse, ListingPages}
import tools.{HttpFetch, HttpRead}
import models._
import org.jsoup.Jsoup
import services.cinemas.common.CinemaScraper

import java.time.{LocalDate, LocalDateTime}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Jedność (Sędziszów Małopolski). WordPress site with a two-level layout:
 * the listing at `www.kinosedziszow.pl/repertuar-2/` gives one
 * `div.item-repertoire` per film (title + a link to a detail page, but only a
 * marketing "Od 12 czerwca" date range — no times). The discrete dated
 * screenings live on each film's detail page (`/repertuar/<slug>/`) under a
 * `div.available-item` per slot: `span.date` ("YYYY-MM-DD"), `span.time`
 * ("HH:MM") and a `kino.mgoks.pl` booking link. So we fetch the listing, then
 * each detail page in parallel.
 *
 * (The bare `kinosedziszow.pl` host returns nothing — the `www.` host is
 * authoritative.) Previously scraped from Filmweb, which had silently gone
 * empty for the venue.
 */
class KinoJednoscClient(http: HttpFetch, override val cinema: Cinema = KinoJednosc)
    extends CinemaScraper {

  import KinoJednoscClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val films = parseListing(HttpRead.page(http, ListingUrl))
    val urls  = films.map(_._2).distinct

    val byUrl = ListingPages.readMore("kino-jednosc", urls, identity[String], timeout = 1.minute) { url =>
      Option(HttpRead.page(http, url)).map(parseShowtimes)
    }.toMap

    SlotsToMovies.foldBlocks(films, _._1, (film: (String, String)) => byUrl.get(film._2).flatten.getOrElse(Seq.empty)) { (title, group, showtimes) =>
      Some(CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = group.headOption.map(_._2),
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      ))
    }
  }
}

object KinoJednoscClient {

  val BaseUrl     = "https://www.kinosedziszow.pl"
  val ListingUrl  = s"$BaseUrl/repertuar-2/"

  /** (title, detail-page URL) for each film tile on the listing. */
  private[cinemas] def parseListing(html: String): Seq[(String, String)] =
    Jsoup.parse(html, BaseUrl).select("div.item-repertoire").asScala.toSeq.flatMap { item =>
      for {
        title <- Option(item.selectFirst("h2.title-repertoire")).map(_.text.trim).filter(_.nonEmpty)
        url   <- Option(item.selectFirst("a.wrap-entry-repertoire")).map(_.attr("abs:href")).filter(_.nonEmpty)
      } yield (title, url)
    }

  /** Discrete showtimes off a film detail page. Each `div.available-item`
   *  repeats its date/time inside a `.on-hover` block, so take the first
   *  `span.date`/`span.time` (document order = the outer pair). */
  private[cinemas] def parseShowtimes(html: String): Seq[Showtime] =
    Jsoup.parse(html, BaseUrl).select("div.available-item").asScala.toSeq.flatMap { item =>
      for {
        date <- Option(item.selectFirst("span.date")).map(_.text.trim).flatMap(d => Try(LocalDate.parse(d)).toOption)
        time <- Option(item.selectFirst("span.time")).map(_.text.trim).flatMap(ScraperParse.parseHHmm)
      } yield {
        val booking = Option(item.selectFirst("a.button-success")).map(_.attr("abs:href")).filter(_.nonEmpty)
        Showtime(LocalDateTime.of(date, time), booking)
      }
    }
}
