package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.{LocalDate, LocalDateTime, LocalTime}
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
    val films = parseListing(http.get(ListingUrl))
    val urls  = films.map(_._2).distinct

    val byUrl = ParallelDetailFetch("kino-jednosc", urls, 1.minute) { url =>
      Try(http.get(url)).toOption.map(parseShowtimes)
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
        time <- Option(item.selectFirst("span.time")).map(_.text.trim).flatMap(t => Try(LocalTime.parse(t)).toOption)
      } yield {
        val booking = Option(item.selectFirst("a.button-success")).map(_.attr("abs:href")).filter(_.nonEmpty)
        Showtime(LocalDateTime.of(date, time), booking)
      }
    }
}
