package services.cinemas.pl

import services.cinemas.common.ScraperParse
import models._
import tools.{HttpFetch, ParallelDetailFetch}
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import services.cinemas.common.CinemaScraper

import java.time.{LocalDate, LocalTime}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Parczew. `kinoparczew.pl` (WordPress/Elementor) lists its current
 * films as a homepage grid of `a[href*=/filmy/]` cards; each film's OWN page
 * (`/filmy/<slug>/`) carries both its identity and its full schedule — there
 * is no separate showtimes listing to merge against. The detail page's
 * `<table>` has one `<tr>` per field (`td.table-cell-head` label,
 * `td.table-cell-entry` value):
 *   - `W kinie:`         one or more "Codziennie od DD.MM.YYYY do DD.MM.YYYY
 *                        o godzinie HH:MM[ i HH:MM]" lines, `<br>`-separated
 *   - `Format:`          `<li>` list, e.g. "2D"
 *   - `Długość:`         runtime in minutes
 *   - `Gatunek:`         `<li>` list of genres
 *   - `Reżyseria:`       plain-text director(s), comma-separated
 *   - `Kraj produkcji:`  `<li>` list of countries
 * No online ticketing — reservation is phone-only, so `bookingUrl` stays
 * `None`. Each "Codziennie od X do Y o godzinie …" run expands to one
 * showtime per day at its listed time(s) ([[ScraperParse.dailyRange]]).
 * Titles are shouted (`LALKA`) and run through [[ScraperParse.sentenceCase]].
 */
class KinoParczewClient(http: HttpFetch, override val cinema: Cinema = KinoParczew)
    extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoParczewClient.HomeUrl)
  override def sourceUrl: Option[String] = Some(KinoParczewClient.HomeUrl)

  def fetch(): Seq[CinemaMovie] = {
    val filmUrls = KinoParczewClient.filmLinks(http.get(KinoParczewClient.HomeUrl))
    val byUrl = ParallelDetailFetch("kino-parczew", filmUrls, 30.seconds) { url =>
      Try(http.get(url)).toOption.flatMap(KinoParczewClient.parseFilm(_, url, cinema))
    }
    filmUrls.flatMap(byUrl.get).flatten.sortBy(_.movie.title)
  }
}

object KinoParczewClient {

  val HomeUrl = "https://kinoparczew.pl"

  private val FilmLink = """^https://kinoparczew\.pl/filmy/[^/]+/?$""".r

  // "Codziennie od 02.10.2026 do 08.10.2026 o godzinie 16:00 i 19:00"
  private val Run =
    """(?i)codziennie\s+od\s+(\d{1,2})\.(\d{1,2})\.(\d{4})\s+do\s+(\d{1,2})\.(\d{1,2})\.(\d{4})\s+o\s+godzinie\s+(\d{1,2}):(\d{2})(?:\s+i\s+(\d{1,2}):(\d{2}))?""".r

  def filmLinks(homeHtml: String): Seq[String] =
    Jsoup.parse(homeHtml, HomeUrl).select("a[href*=/filmy/]").asScala.toSeq
      .map(_.attr("abs:href"))
      .filter(FilmLink.findFirstIn(_).isDefined)
      .distinct

  def parseFilm(html: String, url: String, cinema: Cinema): Option[CinemaMovie] = {
    val document = Jsoup.parse(html, url)
    Option(document.selectFirst("h1.entry-title")).map(_.text.trim).filter(_.nonEmpty).map { rawTitle =>
      val fields = fieldsOf(document)
      val format = fields.get("format").map(liTextsOf).getOrElse(Seq.empty)
      val showtimes = fields.get("w kinie").toSeq.flatMap(runsIn).flatMap { case (from, to, times) =>
        times.flatMap(t => ScraperParse.dailyRange(from, to, t))
      }.distinct.sorted.map(dt => Showtime(dt, None, format = format.toList))

      CinemaMovie(
        movie     = Movie(
          title          = ScraperParse.sentenceCase(rawTitle),
          runtimeMinutes = fields.get("długość").flatMap(e => """\d+""".r.findFirstIn(e.text)).flatMap(n => Try(n.toInt).toOption),
          countries      = fields.get("kraj produkcji").map(liTextsOf).getOrElse(Seq.empty),
          genres         = fields.get("gatunek").map(liTextsOf).getOrElse(Seq.empty)
        ),
        cinema    = cinema,
        posterUrl = Option(document.selectFirst("img[itemprop=image]")).map(_.attr("abs:src")).filter(_.nonEmpty),
        filmUrl   = Some(url),
        synopsis  = synopsisOf(document),
        cast      = Seq.empty,
        director  = fields.get("reżyseria").map(_.text.trim).filter(_.nonEmpty)
                      .map(_.split(",").toSeq.map(_.trim).filter(_.nonEmpty)).getOrElse(Seq.empty),
        showtimes = showtimes
      )
    }
  }

  private def fieldsOf(document: Document): Map[String, Element] =
    document.select("table tr").asScala.toSeq.flatMap { tr =>
      for {
        head  <- Option(tr.selectFirst("td.table-cell-head"))
        entry <- Option(tr.selectFirst("td.table-cell-entry"))
      } yield head.text.trim.stripSuffix(":").toLowerCase -> entry
    }.toMap

  private def liTextsOf(entry: Element): Seq[String] =
    entry.select("li").asScala.toSeq.map(_.text.trim).filter(_.nonEmpty)

  private def runsIn(entry: Element): Seq[(LocalDate, LocalDate, Seq[LocalTime])] =
    ScraperParse.linesOf(entry).flatMap(l => Run.findFirstMatchIn(l)).flatMap { m =>
      for {
        from <- Try(LocalDate.of(m.group(3).toInt, m.group(2).toInt, m.group(1).toInt)).toOption
        to   <- Try(LocalDate.of(m.group(6).toInt, m.group(5).toInt, m.group(4).toInt)).toOption
      } yield {
        val times = Seq(
          Try(LocalTime.of(m.group(7).toInt, m.group(8).toInt)).toOption,
          Option(m.group(9)).flatMap(h => Try(LocalTime.of(h.toInt, m.group(10).toInt)).toOption)
        ).flatten
        (from, to, times)
      }
    }

  private def synopsisOf(document: Document): Option[String] =
    document.select("div.movie-content h2").asScala.toSeq
      .find(_.text.trim.equalsIgnoreCase("Opis filmu"))
      .flatMap(h => Option(h.nextElementSibling))
      .filter(_.tagName == "p")
      .map(_.text.trim)
      .filter(_.nonEmpty)
}
