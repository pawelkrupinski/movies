package services.cinemas.pl

import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.CinemaScraper

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Świt (DK Świt, Warszawa). The `/kino/` page renders every film as a card
 * with its runtime / year / director and a row of bookable showtimes (each slot
 * carries an absolute `data-showtime-date` (YYYYMMDD) + time + biletyna link).
 * Everything needed is on the one page — no detail fetches.
 */
class SwitClient(http: HttpFetch) extends CinemaScraper {

  val cinema: Cinema = KinoSwit

  private val ListingUrl = "https://dkswit.com.pl/kino/"
  private val DateFmt    = DateTimeFormatter.ofPattern("yyyyMMdd")
  private val YearPat    = """\b((?:19|20)\d{2})\b""".r

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(ListingUrl)
  override def sourceUrl: Option[String] = Some(ListingUrl)

  def fetch(): Seq[CinemaMovie] =
    Jsoup.parse(http.get(ListingUrl)).select("div.cks-movie-card").asScala.toSeq.flatMap(parseCard)

  private def parseCard(card: Element): Option[CinemaMovie] = {
    val title = Option(card.selectFirst("h2")).map(_.text.trim).filter(_.nonEmpty)
    val showtimes = card.select("a.cks-showtime-item").asScala.toSeq.flatMap { a =>
      val date = Option(a.attr("data-showtime-date")).filter(_.nonEmpty).flatMap(d => Try(LocalDate.parse(d, DateFmt)).toOption)
      val time = Option(a.selectFirst("span.cks-st-time")).map(_.text.trim)
        .flatMap(ScraperParse.parseHHmm)
      val booking = Option(a.attr("href")).filter(_.nonEmpty)
      for { d <- date; t <- time } yield Showtime(d.atTime(t), booking, None, Nil)
    }.distinctBy(s => (s.dateTime, s.bookingUrl)).sortBy(_.dateTime)

    title.filter(_ => showtimes.nonEmpty).map { t =>
      val cardText = card.text
      val runtime  = """(\d+)\s*min""".r.findFirstMatchIn(cardText).map(_.group(1).toInt)
      val infoLine = Option(card.selectFirst("p.text-gray-600")).map(_.text).getOrElse("")
      val year     = YearPat.findFirstMatchIn(infoLine).map(_.group(1).toInt)
      val director = """(?i)reżyseria:\s*([^•|<]+)""".r.findFirstMatchIn(infoLine).map(_.group(1).trim)
                       .toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
      // The genre badge is the purple-tinted pill on the card; the blue one is
      // the "Napisy PL" subtitle tag, so scope to the purple class.
      val genres   = card.select("span[class*=text-purple]").asScala.toSeq.map(_.text.trim).filter(_.nonEmpty)
                       .map(tools.TextNormalization.titleCaseIfAllLower)
      val detailUrl = Option(card.attr("onclick")).flatMap(o => """window\.location='([^']+)'""".r.findFirstMatchIn(o).map(_.group(1)))
      CinemaMovie(
        movie     = Movie(title = t, runtimeMinutes = runtime, releaseYear = year, genres = genres),
        cinema    = cinema,
        posterUrl = Option(card.selectFirst("img[src]")).map(_.attr("src")).filter(_.nonEmpty),
        filmUrl   = detailUrl,
        synopsis  = Option(card.selectFirst("p.text-gray-700")).map(_.text.trim).filter(_.length > 20),
        cast      = Seq.empty,
        director  = director,
        showtimes = showtimes
      )
    }
  }
}
