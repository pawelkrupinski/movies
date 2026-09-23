package services.cinemas.pl

import services.cinemas.common.{CinemaScraper, ScraperParse, SlotsToMovies}
import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

import java.time.LocalDateTime
import scala.jdk.CollectionConverters._

/**
 * Kino Kultura (Chojnów, run by MOKSiR Chojnów), on a bespoke "Kursorek" CMS
 * (`kino.chojnow.eu`). The repertoire page
 * (`/repertuar,m15,s1500.html`) is a plain server-rendered Bootstrap card
 * grid — one `div.card` per programmed item — NOT a day-by-day calendar: the
 * venue posts events sparsely, sometimes only one or two live at a time,
 * weeks apart. Per card:
 *   - `h2.card-title`                → title.
 *   - a `p.card-text` whose text starts "Rozpoczęcie:" → the screening's
 *     start, either `YYYY-MM-DD HH:MM` (an ordinary single screening) or a
 *     bare `YYYY-MM-DD` (the first day of a multi-date "event cinema"
 *     broadcast whose actual per-date times are only in the free-text
 *     description prose, with no structure worth parsing). Only the
 *     dated-WITH-time form yields a showtime; the bare-date form is dropped
 *     rather than guessed at.
 *   - `img[src]`                     → poster.
 *
 * Booking is off-site, via the town's own ASP.NET ticketing subsite
 * (`bilety.chojnow.eu`) reached from a site-wide nav link rather than a
 * per-event one, so `bookingUrl` is always `None`.
 */
class KinoKulturaChojnowClient(http: HttpFetch, override val cinema: Cinema = KinoKulturaChojnow)
    extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoKulturaChojnowClient.RepertoireUrl)
  override def sourceUrl: Option[String] = Some(KinoKulturaChojnowClient.RepertoireUrl)

  def fetch(): Seq[CinemaMovie] =
    KinoKulturaChojnowClient.parse(http.get(KinoKulturaChojnowClient.RepertoireUrl), cinema)
}

object KinoKulturaChojnowClient {

  val BaseUrl       = "https://kino.chojnow.eu"
  val RepertoireUrl = s"$BaseUrl/repertuar,m15,s1500.html"

  private case class RawSlot(title: String, dateTime: LocalDateTime, poster: Option[String])

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)
    val slots = document.select("#tresc div.card").asScala.toSeq.flatMap(parseCard)

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, bookingUrl = None)) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = group.flatMap(_.poster).headOption,
        filmUrl   = None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }

  private def parseCard(card: Element): Option[RawSlot] = {
    val title = Option(card.selectFirst("h2.card-title")).map(_.text.trim).filter(_.nonEmpty)
    val startText = card.select("p.card-text").asScala.toSeq
      .map(_.text.trim)
      .find(_.startsWith("Rozpoczęcie"))
    val poster = Option(card.selectFirst("img[src]")).map(_.attr("abs:src")).filter(_.nonEmpty)

    for {
      t  <- title
      st <- startText
      dt <- ScraperParse.parseDateTime(st)
    } yield RawSlot(t, dt, poster)
  }
}
