package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

import java.time.{LocalDate, LocalDateTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Pionier 1907 (Szczecin) — the oldest continuously-operating cinema in
 * the world, run by Dom Kultury "Krzemień". Its WordPress repertoire page at
 * `/repertuar/` is a single server-rendered list covering the whole programme
 * (no pagination): a flat `div.repertuar-list` whose children alternate between
 *   - `h2.naglowek[id=dzien_YYYY-MM-DD]` — a day heading that carries the ISO
 *     date in its `id` and applies to every screening until the next heading,
 *   - `div.event_box` — one screening, with
 *       * `h3 a span`            → film title,
 *       * `h2` text node (`HH:MM`) before the `<br>` → start time, and
 *         `small.sala`           → auditorium (Sala Czerwona / Kiniarnia / …),
 *       * `a.bilet[href]`        → the biletomat.pl booking URL.
 *
 * Headings and event boxes are SIBLINGS (the box isn't nested under the day),
 * so the parser walks the list in document order and carries the last seen
 * date forward onto the boxes that follow it — the same shape as Kino Sfinks's
 * carry-forward, but keyed off the heading's `id` rather than a blank cell.
 *
 * The listing carries everything we surface (title + per-screening date/time +
 * booking link); TMDB enriches poster/synopsis/runtime downstream, so there's
 * no per-film detail fetch.
 */
class PionierClient(http: tools.HttpFetch, override val cinema: Cinema = KinoPionier) extends CinemaScraper {

  import PionierClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(PageUrl)

  def fetch(): Seq[CinemaMovie] = {
    val document   = Jsoup.parse(http.get(PageUrl))
    val slots = parseSlots(document)

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking, s.room)) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = group.flatMap(_.filmUrl).headOption,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }
}

object PionierClient {

  val BaseUrl = "https://pionier1907.pl"
  val PageUrl = s"$BaseUrl/repertuar/"

  private val DayIdPat = """dzien_(\d{4}-\d{2}-\d{2})""".r

  private case class RawSlot(
    title:    String,
    dateTime: LocalDateTime,
    room:     Option[String],
    booking:  Option[String],
    filmUrl:  Option[String]
  )

  /** Walk `.repertuar-list` children in document order: day headings set the
    * current date, event boxes inherit it. */
  private def parseSlots(document: org.jsoup.nodes.Document): Seq[RawSlot] = {
    var carriedDate: Option[LocalDate] = None
    val nodes = document.select("div.repertuar-list").asScala.headOption
      .map(_.children.asScala.toSeq)
      .getOrElse(Seq.empty)

    nodes.flatMap { element =>
      if (element.hasClass("naglowek")) {
        carriedDate = headingDate(element)
        None
      } else if (element.hasClass("event_box")) {
        for {
          date  <- carriedDate
          title <- Option(element.selectFirst("h3 a span")).map(_.text.trim).filter(_.nonEmpty)
          time  <- Option(element.selectFirst("h2")).flatMap(h => ScraperParse.parseHHmm(h.text))
        } yield {
          val room    = Option(element.selectFirst("small.sala")).map(_.text.trim).filter(_.nonEmpty)
          val booking = Option(element.selectFirst("a.bilet[href]")).map(_.attr("href"))
            .filter(h => h.nonEmpty && h != "#")
          val filmUrl = Option(element.selectFirst("h3 a[href]")).map(_.attr("href")).filter(_.nonEmpty)
          RawSlot(title, LocalDateTime.of(date, time), room, booking, filmUrl)
        }
      } else None
    }
  }

  private def headingDate(element: Element): Option[LocalDate] =
    DayIdPat.findFirstMatchIn(element.id)
      .flatMap(m => Try(LocalDate.parse(m.group(1))).toOption)
}
