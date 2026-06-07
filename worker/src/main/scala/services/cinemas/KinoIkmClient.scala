package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino IKM — Kino Kunszt Wodny, run by the Instytut Kultury Miejskiej in
 * Gdańsk (Trójmiasto city scope). The cinema's own page
 * (`ikm.gda.pl/kino-kunszt-wodny/`) is server-rendered WordPress and carries
 * the whole repertoire inline — there's no biletyna.pl iframe and no detail
 * fetch. The booking link on every screening is a single generic
 * `bilety.ikm.gda.pl/rezerwacja/…?idg=2&idl=0` ticketing entry point, not a
 * per-showtime URL, so it's the same for all rows; we keep it as the booking
 * URL all the same.
 *
 * Layout — a `div.screeningtable` with one `div.schedulerow` per screening:
 *   - `div.tday` → `DD.MM, <span>Wt</span>` — date with NO year and a Polish
 *     weekday abbreviation. The year is inferred relative to `today`
 *     (year-boundary roll-forward, like the other year-less scrapers).
 *   - `div.ttitle > h3 > a` → film title (anchor text) + the per-film page URL.
 *   - `div.ttitle > h4` → `reż. <directors> / napisy: …` — director(s) and the
 *     subtitle/accessibility note.
 *   - `div.ttime > a` → start `HH:MM` + the booking URL.
 *
 * Posters live in a separate `section.movielist` above the table, one
 * `div.singleposter` per film keyed by the same film-page URL — we index those
 * and join by URL. The listing has everything we display; TMDB enriches the
 * rest downstream. One `CinemaMovie` per title, screenings merged and sorted.
 */
class KinoIkmClient(
  http: HttpFetch,
  override val cinema: Cinema,
  today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoIkmClient.PageUrl)

  def fetch(): Seq[CinemaMovie] = parseHtml(http.get(KinoIkmClient.PageUrl))

  def parseHtml(html: String): Seq[CinemaMovie] = {
    val doc     = Jsoup.parse(html)
    val posters = posterIndex(doc)
    val slots   = doc.select("div.screeningtable div.schedulerow").asScala.toSeq.flatMap(parseRow)

    slots.groupBy(_.title).toSeq.flatMap { case (title, group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, s.booking))
        .distinctBy(s => (s.dateTime, s.bookingUrl))
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = group.flatMap(_.filmUrl).flatMap(posters.get).headOption,
        filmUrl   = group.flatMap(_.filmUrl).headOption,
        synopsis  = None,
        cast      = Seq.empty,
        director  = group.flatMap(_.directors).distinct,
        showtimes = showtimes
      ))
    }.sortBy(_.movie.title)
  }

  /** Film-page URL → poster URL, read off the `section.movielist` grid. */
  private def posterIndex(doc: Document): Map[String, String] =
    doc.select("section.movielist div.singleposter").asScala.toSeq.flatMap { card =>
      val url    = Option(card.selectFirst("h4 a[href]")).map(_.attr("href")).filter(_.nonEmpty)
      val poster = Option(card.selectFirst("div.pthumb img[src]")).map(_.attr("src")).filter(_.nonEmpty)
      for { u <- url; p <- poster } yield u -> p
    }.toMap

  private def parseRow(row: Element): Option[KinoIkmClient.RawSlot] = {
    val link  = Option(row.selectFirst("div.ttitle h3 a[href]"))
    val title = link.map(_.text.trim).filter(_.nonEmpty)
    val url   = link.map(_.attr("href")).filter(_.nonEmpty)
    val date  = Option(row.selectFirst("div.tday")).map(_.text).flatMap(d => KinoIkmClient.parseDate(d, today))
    val time  = Option(row.selectFirst("div.ttime a")).map(_.text).flatMap(ScraperParse.parseHHmm)

    (title, date, time) match {
      case (Some(t), Some(d), Some(tm)) =>
        val booking   = Option(row.selectFirst("div.ttime a[href]")).map(_.attr("href")).filter(_.nonEmpty)
        val directors = Option(row.selectFirst("div.ttitle h4")).map(_.text).toSeq.flatMap(KinoIkmClient.parseDirectors)
        Some(KinoIkmClient.RawSlot(t, LocalDateTime.of(d, tm), booking, url, directors))
      case _ => None
    }
  }
}

object KinoIkmClient {

  val PageUrl = "https://ikm.gda.pl/kino-kunszt-wodny/"

  // The `.tday` text reads "DD.MM, Wt" — only the leading `DD.MM` is the date;
  // the weekday abbreviation that follows is decoration.
  private val DatePat = """(\d{1,2})\.(\d{1,2})""".r

  /** The leading `DD.MM` of a `.tday` label, with the year inferred relative to
   *  `today`. The page carries no year, so a date more than six months in the
   *  past rolls forward to next year (year-boundary case); recent-past dates
   *  stay put. Mirrors `KinoBulgarskaClient`'s rollover. */
  def parseDate(text: String, today: LocalDate): Option[LocalDate] =
    DatePat.findFirstMatchIn(text).flatMap { m =>
      Try(LocalDate.of(today.getYear, m.group(2).toInt, m.group(1).toInt)).toOption.map { candidate =>
        if (candidate.isBefore(today.minusMonths(6))) candidate.plusYears(1) else candidate
      }
    }

  // The `.ttitle h4` reads "reż. <names> / napisy: …". Take everything after
  // "reż." up to the first " / " (the subtitle/accessibility note), then split
  // the comma-separated director list.
  private val DirectorPat = """(?i)reż\.\s*([^/]+)""".r

  def parseDirectors(h4: String): Seq[String] =
    DirectorPat.findFirstMatchIn(h4).map(_.group(1).trim).toSeq
      .flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))

  private[cinemas] case class RawSlot(
    title:     String,
    dateTime:  LocalDateTime,
    booking:   Option[String],
    filmUrl:   Option[String],
    directors: Seq[String]
  )
}
