package services.cinemas

import models._
import org.jsoup.Jsoup
import play.api.libs.json._
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime}
import scala.util.Try

/**
 * Kino Diana (Prudnik). Its ticketing site biletykinodiana.pl is a jQuery
 * eventCalendar whose feed at `/Repertuar/Kalendarz1JsonDane?filter=<base>/`
 * returns a JSON array with one record per screening — but the meaningful
 * fields are buried in an HTML `event-item` blob inside each record's `title`
 * JSON string:
 *   - `date` (JSON)        — "YYYY-MM-DD HH:MM:SS"; only the DATE is real (the
 *                            time is always 23:59:00 boilerplate, ignored).
 *   - `p.event-title`      — film title, with a trailing format tag stripped by
 *                            [[ScraperParse.stripFormatTags]] ("… - dubbing 2D").
 *   - `p.event-hour`       — "piątek, godz.15:30" — the real screening time.
 *   - `a[href*=/Bilety/Sala]` — the per-screening booking link.
 * So we take the date from the JSON field and the time/title/booking from the
 * parsed HTML, then merge screenings by cleaned title. Previously scraped from
 * Filmweb, which had silently gone empty for the venue (every poll returned
 * `[]` → zero showtimes on /uptime).
 */
class KinoDianaClient(http: HttpFetch, override val cinema: Cinema = KinoDiana)
    extends CinemaScraper {

  import KinoDianaClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] =
    parseFeed(Try(http.get(FeedUrl)).getOrElse(""))
      .groupBy(_._1)
      .toSeq
      .flatMap { case (title, group) =>
        val showtimes = group
          .map { case (_, dt, booking) => Showtime(dt, booking) }
          .distinctBy(s => (s.dateTime, s.bookingUrl))
          .sortBy(_.dateTime)
        if (showtimes.isEmpty) None
        else Some(CinemaMovie(
          movie     = Movie(title),
          cinema    = cinema,
          posterUrl = None,
          filmUrl   = None,
          synopsis  = None,
          cast      = Seq.empty,
          director  = Seq.empty,
          showtimes = showtimes
        ))
      }
      .sortBy(_.movie.title)
}

object KinoDianaClient {

  val BaseUrl = "https://biletykinodiana.pl"
  val FeedUrl = s"$BaseUrl/Repertuar/Kalendarz1JsonDane?filter=$BaseUrl/"

  /** (cleaned title, screening datetime, booking URL) for each screening in the
   *  eventCalendar feed. Date comes from the JSON `date`; time + title +
   *  booking from the HTML blob inside the `title` field. */
  private[cinemas] def parseFeed(body: String): Seq[(String, LocalDateTime, Option[String])] = {
    val records = Try(Json.parse(body)).toOption.collect { case a: JsArray => a.value.toSeq }
      .getOrElse(Seq.empty)
    records.flatMap { record =>
      val date = (record \ "date").asOpt[String].map(_.take(10)).flatMap(d => Try(LocalDate.parse(d)).toOption)
      val document  = Jsoup.parse((record \ "title").asOpt[String].getOrElse(""), BaseUrl)
      val title   = Option(document.selectFirst("p.event-title")).map(_.text.trim).filter(_.nonEmpty)
      val time    = Option(document.selectFirst("p.event-hour")).map(_.text).flatMap(ScraperParse.parseHHmm)
      val booking = Option(document.selectFirst("a[href*=/Bilety/Sala]")).map(_.attr("abs:href")).filter(_.nonEmpty)
      for { d <- date; t <- title; lt <- time } yield (cleanTitle(t), LocalDateTime.of(d, lt), booking)
    }
  }

  /** Strip the trailing format tag and sentence-case the ALL-CAPS title so
   *  variants merge and the result reads like the other sources. */
  private[cinemas] def cleanTitle(raw: String): String =
    ScraperParse.sentenceCase(ScraperParse.stripFormatTags(raw))
}
