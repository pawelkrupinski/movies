package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Roma (Zabrze) — run by Miejski Ośrodek Kultury. The repertoire page at
 * `/repertuar` is server-rendered: each upcoming screening is a separate
 * `div.poster-box` card holding exactly one date + one time + the film title.
 * Films with multiple screenings appear as multiple cards.
 *
 * Within each card:
 *  - `div.big-date` → date in `DD.MM` format (no year).
 *  - `div.hour`     → time as "godz. HH:MM".
 *  - `a.film-title` → film title text + relative URL `/repertuar/<slug>`.
 *  - `img.img-responsive[src]` → poster image.
 *
 * There are no per-screening booking links on this page (the cinema uses a
 * separate contact/reservation system), so `bookingUrl` is always `None`.
 *
 * The year is inferred from `today` the same way as `KinoZorzaClient`:
 * a `DD.MM` date more than 60 days in the past belongs to next year.
 */
class KinoRomaClient(
  http:             HttpFetch,
  override val cinema: Cinema,
  today:            LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import KinoRomaClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val html = http.get(RepertoireUrl)
    val document  = Jsoup.parse(html)
    val slots = parseDocument(document, today)

    // Group by slug (one CinemaMovie per film)
    val bySlug = slots.groupBy(_.filmSlug)
    bySlug.toSeq.flatMap { case (_, group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, bookingUrl = None))
        .distinctBy(_.dateTime)
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else {
        val head = group.head
        Some(CinemaMovie(
          movie     = Movie(head.title),
          cinema    = cinema,
          posterUrl = head.posterUrl,
          filmUrl   = Some(BaseUrl + "/repertuar/" + head.filmSlug),
          synopsis  = None,
          cast      = Seq.empty,
          director  = Seq.empty,
          showtimes = showtimes
        ))
      }
    }.sortBy(_.movie.title)
  }
}

object KinoRomaClient {

  val BaseUrl       = "https://www.kinoroma.zabrze.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar"

  private val DayMonthPat = """(\d{1,2})\.(\d{1,2})""".r
  private val SlugPat     = """/repertuar/([^/?]+)""".r

  private[cinemas] case class RawSlot(
    title:     String,
    filmSlug:  String,
    dateTime:  LocalDateTime,
    posterUrl: Option[String]
  )

  private[cinemas] def parseDocument(document: Document, today: LocalDate): Seq[RawSlot] =
    document.select("div.poster-box").asScala.toSeq.flatMap { box =>
      val dateText  = Option(box.selectFirst("div.big-date")).map(_.text.trim).getOrElse("")
      val hourText  = Option(box.selectFirst("div.hour")).map(_.text.trim).getOrElse("")
      val titleEl   = Option(box.selectFirst("a.film-title"))
      val title     = titleEl.map(_.text.trim).filter(_.nonEmpty)
      val filmSlug  = titleEl.flatMap(a => SlugPat.findFirstMatchIn(a.attr("href")).map(_.group(1)))
      val posterUrl = Option(box.selectFirst("img.img-responsive[src]")).map(_.attr("src")).filter(_.nonEmpty)

      for {
        t    <- title
        slug <- filmSlug
        date <- parseDate(dateText, today)
        time <- ScraperParse.parseHHmm(hourText)
      } yield RawSlot(t, slug, LocalDateTime.of(date, time), posterUrl)
    }

  /** "DD.MM" → LocalDate; if the date would be more than 60 days in the past,
   *  assume next year (handles December → January page-turn). */
  private[cinemas] def parseDate(text: String, today: LocalDate): Option[LocalDate] =
    DayMonthPat.findFirstMatchIn(text).flatMap { m =>
      Try {
        val day   = m.group(1).toInt
        val month = m.group(2).toInt
        val candidate = LocalDate.of(today.getYear, month, day)
        if (candidate.isBefore(today.minusDays(60))) candidate.plusYears(1) else candidate
      }.toOption
    }
}
