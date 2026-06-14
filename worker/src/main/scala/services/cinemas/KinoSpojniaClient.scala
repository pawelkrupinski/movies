package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Spójnia (Aleksandrów Łódzki, the Młodzieżowy Dom Kultury cinema). Its
 * own listing at `kinospojnia.pl/repertuar.php` is a bespoke server-rendered
 * PHP page — one `<table>` per screening, grouped under `div.repertuardata`
 * day headers. Each screening table carries:
 *   - `span.godzina`  → the time ("17:30")
 *   - `a.tytul`       → the film title (href `film.php?id=N`)
 *   - `a.kupbilet`    → the booking link on the Charlie/MSI ticketing host,
 *                       whose query carries the authoritative ISO date
 *                       (`…&date=2026-06-12`) — so the absolute screening date
 *                       comes straight off the buy link, no year inference
 *                       from the Polish-month day headers needed.
 *   - a gray metadata line ("135', USA, 2026, Sci-fi") → release year.
 *
 * (Booking routes through `bilety.charlie.pl`, the same MSI ticketing instance
 * Kino Charlie Łódź uses, but Spójnia's own `repertuar.php` is the cleaner,
 * single-venue source, so we scrape that rather than the shared portal.)
 *
 * Previously scraped from Filmweb, which had silently gone empty for the venue
 * (every poll returned `[]`) though the cinema is open and screening.
 */
class KinoSpojniaClient(http: HttpFetch, override val cinema: Cinema = KinoSpojnia)
    extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoSpojniaClient.BaseUrl)
  override def sourceUrl: Option[String] = Some(KinoSpojniaClient.BaseUrl)

  def fetch(): Seq[CinemaMovie] =
    KinoSpojniaClient.parse(http.get(KinoSpojniaClient.RepertoireUrl), cinema)
}

object KinoSpojniaClient {

  val BaseUrl       = "https://kinospojnia.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar.php"

  // The buy-link query carries the ISO date, percent-encoded in the raw href
  // (`…%26date%3d2026-06-12`) — match both `date=` and `date%3d`.
  private val BookingDatePat = """(?i)date(?:=|%3d)(\d{4}-\d{2}-\d{2})""".r
  private val YearPat        = """\b(19|20)\d{2}\b""".r

  private case class RawSlot(
    title:    String,
    dateTime: LocalDateTime,
    booking:  Option[String],
    filmUrl:  Option[String],
    year:     Option[Int]
  )

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)

    val slots = document.select("table:has(span.godzina):has(a.tytul)").asScala.toSeq.flatMap { t =>
      for {
        titleEl <- Option(t.selectFirst("a.tytul"))
        title    = titleEl.text.replace(' ', ' ').trim if title.nonEmpty
        timeStr <- Option(t.selectFirst("span.godzina")).map(_.text.trim)
        time    <- Try(LocalTime.parse(timeStr)).toOption
        bookEl   = Option(t.selectFirst("a.kupbilet"))
        date    <- bookEl.flatMap(b => BookingDatePat.findFirstMatchIn(b.attr("href")))
                     .flatMap(m => Try(LocalDate.parse(m.group(1))).toOption)
      } yield RawSlot(
        title    = title,
        dateTime = LocalDateTime.of(date, time),
        booking  = bookEl.map(_.attr("abs:href")).filter(_.nonEmpty),
        filmUrl  = Option(titleEl.attr("abs:href")).filter(_.nonEmpty),
        // Gray metadata line e.g. "135', USA, 2026, Sci-fi" → release year.
        year     = Option(t.selectFirst("div[style*=808080]"))
                     .flatMap(d => YearPat.findFirstMatchIn(d.text)).map(_.group(0).toInt)
      )
    }

    slots.groupBy(_.title).toSeq.flatMap { case (title, group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, s.booking))
        .distinctBy(s => (s.dateTime, s.bookingUrl))
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title, releaseYear = group.flatMap(_.year).headOption),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = group.flatMap(_.filmUrl).headOption,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      ))
    }.sortBy(_.movie.title)
  }
}
