package services.cinemas.pl

import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

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
  // A bare-year token in the gray meta line ("2026").
  private val YearTokenPat   = """^(?:19|20)\d{2}$""".r
  // A runtime token ("135'" / "55’") — the minutes followed by a prime.
  private val RuntimeTokenPat = """^(\d{1,3})\s*['’]""".r

  /** Everything the gray metadata line ("135', USA, 2026, Sci-fi" or
   *  "104', USA, Chiny, 2026, Animacja, Komedia") carries. The line is ordered
   *  `runtime', country[, country…], year, genre[, genre…]` — so the year token
   *  splits countries (before) from genres (after), and the prime token is the
   *  runtime. */
  private[cinemas] case class GrayMeta(
    year:      Option[Int] = None,
    runtime:   Option[Int] = None,
    countries: Seq[String]  = Seq.empty,
    genres:    Seq[String]  = Seq.empty
  )

  private case class RawSlot(
    title:    String,
    dateTime: LocalDateTime,
    booking:  Option[String],
    filmUrl:  Option[String],
    meta:     GrayMeta
  )

  private[cinemas] def parseGrayMeta(line: String): GrayMeta = {
    val tokens  = line.split(",").map(_.trim).filter(_.nonEmpty).toSeq
    val runtime = tokens.iterator.flatMap(t => RuntimeTokenPat.findFirstMatchIn(t)).map(_.group(1).toInt).nextOption()
    val yearIdx = tokens.indexWhere(t => YearTokenPat.matches(t))
    if (yearIdx < 0) GrayMeta(runtime = runtime)
    else {
      val year      = scala.util.Try(tokens(yearIdx).toInt).toOption
      // Countries sit between the runtime prime and the year; genres after it.
      val countries = tokens.take(yearIdx).filterNot(t => RuntimeTokenPat.findFirstIn(t).isDefined)
      val genres    = tokens.drop(yearIdx + 1)
      GrayMeta(year, runtime, countries, genres)
    }
  }

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)

    val slots = document.select("table:has(span.godzina):has(a.tytul)").asScala.toSeq.flatMap { t =>
      for {
        titleElement <- Option(t.selectFirst("a.tytul"))
        title    = titleElement.text.replace(' ', ' ').trim if title.nonEmpty
        timeStr <- Option(t.selectFirst("span.godzina")).map(_.text.trim)
        time    <- Try(LocalTime.parse(timeStr)).toOption
        bookElement   = Option(t.selectFirst("a.kupbilet"))
        date    <- bookElement.flatMap(b => BookingDatePat.findFirstMatchIn(b.attr("href")))
                     .flatMap(m => Try(LocalDate.parse(m.group(1))).toOption)
      } yield RawSlot(
        title    = title,
        dateTime = LocalDateTime.of(date, time),
        booking  = bookElement.map(_.attr("abs:href")).filter(_.nonEmpty),
        filmUrl  = Option(titleElement.attr("abs:href")).filter(_.nonEmpty),
        // Gray metadata line e.g. "135', USA, 2026, Sci-fi" → runtime, countries,
        // release year, genres.
        meta     = Option(t.selectFirst("div[style*=808080]"))
                     .map(d => parseGrayMeta(d.text)).getOrElse(GrayMeta())
      )
    }

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking)) { (title, group, showtimes) =>
      val metas = group.map(_.meta)
      CinemaMovie(
        movie     = Movie(
          title,
          runtimeMinutes = metas.flatMap(_.runtime).headOption,
          releaseYear    = metas.flatMap(_.year).headOption,
          countries      = metas.map(_.countries).find(_.nonEmpty).getOrElse(Seq.empty),
          genres         = metas.map(_.genres).find(_.nonEmpty).getOrElse(Seq.empty)
        ),
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
