package services.cinemas.pl

import services.cinemas.common.ScraperParse
import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.CinemaScraper

import java.time.{LocalDate, LocalDateTime, MonthDay, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Sokół (Dom Kultury "Sokół", Strzyżów) — not to be confused with the
 * unrelated `KinoSokol*` venues this codebase already models for other towns
 * (Brzozów, Sokółka, Nisko, …). Its own WordPress site (`dksokol.eu/kino/`) is
 * fully server-rendered — one `article.screening` per film:
 *   - `.screening-info h3 a`             → title + the venue's own `/seans/…/` page
 *   - `.screening-poster img[src]`       → poster
 *   - `.screening-meta-badges span`      → free-text badges in no fixed order
 *     across films (age like "od 10 lat", a runtime like "92 min."/"112 minut",
 *     a genre) — a runtime-shaped badge is picked out by regex, an age-shaped
 *     one dropped, and everything else kept as a genre.
 *   - `.screening-excerpt`               → a short synopsis blurb
 *   - `.screening-terms li`              → one showtime per `<li>`: the day
 *     number (`.screening-term-date b`), a 3-letter Polish month abbreviation
 *     (`.screening-term-date em`, e.g. "wrz"/"paź") with NO year — inferred
 *     forward from `today` — and `HH:MM` (`.screening-term-time`).
 *
 * Ticket sale isn't live yet: every "buy" button on the page is a dead `#`
 * anchor labelled "Sprzedaż wkrótce" ("sale coming soon"; the CSS is
 * literally commented "ready for Visual Ticket"), so no booking URL is set.
 *
 * Verified screening real, dated films 2026-09-23 through 2026-10-07: Księga
 * pustyni, 100 Dni: Misja Zeus, Pucio kocha zwierzaki, Dzień dziecka księdza
 * Jana Kaczkowskiego, Toy Story 5, Mistyczka.
 */
class KinoSokolStrzyzowClient(
  http:                HttpFetch,
  override val cinema: Cinema    = KinoSokolStrzyzow,
  today:               LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoSokolStrzyzowClient.RepertoireUrl)
  override def sourceUrl: Option[String] = Some(KinoSokolStrzyzowClient.RepertoireUrl)

  def fetch(): Seq[CinemaMovie] =
    KinoSokolStrzyzowClient.parse(http.get(KinoSokolStrzyzowClient.RepertoireUrl), cinema, today)
}

object KinoSokolStrzyzowClient {

  val BaseUrl       = "https://dksokol.eu"
  val RepertoireUrl = s"$BaseUrl/kino/"

  private val RuntimePat = """(\d+)\s*min""".r
  private val AgePat     = """(?i)^\s*od\s+\d""".r

  def parse(html: String, cinema: Cinema, today: LocalDate): Seq[CinemaMovie] =
    Jsoup.parse(html, BaseUrl).select("article.screening").asScala.toSeq.flatMap(parseArticle(_, cinema, today))

  private def parseArticle(article: Element, cinema: Cinema, today: LocalDate): Option[CinemaMovie] = {
    val titleElement = Option(article.selectFirst(".screening-info h3 a"))
    val badges  = article.select(".screening-meta-badges span").asScala.toSeq.map(_.text.trim).filter(_.nonEmpty)
    val runtime = badges.flatMap(RuntimePat.findFirstMatchIn).map(_.group(1).toInt).headOption
    val genres  = badges.filterNot(b => RuntimePat.findFirstIn(b).isDefined || AgePat.findFirstIn(b).isDefined)

    val showtimes = article.select(".screening-terms li").asScala.toSeq.flatMap { li =>
      for {
        dayText   <- Option(li.selectFirst(".screening-term-date b")).map(_.text.trim)
        day       <- dayText.toIntOption
        monthText <- Option(li.selectFirst(".screening-term-date em")).map(_.text.trim)
        month     <- ScraperParse.polishMonthAbbrev(monthText)
        monthDay  <- Try(MonthDay.of(month, day)).toOption
        date      <- ScraperParse.upcomingDate(monthDay, today)
        timeText  <- Option(li.selectFirst(".screening-term-time")).map(_.text.trim)
        time      <- ScraperParse.parseHHmm(timeText)
      } yield Showtime(LocalDateTime.of(date, time), None)
    }.distinctBy(_.dateTime).sortBy(_.dateTime)

    for {
      titleAnchor <- titleElement
      title = titleAnchor.text.trim if title.nonEmpty
      if showtimes.nonEmpty
    } yield CinemaMovie(
      movie     = Movie(title, runtimeMinutes = runtime, genres = genres),
      cinema    = cinema,
      posterUrl = Option(article.selectFirst(".screening-poster img[src]")).map(_.attr("abs:src")).filter(_.nonEmpty),
      filmUrl   = Option(titleAnchor.attr("abs:href")).filter(_.nonEmpty),
      synopsis  = Option(article.selectFirst(".screening-excerpt")).map(_.text.trim).filter(_.nonEmpty),
      cast      = Seq.empty,
      director  = Seq.empty,
      showtimes = showtimes
    )
  }
}
