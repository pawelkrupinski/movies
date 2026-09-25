package services.cinemas.pl

import services.cinemas.common.ScraperParse
import models._
import services.movies.FormatTags
import tools.HttpFetch
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper, DayChunks, ScrapeHorizon}

import java.time.{LocalDate, LocalDateTime, ZoneId}
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._

/**
 * Kino KADR (Tomaszowski Dom Kultury, Tomaszów Lubelski) — not to be confused
 * with [[KinoKadrStaszow]], its unrelated Staszów namesake. Its own WordPress
 * site (`kinokadr.pl`) doesn't server-render the schedule: `/repertuar/` ships
 * a 14-day date-picker widget that fetches each day's films from the standard
 * WP AJAX gateway.
 *
 * Two requests per day: a `GET /repertuar/` first, to scrape the current
 * `security` nonce the page's inline script embeds (`data.append("security",
 * "…")`) — then `POST /wp-admin/admin-ajax.php` with
 * `action=kinorep_get_films&date=DD-MM-YYYY&security=<nonce>`, which answers
 * one `article.movie-card` per film screening that day (or a fixed ~200-byte
 * "brak seansów" stub for an empty one). No batch/date-range endpoint exists,
 * so this is [[ChunkedCinemaScraper]]-shaped like [[KinoSwiatowidElblagClient]]
 * — each chunk re-fetches its own nonce (a chunk must stand alone; the nonce
 * isn't shared state across chunk tasks) rather than trusting one scraped at
 * `planChunks` time to still be valid whenever `fetchChunk` runs.
 *
 * Per film:
 *   - `h3.movie-title`                        → title
 *   - `.movie-info` first `span`               → "NN min" runtime
 *   - `.movie-extra span.movie-genre`          → comma-list genres
 *   - `.movie-extra span.movie-audio`          → dub/subtitle badge ("Dubbing")
 *   - `.movie-poster img[src]`                 → poster
 *   - `.movie-description .filmPlotsSection__plot` → synopsis (the venue's
 *     template pastes this straight out of Filmweb's own markup)
 *   - `.showtimes .showtime span`              → one `HH:MM` per showing
 *   - `.ticket-container a.trailer-btn[href]`  → YouTube trailer (there is no
 *     ticket-buying link despite the class name — this venue sells at the
 *     door/by phone only)
 *
 * Verified screening real, dated films 2026-09-23 through 2026-10-07 (a small
 * fixed weekly rota, not every day): Mistyczka / 100 dni: Misja Zeus / Lalka,
 * daily at 14:50 / 16:30 / 18:30 on 30 Sep, 1, 3 and 4 Oct 2026.
 */
class KinoKadrTomaszowLubelskiClient(
  http:                HttpFetch,
  override val cinema: Cinema    = KinoKadrTomaszowLubelski,
  today:               LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends ChunkedCinemaScraper {

  import KinoKadrTomaszowLubelskiClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(RepertoireUrl)
  override def sourceUrl: Option[String] = Some(RepertoireUrl)

  def planChunks(): Seq[String] = {
    val nonce = fetchNonce()
    DayChunks.keys(ScrapeHorizon.liveDays(today) { day => parseDay(postDay(nonce, day), day, cinema).nonEmpty })
  }

  def fetchChunk(key: String): Seq[CinemaMovie] = {
    val nonce = fetchNonce()
    DayChunks.days(key).flatMap(day => parseDay(postDay(nonce, day), day, cinema))
  }

  private def fetchNonce(): String =
    NoncePat.findFirstMatchIn(http.get(RepertoireUrl)).map(_.group(1))
      .getOrElse(throw new RuntimeException(s"KinoKadrTomaszowLubelskiClient: no AJAX nonce found on $RepertoireUrl"))

  private def postDay(nonce: String, day: LocalDate): String =
    http.post(AjaxUrl, s"action=$Action&date=${DateFormat.format(day)}&security=$nonce",
      "application/x-www-form-urlencoded")
}

object KinoKadrTomaszowLubelskiClient {

  val BaseUrl       = "https://kinokadr.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar/"
  val AjaxUrl       = s"$BaseUrl/wp-admin/admin-ajax.php"

  private val Action = "kinorep_get_films"
  private val DateFormat = DateTimeFormatter.ofPattern("dd-MM-yyyy")

  private val NoncePat   = """data\.append\(\s*"security"\s*,\s*"([0-9a-f]+)"\s*\)""".r
  private val RuntimePat = """(\d+)\s*min""".r

  private[cinemas] def parseDay(html: String, day: LocalDate, cinema: Cinema): Seq[CinemaMovie] =
    Jsoup.parse(html, BaseUrl).select("article.movie-card").asScala.toSeq.flatMap(parseFilm(_, day, cinema))

  private def parseFilm(article: Element, day: LocalDate, cinema: Cinema): Option[CinemaMovie] = {
    val format = Option(article.selectFirst(".movie-extra span.movie-audio")).map(_.text).toSeq
      .flatMap(FormatTags.formatTokensIn).distinct.toList

    val showtimes = article.select(".showtimes .showtime span").asScala.toSeq
      .flatMap(s => ScraperParse.parseHHmm(s.text))
      .map(t => Showtime(LocalDateTime.of(day, t), None, format = format))

    for {
      title <- Option(article.selectFirst("h3.movie-title")).map(_.text.trim).filter(_.nonEmpty)
      if showtimes.nonEmpty
    } yield CinemaMovie(
      movie     = Movie(
        title,
        runtimeMinutes = Option(article.selectFirst(".movie-info span")).map(_.text)
                            .flatMap(RuntimePat.findFirstMatchIn).map(_.group(1).toInt),
        genres         = Option(article.selectFirst(".movie-extra span.movie-genre")).map(_.text).toSeq
                            .flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
      ),
      cinema      = cinema,
      posterUrl   = Option(article.selectFirst(".movie-poster img[src]")).map(_.attr("abs:src")).filter(_.nonEmpty),
      filmUrl     = None,
      synopsis    = Option(article.selectFirst(".movie-description .filmPlotsSection__plot")).map(_.text.trim).filter(_.nonEmpty),
      cast        = Seq.empty,
      director    = Seq.empty,
      showtimes   = showtimes.sortBy(_.dateTime),
      trailerUrl  = Option(article.selectFirst(".ticket-container a.trailer-btn[href]")).map(_.attr("abs:href"))
                      .filter(_.nonEmpty).flatMap(ScraperParse.canonicalTrailer)
    )
  }
}
