package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.HttpFetch

import java.time.LocalDateTime
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Ślęża (Sobótka, run by the Regionalne Centrum Kultury Sobótka). Its
 * listing at `rcks.pl/kino-sleza/repertuar/` is a server-rendered WordPress
 * page — one `div.movie` block per film:
 *   - `h5.movie-title` → title (its own text; a nested `span.trailer` holds the
 *     YouTube link, which `ownText` excludes).
 *   - `figure.movie-img img` → poster.
 *   - `p > em > strong` → a "Genre, Genre // napisy //" line (genres before the
 *     first `//`).
 *   - `span.description` → synopsis.
 *   - `<h6>Seans:</h6><ul><li>` → showtimes, each `li` carrying a `D.MM.YYYY`
 *     date and a `<span>HH:MM</span>` time (the year is on the page, so no
 *     inference). The venue takes phone reservations only — no booking links.
 *
 * Previously scraped from Filmweb, which had silently gone empty for the venue
 * (every poll returned `[]`) though the cinema is open and screening.
 */
class KinoSlezaClient(http: HttpFetch, override val cinema: Cinema = KinoSleza)
    extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoSlezaClient.BaseUrl)

  def fetch(): Seq[CinemaMovie] =
    KinoSlezaClient.parse(http.get(KinoSlezaClient.RepertoireUrl), cinema)
}

object KinoSlezaClient {

  val BaseUrl       = "https://rcks.pl"
  val RepertoireUrl = s"$BaseUrl/kino-sleza/repertuar/"

  private val DatePat = """(\d{1,2})\.(\d{1,2})\.(\d{4})""".r
  private val TimePat = """(\d{1,2}):(\d{2})""".r

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] =
    Jsoup.parse(html, BaseUrl).select("div.movie").asScala.toSeq.flatMap(parseMovie(_, cinema))

  private def parseMovie(movie: Element, cinema: Cinema): Option[CinemaMovie] =
    for {
      title    <- Option(movie.selectFirst("h5.movie-title")).map(_.ownText.trim).filter(_.nonEmpty)
      showtimes = parseShowtimes(movie)
      if showtimes.nonEmpty
    } yield CinemaMovie(
      movie     = Movie(title, genres = genresOf(movie)),
      cinema    = cinema,
      posterUrl = Option(movie.selectFirst("figure.movie-img img")).map(_.attr("abs:src")).filter(_.nonEmpty),
      filmUrl   = None,
      synopsis  = Option(movie.selectFirst("span.description")).map(_.text.trim).filter(_.length > 20),
      cast      = Seq.empty,
      director  = Seq.empty,
      showtimes = showtimes
    )

  /** One `Showtime` per `<li>` under the "Seans:" list. The HTML is a touch
   *  malformed (`</span<`), so pull the date and time by regex off the row text
   *  rather than relying on the `<span>` boundary. */
  private def parseShowtimes(movie: Element): Seq[Showtime] =
    movie.select("ul li").asScala.toSeq.flatMap { li =>
      val text = li.text
      for {
        d <- DatePat.findFirstMatchIn(text)
        t <- TimePat.findFirstMatchIn(text)
        dt <- Try(LocalDateTime.of(
                d.group(3).toInt, d.group(2).toInt, d.group(1).toInt,
                t.group(1).toInt, t.group(2).toInt)).toOption
      } yield Showtime(dt, None)
    }.distinctBy(_.dateTime).sortBy(_.dateTime)

  /** Genres are the comma-list before the first `//` in the "Komedia, Dramat //
   *  napisy //" metadata line (anything after `//` is format, not genre). */
  private def genresOf(movie: Element): Seq[String] =
    Option(movie.selectFirst("p em strong")).map(_.text).toSeq
      .flatMap(_.split("//").headOption)
      .flatMap(_.split(",").map(_.trim))
      .filter(_.nonEmpty)
}
