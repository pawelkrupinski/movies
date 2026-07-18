package services.cinemas.pl

import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.{LocalDate, LocalDateTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Krapkowice — the cinema run by the Krapkowicki Dom Kultury. Its
 * repertoire lives first-hand on the venue's own Drupal site at
 * `kdk.krapkowice.pl/kino`, server-rendered as a list of `li.latest-kino-item`
 * blocks, one per screening:
 *   - `div.kino-date time[datetime]` — the screening DATE ("2026-06-15T…");
 *     only the date part is reliable (the embedded time carries a spurious
 *     `Z`/offset), so we read the time elsewhere.
 *   - `div.kino-godziny`             — the real screening time as visible text
 *     ("19:00").
 *   - `div.kino-title`               — film title, carrying a trailing
 *     format/version tag ("… 2D Nap", "… 2D DUB") that
 *     [[ScraperParse.extractFormatTags]] peels into `Showtime.format` so the
 *     same film's dubbed + subtitled screenings merge into one row.
 * One fetch yields the full programme — no per-screening pages or booking links
 * (the site sells through one shared bilety24 "Kup bilet" button, not a
 * deep-link per slot). Titles are already mixed-case, so we strip the version
 * tag but keep the casing (unlike the ALL-CAPS MSI portals).
 *
 * Replaces the bilety24-organizer scrape (krapkowicki-dom-kultury-1244): the
 * venue's own page carries the same programme with one fewer third party
 * between us and the cinema. The Filmweb id (1681) stays as the fallback.
 */
class KdkKrapkowiceClient(http: HttpFetch, override val cinema: Cinema = KinoKrapkowice)
    extends CinemaScraper {

  import KdkKrapkowiceClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoUrl)
  override def sourceUrl: Option[String] = Some(KinoUrl)

  def fetch(): Seq[CinemaMovie] = parse(http.get(KinoUrl), cinema)
}

object KdkKrapkowiceClient {

  val BaseUrl = "https://kdk.krapkowice.pl"
  val KinoUrl = s"$BaseUrl/kino"

  private case class RawSlot(title: String, dateTime: LocalDateTime, format: List[String])

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(html, BaseUrl)

    val slots = document.select("li.latest-kino-item").asScala.toSeq.flatMap { item =>
      for {
        rawTitle <- Option(item.selectFirst("div.kino-title")).map(_.text)
        (title, tokens) = ScraperParse.extractFormatTags(rawTitle) if title.nonEmpty
        date <- Option(item.selectFirst("div.kino-date time[datetime]"))
                  .map(_.attr("datetime").take(10))
                  .flatMap(d => Try(LocalDate.parse(d)).toOption)
        time <- Option(item.selectFirst("div.kino-godziny")).flatMap(g => ScraperParse.parseHHmm(g.text))
      } yield RawSlot(title, LocalDateTime.of(date, time), tokens)
    }

    SlotsToMovies.fold(
      slots,
      titleOf    = _.title,
      showtimeOf = s => Showtime(s.dateTime, None, format = s.format),
      distinctBy = s => (s.dateTime, s.format)
    ) { (title, _, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }
}
