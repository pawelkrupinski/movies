package services.cinemas.pl

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{AgeRating, CinemaScraper, ScraperParse, SlotsToMovies}
import tools.HttpFetch

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Łuków (Łukowski Ośrodek Kultury). The venue's own WordPress site replaced
 * Filmweb as the source: Filmweb carried a thin slice of the programme, and the
 * ekobilet.pl ticketing page only lists films whose sale has opened, while
 * `/repertuar/` lists every screening about five weeks ahead in one
 * server-rendered page.
 *
 * The page is a day-tab widget: one `div.kino-rep-panel[data-day]` per day, each
 * holding an `article.kino-film` per film screening that day (so a film recurs
 * once per day it plays). Per article:
 *   - `h3.kino-film-title a`      → title + the venue's `/movies/<slug>/` page
 *   - `a.kino-film-poster img`    → poster
 *   - `p.kino-film-genres`        → comma-separated genres
 *   - `p.kino-film-meta`          → "NNN minut" and the `span.kino-film-age`
 *     badge ("12+", "B.O.")
 *   - `p.kino-film-desc`          → the synopsis, cut short with "..." for all
 *     but the shortest; a cut one is dropped rather than stored half-finished
 *   - `li.kino-slot[data-at]`     → one screening, `data-at="YYYY-MM-DD HH:MM"`,
 *     with `span.kino-format` ("2D") and `span.kino-version` ("dubbing",
 *     "napisy", "polski"), and an `a.kino-slot-link` ekobilet booking link once
 *     sale has opened
 *
 * The film pages add only the uncut synopsis and a trailer — no year, director,
 * country or original title — so they are not fetched.
 */
class KinoLukowClient(http: HttpFetch, override val cinema: Cinema = KinoLukow) extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoLukowClient.RepertoireUrl)
  override def sourceUrl: Option[String] = Some(KinoLukowClient.RepertoireUrl)

  // A failed fetch propagates: swallowed, it would read as a venue with no
  // screenings — a white scrape instead of a red one.
  def fetch(): Seq[CinemaMovie] = KinoLukowClient.parse(http.get(KinoLukowClient.RepertoireUrl), cinema)
}

object KinoLukowClient {

  val BaseUrl       = "https://kino.lukow.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar/"

  private val SlotAt    = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
  private val Minutes   = """(\d{2,3})\s*minut""".r
  private val Truncated = """(?:\.\.\.|…)\s*$""".r

  private case class Slot(film: Element, title: String, showtime: Showtime)

  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val slots = Jsoup.parse(html, BaseUrl).select("article.kino-film").asScala.toSeq.flatMap { film =>
      Option(film.selectFirst("h3.kino-film-title")).map(_.text.trim).filter(_.nonEmpty).toSeq
        .flatMap(title => film.select("li.kino-slot[data-at]").asScala.toSeq.flatMap(showtime).map(Slot(film, title, _)))
    }

    SlotsToMovies.fold(slots, _.title, _.showtime) { (title, group, showtimes) =>
      val film = group.head.film
      def text(selector: String) = Option(film.selectFirst(selector)).map(_.text.trim).filter(_.nonEmpty)
      CinemaMovie(
        movie = Movie(
          title          = title,
          runtimeMinutes = text("p.kino-film-meta").flatMap(Minutes.findFirstMatchIn(_)).map(_.group(1).toInt),
          genres         = text("p.kino-film-genres").toSeq.flatMap(_.split(",")).map(_.trim).filter(_.nonEmpty)
        ),
        cinema    = cinema,
        posterUrl = Option(film.selectFirst("a.kino-film-poster img")).map(_.attr("abs:src")).filter(_.nonEmpty),
        filmUrl   = Option(film.selectFirst("h3.kino-film-title a")).map(_.attr("abs:href")).filter(_.nonEmpty),
        synopsis  = text("p.kino-film-desc").filter(Truncated.findFirstIn(_).isEmpty),
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes,
        ageRating = AgeRating.normalizeDroppingNoRestriction(text("span.kino-film-age"))
      )
    }
  }

  private def showtime(slot: Element): Option[Showtime] =
    Try(LocalDateTime.parse(slot.attr("data-at").trim, SlotAt)).toOption.map { at =>
      Showtime(
        dateTime   = at,
        bookingUrl = Option(slot.selectFirst("a.kino-slot-link")).map(_.attr("abs:href")).filter(_.nonEmpty),
        format     = ScraperParse.formatTokensIn(slot.select("span.kino-format, span.kino-version").asScala.map(_.text).mkString(" ").toLowerCase)
      )
    }
}
