package services.cinemas.pl

import services.cinemas.common.{CinemaScraper, ScrapeHorizon, ScraperParse}
import models._
import play.api.libs.json.Json
import tools.{HttpFetch, ParallelDetailFetch}
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Bajka, the cinema of Centrum Kultury w Błoniu (`kino.blonie.pl`, a bespoke
 * WordPress theme). The day-by-day repertoire (`/?repertuar_date=YYYY-MM-DD`) shows
 * only title + times for ONE day, so walking it would mean fetching every calendar
 * day blind. Instead each film's own page is the source:
 *
 *   - `/filmy/` lists every film with screenings; the homepage adds its announced
 *     ("Zapowiedzi") films. Their `/film/<slug>/` links are unioned.
 *   - The film page carries the identity: `h1` title (shouted — `LALKA`), the poster,
 *     the `div.prose` synopsis, a YouTube trailer, and a `ul.film-meta-list` of
 *     icon-labelled facts in a fixed order — version (`PL`/`DUBBING`/`NAPISY`),
 *     genres (`Dramat / Obyczajowy`), runtime (`2 godz. 42 min.`), age (`Od lat: 12`),
 *     country (`POLSKA`/`USA`, often omitted), price (`Bilety: …`). The items carry no
 *     class telling them apart, so they are recognised by shape: genres are what
 *     precedes the runtime, the age slot the item right after it (`Bez ograniczeń`
 *     when unrated), countries what follows the age slot.
 *   - Its "Seanse" carousel renders only four days; the rest load through
 *     `admin-ajax.php?action=base_cinema_film_dates`, keyed by the page's
 *     `data-film-id`. One POST with `limit` = [[ScrapeHorizon.MaxDays]] and the
 *     cursor on yesterday returns every upcoming day at once, as the same
 *     `article.film-showtime-day-card` markup (`D.MM.YYYY` + `span.showtime-pill`s).
 *
 * The site publishes no director or production year. Tickets are sold at the box
 * office only, so no booking URL is surfaced.
 */
class KinoBajkaBlonieClient(
  http:  HttpFetch,
  override val cinema: Cinema = KinoBajkaBlonie,
  today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import KinoBajkaBlonieClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(HomeUrl)
  override def sourceUrl: Option[String] = Some(HomeUrl)

  // The two listings propagate their failures (a red scrape, not a white one); a
  // single film page that fails drops only that film, like every
  // ParallelDetailFetch client.
  def fetch(): Seq[CinemaMovie] = {
    val filmUrls = Seq(FilmsUrl, HomeUrl).map(http.get).flatMap(filmLinks).distinct
    val byUrl = ParallelDetailFetch("kino-blonie", filmUrls, 30.seconds) { url =>
      Try(filmOf(url)).toOption.flatten
    }
    filmUrls.flatMap(byUrl.get).flatten.filter(_.showtimes.nonEmpty).sortBy(_.movie.title)
  }

  private def filmOf(url: String): Option[CinemaMovie] = {
    val page = Jsoup.parse(http.get(url), url)
    filmIdOf(page).flatMap { id =>
      parseFilm(page, url, showtimesOf(http.post(AjaxUrl, datesBody(id, today), FormContentType)), cinema)
    }
  }
}

object KinoBajkaBlonieClient {

  val HomeUrl  = "https://kino.blonie.pl/"
  val FilmsUrl = s"${HomeUrl}filmy/"
  val AjaxUrl  = s"${HomeUrl}wp-admin/admin-ajax.php"

  private val FormContentType = "application/x-www-form-urlencoded"

  private val FilmLink = """^https://kino\.blonie\.pl/film/[^/]+/?$""".r
  private val Runtime  = """(?:(\d+)\s*godz\.?)?\s*(?:(\d+)\s*min)""".r
  private val AgeFrom  = """(?i)od\s+lat:?\s*(\d+)""".r
  private val DayDate  = """(\d{1,2})\.(\d{1,2})\.(\d{4})""".r

  /** The film-dates POST: every upcoming day after yesterday, up to the shared horizon. */
  def datesBody(filmId: String, today: LocalDate): String =
    s"action=base_cinema_film_dates&film_id=$filmId&direction=next&cursor_date=${today.minusDays(1)}&limit=${ScrapeHorizon.MaxDays}"

  def filmLinks(html: String): Seq[String] =
    Jsoup.parse(html, HomeUrl).select("a[href*=/film/]").asScala.toSeq
      .map(_.attr("abs:href"))
      .filter(FilmLink.findFirstIn(_).isDefined)
      .distinct

  private def filmIdOf(page: Document): Option[String] =
    Option(page.selectFirst("[data-cinema-film-dates][data-film-id]")).map(_.attr("data-film-id")).filter(_.nonEmpty)

  /** Showtimes from the film-dates answer: `{success, data: {html}}` whose html
   *  holds one `article.film-showtime-day-card` per day. */
  def showtimesOf(json: String): Seq[LocalDateTime] = {
    val html = (Json.parse(json) \ "data" \ "html").asOpt[String].getOrElse("")
    Jsoup.parse(html).select("article.film-showtime-day-card").asScala.toSeq.flatMap { card =>
      val date = DayDate.findFirstMatchIn(card.select(".film-showtime-day-card__date").text).flatMap { m =>
        Try(LocalDate.of(m.group(3).toInt, m.group(2).toInt, m.group(1).toInt)).toOption
      }
      date.toSeq.flatMap { d =>
        card.select("span.showtime-pill").asScala.toSeq.flatMap(p => ScraperParse.parseHHmm(p.text.trim)).map(LocalDateTime.of(d, _))
      }
    }.distinct.sorted
  }

  def parseFilm(page: Document, url: String, dateTimes: Seq[LocalDateTime], cinema: Cinema): Option[CinemaMovie] =
    Option(page.selectFirst("h1")).map(_.text.trim).filter(_.nonEmpty).map { rawTitle =>
      val facts    = page.select("ul.film-meta-list .film-meta-list__text").asScala.toSeq.map(_.text.trim).filter(_.nonEmpty)
      val runtimeAt = facts.indexWhere(f => runtimeOf(f).isDefined)
      // The age slot follows the runtime: "Od lat: 12", or "Bez ograniczeń" (no rating).
      val ageAt     = if (runtimeAt < 0) -1 else runtimeAt + 1
      val format    = facts.headOption.map(ScraperParse.formatTokensIn).getOrElse(Nil)
      // Before the runtime: the version badge, then the genres ("Dramat / Obyczajowy").
      val genres    = (if (runtimeAt < 0) Seq.empty else facts.take(runtimeAt).drop(1))
                        .flatMap(_.split("/")).map(_.trim).filter(_.nonEmpty)
      // After the age rating: the countries, then the ticket price.
      val countries = (if (ageAt < 0) Seq.empty else facts.drop(ageAt + 1))
                        .filterNot(_.startsWith("Bilety"))
                        .flatMap(_.split("[,/]")).map(_.trim).filter(_.nonEmpty)
      val title     = ScraperParse.sentenceCase(rawTitle)
      CinemaMovie(
        movie       = Movie(
          title          = title,
          runtimeMinutes = facts.lift(runtimeAt).flatMap(runtimeOf),
          countries      = countries,
          genres         = genres,
          rawTitle       = Option(rawTitle).filter(_ != title)
        ),
        cinema      = cinema,
        posterUrl   = Option(page.selectFirst("img.film-poster")).map(_.attr("abs:src")).filter(_.nonEmpty),
        filmUrl     = Some(url),
        synopsis    = Option(page.selectFirst("div.prose")).map(_.text.trim).filter(_.nonEmpty),
        cast        = Seq.empty,
        director    = Seq.empty,
        showtimes   = dateTimes.map(Showtime(_, None, format = format)),
        trailerUrl  = trailerOf(page),
        ageRating   = facts.lift(ageAt).flatMap(AgeFrom.findFirstMatchIn).map(m => s"${m.group(1)}+")
      )
    }

  /** "2 godz. 42 min." → 162; "96 min." → 96. */
  private def runtimeOf(fact: String): Option[Int] =
    Runtime.findFirstMatchIn(fact).map { m =>
      Option(m.group(1)).map(_.toInt * 60).getOrElse(0) + m.group(2).toInt
    }

  private def trailerOf(page: Element): Option[String] =
    page.select("iframe[src], [data-src]").asScala.toSeq
      .flatMap(e => Seq(e.attr("src"), e.attr("data-src")))
      .filter(_.nonEmpty)
      .flatMap(ScraperParse.canonicalTrailer)
      .headOption
}
