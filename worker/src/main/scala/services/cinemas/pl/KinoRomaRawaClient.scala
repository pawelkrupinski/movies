package services.cinemas.pl

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element}
import services.cinemas.common.{AgeRating, CinemaScraper, ListingPages, ScraperParse}
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Cyfrowe Kino Roma HD (Rawa Mazowiecka), run by the town's MDK on a bespoke
 * WordPress theme. `/kino/plan-seansow/` shows one `li.tile--film` card per
 * programmed film (schema.org `ScreeningEvent` microdata) linking to the
 * film's `/wpisy/<slug>/` post, plus a one-day calendar widget. The card only
 * gives a date RANGE; the post is where the exact times live:
 *
 *   - `div.single__schedule table tr` — one row per screening day: a year-less
 *     "30 września" `<time>` then one `HH:MM` `<time>` per showing. The year
 *     comes from `today` ([[ScraperParse.upcomingDate]]).
 *   - `hgroup[itemtype=schema.org/Movie]` — `genre`, `countryOfOrigin`,
 *     `dateCreated` (the production year), `director`, `typicalAgeRange`
 *     ("od 13 lat") and `duration` ("160 min") microdata.
 *   - `div.single__text[itemprop=description]` — the plot, followed by
 *     box-office notices (pre-sale dates, price list) that the venue sets as
 *     whole-paragraph `<strong>`; those paragraphs are dropped.
 *   - `.single__video iframe` — the YouTube trailer.
 *
 * So a scrape is the plan page plus one post per film (nine at capture time);
 * a post that fails is tolerated while any other answers
 * ([[ListingPages.requireAnyReached]]). Tickets are sold at the box office
 * only — there is no booking link to surface.
 */
class KinoRomaRawaClient(
  http:  HttpFetch,
  override val cinema: Cinema = KinoRomaRawa,
  today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import KinoRomaRawaClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(PlanUrl)

  def fetch(): Seq[CinemaMovie] = {
    val posts = filmPostUrls(http.get(PlanUrl)).map(url => url -> Try(http.get(url)))
    ListingPages.requireAnyReached(posts.map(_._2))
    posts.flatMap { case (url, page) => page.toOption.flatMap(parseFilmPost(_, url, today, cinema)) }
      .sortBy(_.movie.title)
  }
}

object KinoRomaRawaClient {

  val BaseUrl = "https://mdkrawa.pl"
  val PlanUrl = s"$BaseUrl/kino/plan-seansow/"

  private val Digits = """(\d+)""".r

  /** The `/wpisy/<slug>/` post of every film card on the plan, in page order. */
  private[pl] def filmPostUrls(html: String): Seq[String] =
    Jsoup.parse(html, BaseUrl).select("li.tile--film a.tile__link[href]").asScala.toSeq
      .map(_.attr("abs:href")).filter(_.nonEmpty).distinct

  private[pl] def parseFilmPost(html: String, url: String, today: LocalDate, cinema: Cinema): Option[CinemaMovie] = {
    val doc = Jsoup.parse(html, BaseUrl)
    val showtimes = schedule(doc, today)
    Option(doc.selectFirst("h1.single__title")).map(_.text.trim).filter(_.nonEmpty)
      .filter(_ => showtimes.nonEmpty)
      .map { title =>
        val movie = Option(doc.selectFirst("hgroup.single__header"))
        def props(name: String): Seq[String] =
          movie.toSeq.flatMap(_.select(s"[itemprop=$name]").asScala.toSeq)
            .flatMap(_.text.split(",")).map(_.trim).filter(_.nonEmpty)
        def prop(name: String): Option[String] = props(name).headOption
        CinemaMovie(
          movie = Movie(
            title          = title,
            runtimeMinutes = prop("duration").flatMap(Digits.findFirstIn).map(_.toInt).filter(_ > 0),
            releaseYear    = prop("dateCreated").flatMap(_.toIntOption),
            countries      = props("countryOfOrigin"),
            genres         = props("genre")
          ),
          cinema     = cinema,
          posterUrl  = Option(doc.selectFirst(".single__image img[src]")).map(_.attr("abs:src")).filter(_.nonEmpty),
          filmUrl    = Some(url),
          synopsis   = Option(doc.selectFirst("div.single__text[itemprop=description]")).map(plot).filter(_.nonEmpty),
          cast       = Seq.empty,
          director   = props("director"),
          showtimes  = showtimes,
          trailerUrl = Option(doc.selectFirst(".single__video iframe[src]")).map(_.attr("abs:src")).flatMap(ScraperParse.canonicalTrailer),
          ageRating  = prop("typicalAgeRange").flatMap(Digits.findFirstIn).flatMap(n => AgeRating.normalize(s"$n+"))
        )
      }
  }

  /** Every `(day, HH:MM)` of the post's schedule table, time-ordered. */
  private def schedule(doc: Document, today: LocalDate): Seq[Showtime] =
    doc.select(".single__schedule tr").asScala.toSeq.flatMap { row =>
      val cells = row.select("time").asScala.toSeq.map(_.text.trim)
      cells.headOption
        .flatMap(ScraperParse.parseDayMonth)
        .flatMap(ScraperParse.upcomingDate(_, today)).toSeq
        .flatMap(day => cells.drop(1).flatMap(ScraperParse.parseHHmm).map(t => Showtime(LocalDateTime.of(day, t), None)))
    }.distinctBy(_.dateTime).sortBy(_.dateTime)

  /** The description minus the venue's box-office notices: paragraphs set
   *  entirely in `<strong>` (pre-sale dates, prices, group bookings). */
  private def plot(description: Element): String = {
    val kept = description.clone()
    kept.select("p").asScala.filter(p => p.select("strong").text.trim == p.text.trim).foreach(_.remove())
    ScraperParse.cleanSynopsis(kept)
  }
}
