package services.cinemas.uk

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{AgeRating, CinemaScraper, OnlyMovieEventsFilter, ScraperParse, SlotsToMovies}
import tools.Whitespace.collapsedSpaces
import tools.{HttpFetch, HttpRead}

import java.time.LocalDate
import scala.jdk.CollectionConverters._
import scala.util.Try
import scala.util.matching.Regex

/**
 * Nova Cinema, Woking — a seven-screen independent Flicks lists with no times. It sells
 * through vivenu, which has no public listing API, but its own home page is
 * server-rendered with the whole programme, one `section.movie` per show:
 *
 * {{{
 * <section class="wrapper bg-light movie item w-100" data-nextscreening="2026-10-16 18:45:00">
 *   <figure><a href="/shows/verity.html"><img src="…/thumbs/Verity.jpg" srcset="…/Verity.jpg"></a></figure>
 *   <h4><img alt="15 Certificate"/><a href="/shows/verity.html">Verity</a></h4>
 *   <div><span>104<span> Minutes</span></span><span data-rt="104"> |</span><span> Thriller</span></div>
 *   <p class="… d-lg-inline">synopsis</p>
 *   <div class="day-block" data-date="2026-10-16">
 *     <div class="screening-card … " data-date="2026-10-16">
 *       <h5>6:45 pm</h5><div>Screen 5</div><div class="tags">AD, Sub</div>
 *       <a href="https://vivenu.com/event/…" class="… cta">Book Now</a></div>…
 * }}}
 *
 * One request is the venue's whole scrape (seven months ahead on 2026-10-12). The page
 * also sells the cinema's comedy night and a secret-film club; those are named in
 * [[venueEventMarkers]].
 */
class NovaCinemaClient(
  http:  HttpFetch,
  override val cinema: Cinema,
  today: => LocalDate
) extends CinemaScraper with OnlyMovieEventsFilter {

  import NovaCinemaClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(HomeUrl)
  override def sourceUrl: Option[String] = Some(HomeUrl)

  override protected def venueEventMarkers: Seq[Regex] = NotFilms

  protected def fetchUnfiltered(): Seq[CinemaMovie] = parse(HttpRead.page(http, HomeUrl), cinema, today)
}

object NovaCinemaClient {

  val HomeUrl = "https://www.novacinema.com/"

  /** Nova's monthly stand-up night, and its club whose film is a surprise: no title to
   *  list. Matched on the lower-cased title. */
  private val NotFilms: Seq[Regex] = Seq("""^nova laughs\b""".r, """^mystery movie club\b""".r)

  /** "Avengers: Doomsday | Infinity Vision", "Dune: Part Three | Preview": the strand
   *  Nova appends after a bar. */
  private val Strand = """\s+\|\s+[^|]+$""".r

  private val CertificateAlt = """(?i)^\s*(\S+)\s+certificate""".r
  private val Minutes = """(\d+)\s*Min""".r
  private val ScreenName = """(?i)screen\s+\d+""".r
  private val Subtitled ="""(?i)\bsub(title[sd]?)?\b""".r

  private case class Slot(show: Element, raw: String, title: String, showtime: Showtime)

  /** The home page → one [[CinemaMovie]] per show with a screening from `today` on. A
   *  page without its films list throws: that is a template change, not an empty
   *  programme. */
  def parse(html: String, cinema: Cinema, today: LocalDate): Seq[CinemaMovie] = {
    val page = Jsoup.parse(html, HomeUrl)
    require(page.selectFirst(".films") != null, "no '.films' list on the Nova home page — the template changed")

    val slots = for {
      show  <- page.select("section.movie").asScala.toSeq
      link  <- Option(show.selectFirst("h4 a")).toSeq
      raw    = link.text.collapsedSpaces
      title  = Strand.replaceFirstIn(raw, "").trim if title.nonEmpty
      card  <- show.select(".screening-card[data-date]").asScala.toSeq
      day   <- Try(LocalDate.parse(card.attr("data-date"))).toOption.toSeq if !day.isBefore(today)
      time  <- Option(card.selectFirst("h5")).flatMap(h => ScraperParse.meridiemClockIn(h.text)).toSeq
    } yield Slot(show, raw, title, Showtime(
      dateTime   = day.atTime(time),
      bookingUrl = Option(card.selectFirst("a.cta[href]")).map(_.attr("abs:href")).filter(_.nonEmpty),
      room       = Option(card.selectFirst("h5")).flatMap(h => Option(h.nextElementSibling)).map(_.text.trim)
        .filter(ScreenName.matches),
      format     = if (Option(card.selectFirst(".tags")).exists(t => Subtitled.findFirstIn(t.text).isDefined)) List("SUB") else Nil))

    SlotsToMovies.fold(slots, _.title, _.showtime) { (title, group, showtimes) =>
      val show = group.head.show
      val link = Option(show.selectFirst("h4 a"))
      CinemaMovie(
        movie = Movie(
          title          = title,
          rawTitle       = Some(group.head.raw).filterNot(_ == title),
          runtimeMinutes = Option(show.selectFirst("[data-rt]")).flatMap(rt => Try(rt.attr("data-rt").toInt).toOption).filter(_ > 0)
            .orElse(Minutes.findFirstMatchIn(show.text).map(_.group(1).toInt))
        ),
        cinema      = cinema,
        posterUrl   = Option(show.selectFirst("figure img[src]")).map(posterOf).filter(_.nonEmpty),
        filmUrl     = link.map(_.attr("abs:href")).filter(_.nonEmpty),
        synopsis    = Option(show.selectFirst("p.d-lg-inline")).map(_.text.trim).filter(_.nonEmpty),
        cast        = Seq.empty,
        director    = Seq.empty,
        showtimes   = showtimes,
        ageRating   = Option(show.selectFirst("h4 img[alt]")).flatMap(img => CertificateAlt.findFirstMatchIn(img.attr("alt")))
          .flatMap(m => AgeRating.normalize(m.group(1)))
      )
    }
  }

  /** The full-size image the `srcset` names, else the thumbnail. */
  private def posterOf(img: Element): String =
    img.attr("srcset").trim.split("\\s+").headOption.filter(_.nonEmpty)
      .flatMap(src => Try(java.net.URI.create(HomeUrl).resolve(src).toString).toOption)
      .getOrElse(img.attr("abs:src"))
}
