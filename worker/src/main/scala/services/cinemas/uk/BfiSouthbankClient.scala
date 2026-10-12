package services.cinemas.uk

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{CinemaScraper, ListingPages, ScraperParse, SlotsToMovies}
import services.cinemas.pl.OnlyMovieEventsFilter
import tools.{HttpFetch, HttpRead}

import java.net.URLDecoder
import java.nio.charset.StandardCharsets.UTF_8
import java.time.{Duration, LocalDate}
import scala.annotation.tailrec
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

/**
 * BFI Southbank — the BFI's four NFT screens, which Flicks lists with no times. The BFI's
 * box office (`whatson.bfi.org.uk`) refuses our requests, but its new cinemas site,
 * `cinemas.bfi.org.uk`, is server-rendered and answers. It is the BFI's own pre-launch
 * build (served `noindex`, not yet linked from bfi.org.uk, its Sentry tag flipping between
 * "staging" and "production" while it is deployed), so it may move; the venue keeps Flicks
 * as its fallback, so if it does, nothing is lost that Flicks had.
 *
 * Two kinds of page:
 *
 *   1. `GET /whats-on/show-list-page.partial?page=N` — 24 film cards a page, from 1 until
 *      a page has none: title, `/whats-on/<slug>`, synopsis, runtime (`PT1H47M`), year,
 *      image, BBFC badge.
 *   2. `GET /whats-on/<slug>` — the film's performances in `section#shows`, under a heading
 *      per day:
 *      {{{
 *      <h3>Fri 23 Oct</h3>
 *      <div class="performanceDetails" data-ga-vanue="BFI Southbank" …>
 *        <a href="https://whatson.bfi.org.uk/Online/login.asp?…" aria-label="20:40 Friday 23 October NFT3">20:40</a>
 *        … <p>NFT3</p>
 *      }}}
 *      A sold-out performance draws `<button class="disabled">18:00</button>` instead of the
 *      link — no booking URL, no label — and is kept, as a sold-out showing is still showing
 *      (Ghosts: The Possession of Button House's only one, 2026-10-12). So the day is read off
 *      the heading, which carries no year and is placed by [[ScraperParse.upcomingDate]], the
 *      time off the link or button, and the screen off its own line. (Some pages also carry a
 *      `<bfi-calendar>` with ISO times, but not all: Deep Cover's seven performances are only
 *      in the blocks.)
 *
 * About 150 requests a scrape (117 films on 2026-10-12, seven weeks ahead), the film pages
 * read two at a time; one that fails drops only its film and leaves the listing
 * incomplete. The same site sells the BFI IMAX at Waterloo, a venue of its own that reads
 * Flicks: only performances whose venue is BFI Southbank, on one of its NFT screens, are
 * kept — not the talks it sells in the Reuben Library.
 */
class BfiSouthbankClient(
  http:  HttpFetch,
  override val cinema: Cinema,
  today: => LocalDate
) extends CinemaScraper with OnlyMovieEventsFilter {

  import BfiSouthbankClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(s"$BaseUrl/whats-on")

  protected def fetchUnfiltered(): Seq[CinemaMovie] = {
    val day   = today
    val cards = listCards(1, Vector.empty)
    val dates = ListingPages.readMore("bfi-southbank-films", cards.map(_.slug), filmUrl)(url =>
      performances(HttpRead.page(http, url), day)).toMap
    films(cards, dates, cinema)
  }

  /** Every film card, page by page until one comes back empty. Page 1 failing fails the
   *  scrape; a later one failing ends the walk with the listing marked incomplete. */
  @tailrec
  private def listCards(page: Int, acc: Vector[Card]): Vector[Card] =
    if (page > MaxPages) acc
    else Try(HttpRead.page(http, listUrl(page))) match {
      case Failure(e) if page == 1 => throw e
      case Failure(e)              => ListingPages.reportFailed(Seq(Failure(e))); acc
      case Success(html) =>
        val cards = parseCards(html).filterNot(c => acc.exists(_.slug == c.slug))
        if (cards.isEmpty) acc else listCards(page + 1, acc ++ cards)
    }
}

object BfiSouthbankClient {

  val BaseUrl = "https://cinemas.bfi.org.uk"

  def listUrl(page: Int): String = s"$BaseUrl/whats-on/show-list-page.partial?page=$page"
  def filmUrl(slug: String): String = s"$BaseUrl/whats-on/$slug"

  /** The venue the site gives the Southbank's own screens. */
  val Venue = "BFI Southbank"

  /** A guard on the page walk: the site lists five or six pages. */
  private val MaxPages = 40

  /** The four NFT screens. The site also sells talks in the BFI Reuben Library. */
  private val Screen = """NFT\s*\d+""".r

  private val ImageProxyHref = """/_image\?(?:[^#]*&)?href=([^&]+)""".r

  private val Slug = """^/whats-on/([a-z0-9][a-z0-9-]*)$""".r

  /** A day heading, "Fri 23 Oct": weekday, day, month. */
  private val DayHeading = """^\s*[A-Za-z]+\s+(\d{1,2})\s+([A-Za-z]+)\s*$""".r

  /** The BBFC badge the card draws as an inline SVG, its `<title>BBFC 15_RGB</title>`
   *  URL-encoded in the `data:` source. */
  private val BbfcBadge = """(?i)BBFC(?:%20|\s)(U|PG|12A|12|15|18|R18)_""".r

  final case class Card(slug: String, title: String, synopsis: Option[String], runtime: Option[Int],
                        year: Option[Int], image: Option[String], certificate: Option[String] = None)

  private case class Slot(card: Card, showtime: Showtime)

  /** One list page's film cards. */
  def parseCards(html: String): Seq[Card] =
    Jsoup.parse(html, BaseUrl).select("article.showCard").asScala.toSeq.flatMap { card =>
      for {
        link  <- Option(card.selectFirst("a.card-link[href]"))
        slug  <- Slug.findFirstMatchIn(link.attr("href")).map(_.group(1))
        title <- Some(link.text.replaceAll("\\s+", " ").trim).filter(_.nonEmpty)
      } yield Card(
        slug     = slug,
        title    = title,
        synopsis = Option(card.selectFirst(".cardContent p")).map(_.text.trim).filter(_.nonEmpty),
        runtime  = Option(card.selectFirst("time[datetime^=PT]"))
          .flatMap(t => Try(Duration.parse(t.attr("datetime")).toMinutes.toInt).toOption).filter(_ > 0),
        year     = Option(card.selectFirst(".workData")).flatMap(yearOf),
        image    = Option(card.selectFirst(".showImage img[src]")).map(_.attr("abs:src")).filter(_.nonEmpty).map(original),
        certificate = card.select(".workData img[src^=data:]").asScala.flatMap(i => BbfcBadge.findFirstMatchIn(i.attr("src"))).headOption
          .map(_.group(1).toUpperCase(java.util.Locale.ROOT))
      )
    }

  /** The CMS original behind the site's resizing proxy (`/_image?href=<original>&w=1280…`). */
  private def original(src: String): String =
    ImageProxyHref.findFirstMatchIn(src).flatMap(m => Try(URLDecoder.decode(m.group(1), UTF_8)).toOption).getOrElse(src)

  /** The year the card's information row gives as a span of its own ("1989"). */
  private def yearOf(row: Element): Option[Int] =
    row.select("span").asScala.map(_.text.trim).collectFirst { case y if y.matches("\\d{4}") => y }
      .flatMap(ScraperParse.yearIn)

  /** A film page's performances on a Southbank screen from `today` on: each block in
   *  `section#shows` on the day of the heading nearest before it. */
  def performances(html: String, today: LocalDate): Seq[Showtime] = {
    val shows = Jsoup.parse(html, BaseUrl).select("section#shows h3, section#shows div.performanceDetails").asScala.toSeq
    val dated = shows.scanLeft((Option.empty[LocalDate], Option.empty[Element])) {
      case (_, heading) if heading.tagName == "h3" => (dayOf(heading.text, today), None)
      case ((day, _), block)                       => (day, Some(block))
    }
    dated.collect { case (Some(day), Some(block)) if !day.isBefore(today) && block.attr("data-ga-vanue").trim.equalsIgnoreCase(Venue) => (day, block) }
      .flatMap { case (day, block) =>
        for {
          sale   <- Option(block.selectFirst("a, button"))
          time   <- ScraperParse.parseHHmm(sale.text)
          screen <- block.select("p").asScala.map(_.text.trim).find(Screen.matches)
        } yield Showtime(
          dateTime   = day.atTime(time),
          bookingUrl = Some(sale.attr("abs:href")).filter(_.startsWith("http")),
          room       = Some(screen))
      }.distinct
  }

  /** The date a day heading ("Fri 23 Oct") names, placed in the year that keeps it upcoming. */
  private def dayOf(heading: String, today: LocalDate): Option[LocalDate] =
    for {
      m        <- DayHeading.findFirstMatchIn(heading)
      month    <- ScraperParse.englishMonth(m.group(2))
      dayMonth <- ScraperParse.monthDay(m.group(1).toInt, month)
      day      <- ScraperParse.upcomingDate(dayMonth, today)
    } yield day

  /** The cards joined to their film pages' performances (by slug): one [[CinemaMovie]] per
   *  film with any. */
  def films(cards: Seq[Card], dates: Map[String, Seq[Showtime]], cinema: Cinema): Seq[CinemaMovie] = {
    val slots = for {
      card     <- cards
      showtime <- dates.getOrElse(card.slug, Nil)
    } yield Slot(card, showtime)

    SlotsToMovies.fold(slots, _.card.title, _.showtime) { (title, group, showtimes) =>
      val card = group.head.card
      CinemaMovie(
        movie       = Movie(title, runtimeMinutes = card.runtime, releaseYear = card.year),
        cinema      = cinema,
        posterUrl   = card.image,
        filmUrl     = Some(filmUrl(card.slug)),
        synopsis    = card.synopsis,
        cast        = Seq.empty,
        director    = Seq.empty,
        showtimes   = showtimes,
        externalIds = Map("bfi" -> card.slug),
        ageRating   = card.certificate
      )
    }
  }
}
