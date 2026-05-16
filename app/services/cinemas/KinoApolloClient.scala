package services.cinemas

import models.{Cinema, CinemaMovie, KinoApollo, Movie, Showtime}
import tools.{HttpFetch, RealHttpFetch}

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.util.Try

/**
 * Kino Apollo (kinoapollo.pl/kino) — a small Poznań cinema running on WordPress +
 * Elementor + Jet Engine. There's no public JSON/REST endpoint, so we scrape the
 * single repertoire page. Each screening is rendered twice (desktop & mobile
 * variants are both in the DOM); we deduplicate by ticketing URL.
 *
 * Layout (within one event block):
 *   1. Day heading (`dd.mm.yyyy` + a Polish weekday) precedes the block
 *   2. Time link  — `<a href=".../event/view/id/N"><span>HH:MM</span>`
 *   3. Title      — `itemprop="name"` div with the film name
 *   4. Synopsis   — a `jet-listing-dynamic-field__content` block of paragraphs
 *   5. Poster     — `<img>` to a WordPress media URL
 *   6. "Kup bilet" button (same booking URL as the time link)
 */
class KinoApolloClient(http: HttpFetch = new RealHttpFetch()) extends CinemaScraper {

  val cinema: Cinema = KinoApollo
  // Production redirects from /kino to /kino/ — request the canonical-shaped URL
  // directly. The FakeHttpFetch can't traverse a trailing slash to a file.
  private val PageUrl = "https://kinoapollo.pl/kino"
  private val DateFmt = DateTimeFormatter.ofPattern("dd.MM.yyyy")

  // Every Jet-Engine dynamic link to the ticketing site has a label span. The
  // time link's label is `HH:MM`; the same event also renders a "Kup bilet"
  // button pointing to the same URL. The film title sits *between* those two
  // anchors inside the event's card — that's our anchor pair for the title.
  private val EventLinkPat = """<a href="(https://bilety\.kinoapollo\.pl/event/view/id/\d+)"[^>]*>\s*<span class="jet-listing-dynamic-link__label">([^<]+)</span>""".r
  // `itemprop="name"` wrapper for the film title.
  // (?s) so `.*?` crosses newlines — the wrapper and the inner content div sit
  // on separate lines.
  private val TitlePat     = """(?s)itemprop="name"[^>]*>.*?jet-listing-dynamic-field__content[^>]*>([^<]+)</div>""".r
  // Day heading content (just the date — the weekday text is in a sibling field).
  private val DatePat      = """jet-listing-dynamic-field__content[^>]*>(\d{1,2}\.\d{1,2}\.\d{4})</div>""".r
  // WordPress media poster URL.
  private val PosterPat    = """https://kinoapollo\.pl/wp-content/uploads/\d{4}/\d{2}/[^\s'"\)]+\.(?:jpg|jpeg|png)""".r
  private val TimeOnlyPat  = """^\d{1,2}:\d{2}$""".r

  def fetch(): Seq[CinemaMovie] = parseHtml(http.get(PageUrl))

  def parseHtml(html: String): Seq[CinemaMovie] = {
    val links  = EventLinkPat.findAllMatchIn(html)
      .map(m => LinkOccurrence(m.start, m.group(1), m.group(2))).toList
    val titles = TitlePat.findAllMatchIn(html).map(m => (m.start, m.group(1).trim)).toList
    val dates  = DatePat.findAllMatchIn(html).map(m => (m.start, m.group(1))).toList

    // Each event renders four anchors on the page (desktop time + desktop "Kup
    // bilet" + same pair for mobile). Group by URL, take the first pair: the
    // title that falls between the time anchor and its "Kup bilet" sibling
    // belongs to that event.
    val events = links.groupBy(_.url).toSeq.flatMap { case (url, occurrences) =>
      val sorted = occurrences.sortBy(_.offset)
      for {
        timeOcc <- sorted.find(o => TimeOnlyPat.matches(o.label))
        kupOcc  <- sorted.find(o => o.offset > timeOcc.offset && !TimeOnlyPat.matches(o.label))
        date    <- dates.takeWhile { case (o, _) => o <= timeOcc.offset }.lastOption.map(_._2)
        title   <- titles.find { case (o, _) => o > timeOcc.offset && o < kupOcc.offset }.map(_._2)
        dt      <- Try(LocalDateTime.of(LocalDate.parse(date, DateFmt), LocalTime.parse(timeOcc.label))).toOption
      } yield {
        val cardEnd = sorted.lastOption.map(_.offset + 200).getOrElse(kupOcc.offset)
        val poster  = pickPoster(PosterPat.findAllIn(html.substring(timeOcc.offset, cardEnd)).toSeq)
        ScreeningRow(url, dt, cleanTitle(title), poster)
      }
    }

    events
      .groupBy(_.title)
      .toSeq
      .map { case (title, rows) =>
        val sorted = rows.sortBy(_.dateTime)
        CinemaMovie(
          movie     = Movie(title),
          cinema    = KinoApollo,
          posterUrl = sorted.flatMap(_.posterUrl).headOption,
          filmUrl   = None,
          synopsis  = None,
          cast      = None,
          director  = None,
          showtimes = sorted.map(r => Showtime(r.dateTime, Some(r.bookingUrl)))
        )
      }
      .sortBy(_.movie.title)
  }

  // Strip event/promo decoration so the same film with a "seans przedpremierowy"
  // (pre-premiere) screening or a "DZIEŃ DZIECKA W APOLLO - ..." (children's-day
  // banner) screening collapses into the same Movie as the regular run. Without
  // this, those decorated rows can't be enriched — TMDB's title search returns
  // nothing for the decorated string and the row stays at tmdbId=None.
  def cleanTitle(title: String): String =
    title
      .stripPrefix("DZIEŃ DZIECKA W APOLLO - ")
      .stripSuffix(" - seans przedpremierowy")

  // WordPress generates many size variants for each poster (e.g.
  // `..._plakat-200x300.jpg`, `..._plakat-683x1024.jpg`, `..._plakat-scaled.jpg`,
  // `..._plakat.jpg`). Pick the largest by height: scaled/no-suffix beat sized,
  // sized are ranked by height number, anything else gets 0.
  private val SizedVariant = """-(\d+)x(\d+)\.[A-Za-z]+$""".r
  private val FullVariant  = """(?:-scaled)?\.[A-Za-z]+$""".r

  private def pickPoster(urls: Seq[String]): Option[String] =
    if (urls.isEmpty) None
    else Some(urls.distinct.maxBy(posterRank))

  private def posterRank(url: String): Int =
    SizedVariant.findFirstMatchIn(url).map(_.group(2).toInt).getOrElse {
      if (url.contains("-scaled.")) 99999  // "-scaled" is the original full-size
      else if (FullVariant.findFirstMatchIn(url).isDefined) 99998  // unsuffixed original
      else 0
    }

  private case class LinkOccurrence(offset: Int, url: String, label: String)

  private case class ScreeningRow(
    bookingUrl: String,
    dateTime:   LocalDateTime,
    title:      String,
    posterUrl:  Option[String]
  )
}

