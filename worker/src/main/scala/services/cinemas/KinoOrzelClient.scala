package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.HttpFetch

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Orzeł — the art-house screen run by Miejskie Centrum Kultury in
 * Bydgoszcz, ticketed through the national Bilety24 marketplace. Unlike the
 * single-venue Bilety24 sites (Kino Luna, Kino Elektronik — see
 * `Bilety24Client`), Orzeł has no own-domain repertoire; its whole programme
 * lives on the dealer's organiser page
 * (`/kino/organizator/mck-kino-orzel-892`).
 *
 * That page is fully self-describing: each screening is one
 * `<a href="/kino/892-<slug>-<filmId>?id=<sessionId>">` whose `title`
 * attribute encodes everything we surface —
 *   `Film: <Title> - YYYY-MM-DD HH:MM - <City>`
 * — alongside a `.image` poster background. The same `<filmId>` repeats across
 * a film's screenings, so grouping by it folds the rows back into one
 * `CinemaMovie`. The bookable link is the sibling
 * `/kup-bilet-na-892-<slug>-<filmId>?id=<sessionId>` form, rebuilt per session.
 *
 * Everything we need is on the listing, so there's no per-film detail fetch;
 * runtime/genres/synopsis are filled downstream by TMDB enrichment.
 */
class KinoOrzelClient(http: HttpFetch, override val cinema: Cinema) extends CinemaScraper {

  import KinoOrzelClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] =
    parse(http.get(ListingUrl), cinema)
}

object KinoOrzelClient {

  val BaseUrl     = "https://www.bilety24.pl"
  // The dealer-organiser id for MCK Kino Orzeł on Bilety24 (`-892`).
  val OrganizerId = "892"
  val ListingUrl  = s"$BaseUrl/kino/organizator/mck-kino-orzel-$OrganizerId"

  // The film-listing anchor: `/kino/<org>-<slug>-<filmId>?id=<sessionId>`. The
  // trailing numeric group before `?id=` is the film id (stable across a film's
  // screenings); the `?id=` value is the per-screening session id.
  private val FilmHrefPat = s"""^/kino/$OrganizerId-.*-(\\d+)\\?id=(\\d+)$$""".r

  // The anchor's `title`: `Film: <Title> - YYYY-MM-DD HH:MM - <City>`. The
  // title itself can contain " - " (e.g. "KONWICKI: pisarz - scenarzysta …"),
  // so the non-greedy name group is anchored by the date that must follow the
  // separating dash — backtracking lands the split on the real date boundary.
  private val TitlePat     = """^Film:\s*(.+?)\s*-\s*(\d{4}-\d{2}-\d{2})\s+(\d{2}:\d{2})\s*-\s*.+$""".r
  private val DateTimeFmt  = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

  private case class RawSlot(filmId: String, title: String, dateTime: LocalDateTime, booking: String, poster: Option[String])

  /** Parse the organiser listing into the cinema's grouped films. Public for
    * the spec's fixture replay. */
  def parse(html: String, cinema: Cinema): Seq[CinemaMovie] = {
    val doc = Jsoup.parse(html)

    val slots = doc.select("a[href][title^=Film:]").asScala.toSeq.flatMap { a =>
      val href = a.attr("href")
      (FilmHrefPat.findFirstMatchIn(href), TitlePat.findFirstMatchIn(a.attr("title"))) match {
        case (Some(h), Some(t)) =>
          val filmId    = h.group(1)
          val sessionId = h.group(2)
          val title     = t.group(1).trim
          Try(LocalDateTime.parse(s"${t.group(2)} ${t.group(3)}", DateTimeFmt)).toOption
            .filter(_ => title.nonEmpty)
            .map { dt =>
              val slug    = href.stripPrefix(s"/kino/$OrganizerId-").takeWhile(_ != '?')
              val booking = s"$BaseUrl/kup-bilet-na-$OrganizerId-$slug?id=$sessionId"
              val poster  = Option(a.selectFirst(".image")).flatMap(e => ScraperParse.cssUrl(e.attr("style")))
              RawSlot(filmId, title, dt, booking, poster)
            }
        case _ => None
      }
    }

    slots
      .groupBy(_.filmId)
      .toSeq
      .flatMap { case (_, group) =>
        val title     = group.head.title
        val showtimes = group
          .map(s => Showtime(s.dateTime, Some(s.booking)))
          .distinctBy(s => (s.dateTime, s.bookingUrl))
          .sortBy(_.dateTime)
        if (showtimes.isEmpty) None
        else Some(CinemaMovie(
          movie     = Movie(title),
          cinema    = cinema,
          posterUrl = group.flatMap(_.poster).headOption,
          filmUrl   = None,
          synopsis  = None,
          cast      = Seq.empty,
          director  = Seq.empty,
          showtimes = showtimes
        ))
      }
      .sortBy(_.movie.title)
  }
}
