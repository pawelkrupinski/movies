package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.HttpFetch

import java.time.LocalDateTime
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Spektrum (Gdańsk — part of the Klub Filmowy Kot, Trójmiasto city scope).
 * The cinema's ticketing system (`bilety.kinospektrum.pl`, the
 * "systembiletowy.pl" white-label) server-renders the whole repertoire on the
 * landing page (`/index.php`) — one `div.event-item` per screening, grouped
 * under day separators.
 *
 * Each `div.event-item` carries:
 *   - a `div.title > a` whose text is the clean film title and whose href is
 *     the booking URL (`/index.php/kup-bilet/<slug>-YYYY-MM-DD-HH-MM`). The
 *     slug tail encodes the exact date and start time, so the showtime is read
 *     straight off the URL — no locale month-name parsing needed, and any
 *     programme decoration baked into the slug ("…-klasyczne-srody-…") is
 *     ignored because the displayed anchor text is the clean title.
 *   - `img.list-repertoire-image` → poster (a site-relative `/uploads/...` URL).
 *   - the runtime in an `<i>NN''</i>` next to a `fa-history` icon, and the
 *     room in the `fa-map-marker-alt` line — runtime isn't surfaced (TMDB
 *     supplies it); room is captured onto the showtime for completeness.
 *
 * The listing has everything we display; there's no per-film detail page to
 * fetch (TMDB enriches the rest downstream). One `CinemaMovie` per title, with
 * the screenings merged and sorted.
 */
class KinoSpektrumClient(http: HttpFetch, override val cinema: Cinema) extends CinemaScraper {

  import KinoSpektrumClient._

  def fetch(): Seq[CinemaMovie] = parseHtml(http.get(PageUrl))

  def parseHtml(html: String): Seq[CinemaMovie] = {
    val slots = Jsoup.parse(html).select("div.event-item").asScala.toSeq.flatMap(parseEvent)

    slots.groupBy(_.title).toSeq.flatMap { case (title, group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, s.booking, s.room))
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
    }.sortBy(_.movie.title)
  }
}

object KinoSpektrumClient {

  val BaseUrl = "https://bilety.kinospektrum.pl"
  // `/index.php` serves the same server-rendered repertoire as the bare root;
  // pointing at the explicit path keeps the URL fixture-addressable.
  val PageUrl = s"$BaseUrl/index.php"

  // `…/kup-bilet/<slug>-YYYY-MM-DD-HH-MM` — the trailing date+time is the exact
  // start; the leading slug is decoration we ignore in favour of the anchor text.
  private val BookingTailPat = """-(\d{4})-(\d{2})-(\d{2})-(\d{2})-(\d{2})$""".r

  private case class RawSlot(
    title:    String,
    dateTime: LocalDateTime,
    booking:  Option[String],
    poster:   Option[String],
    room:     Option[String]
  )

  private def parseEvent(item: Element): Option[RawSlot] = {
    val link     = Option(item.selectFirst("div.title a[href]"))
    val href     = link.map(_.attr("href")).filter(_.nonEmpty)
    val title    = link.map(_.text.trim).filter(_.nonEmpty)
    val dateTime = href.flatMap(parseBookingDateTime)
    (title, dateTime) match {
      case (Some(t), Some(dt)) =>
        val booking = href.map(absolute)
        val poster  = Option(item.selectFirst("img.list-repertoire-image[src]"))
          .map(_.attr("src")).filter(_.nonEmpty).map(absolute)
        val room    = Option(item.selectFirst("i.fa-map-marker-alt"))
          .flatMap(i => Option(i.nextSibling)).map(_.toString.trim).filter(_.nonEmpty)
        Some(RawSlot(t, dt, booking, poster, room))
      case _ => None
    }
  }

  /** The exact start encoded in the booking-URL slug tail. */
  private def parseBookingDateTime(href: String): Option[LocalDateTime] =
    BookingTailPat.findFirstMatchIn(href).flatMap { m =>
      Try(LocalDateTime.of(
        m.group(1).toInt, m.group(2).toInt, m.group(3).toInt,
        m.group(4).toInt, m.group(5).toInt
      )).toOption
    }

  private def absolute(url: String): String =
    if (url.startsWith("http")) url else BaseUrl + (if (url.startsWith("/")) url else "/" + url)
}
