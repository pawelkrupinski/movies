package services.cinemas.pl

import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

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
 *     room in the `fa-map-marker-alt` line — both captured (runtime as a hint /
 *     fallback before TMDB enriches; room onto the showtime for completeness).
 *
 * The listing has everything we display; there's no per-film detail page to
 * fetch (TMDB enriches the rest downstream). One `CinemaMovie` per title, with
 * the screenings merged and sorted.
 */
class KinoSpektrumClient(http: HttpFetch, override val cinema: Cinema) extends CinemaScraper {

  import KinoSpektrumClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(PageUrl)

  def fetch(): Seq[CinemaMovie] = parseHtml(http.get(PageUrl))

  def parseHtml(html: String): Seq[CinemaMovie] = {
    val slots = Jsoup.parse(html).select("div.event-item").asScala.toSeq.flatMap(parseEvent)

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking, s.room)) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title, runtimeMinutes = group.flatMap(_.runtime).headOption),
        cinema    = cinema,
        posterUrl = group.flatMap(_.poster).headOption,
        filmUrl   = None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = group.map(_.director).find(_.nonEmpty).getOrElse(Seq.empty),
        showtimes = showtimes
      )
    }
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
  // The event-item's `div.description` carries a labelled `Reżyseria: <names>`
  // line (then `Scenariusz…`, `Obsada…` on following `<br>` lines). Anchored on
  // the explicit label and bounded by the next tag, so the surrounding synopsis
  // prose can't leak in. The director list is comma-separated.
  private val DirectorPat = """(?iu)reżyseria\s*:?\s*([^<\n]+)""".r
  // The runtime sits in the `<i>NN''</i>` immediately after the `fa-history`
  // clock icon ("…fa-history\"></i> <i>124''</i>").
  private val RuntimePat = """(\d+)""".r

  private case class RawSlot(
    title:    String,
    dateTime: LocalDateTime,
    booking:  Option[String],
    poster:   Option[String],
    room:     Option[String],
    director: Seq[String],
    runtime:  Option[Int]
  )

  /** Runtime (minutes) from the `<i>NN''</i>` sibling of the `fa-history` icon. */
  private[cinemas] def parseRuntime(item: Element): Option[Int] =
    Option(item.selectFirst("i.fa-history"))
      .flatMap(i => Option(i.nextElementSibling))
      .filter(_.tagName == "i")
      .flatMap(e => RuntimePat.findFirstMatchIn(e.text)).map(_.group(1).toInt)

  /** Director(s) from the `Reżyseria:` line of the event-item description,
   *  comma-split. Empty when the description carries no such line. */
  private[cinemas] def parseDirector(item: Element): Seq[String] =
    Option(item.selectFirst("div.description")).map(_.html()).toSeq
      .flatMap(DirectorPat.findFirstMatchIn(_).map(_.group(1)))
      .flatMap(_.split(","))
      .map(_.trim.stripSuffix(".").trim)
      .filter(_.nonEmpty)

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
        Some(RawSlot(t, dt, booking, poster, room, parseDirector(item), parseRuntime(item)))
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
