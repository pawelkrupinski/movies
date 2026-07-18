package services.cinemas.pl

import scala.math.Ordering.Implicits.infixOrderingOps
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Element, TextNode}
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Narodowe Centrum Kultury Filmowej (NCKF) — the cinematheque run by EC1 Łódź
 * inside the repurposed former power station at ul. Targowa 1/3. The venue's
 * repertoire is published at
 * `ec1lodz.pl/narodowe-centrum-kultury-filmowej/repertuar-kina/` on a
 * server-rendered TYPO3 page that embeds the complete upcoming schedule.
 *
 * Each `div.events-archive__item.cinema.active` is a distinct showing event;
 * a single film may appear across multiple such divs (one per showing day).
 * Within each event wrapper the schedule block `div.daty-godz-seansow` repeats
 * once per screening slot and contains:
 *   - `span.daty-godz-seansow__date-full` (hidden) — the ISO date
 *     `YYYY-MM-DD`, which avoids any year-inference ambiguity.
 *   - `div.daty-godz-seansow__hour` — time as `HH:MM`.
 *   - `a.daty-godz-seansow__url[href]` — booking URL on
 *     `bilety.ec1lodz.pl`; empty `href` means "tickets coming soon"
 *     (`Wkrótce`), in which case `bookingUrl` is `None`.
 *   - Trailing text after the `</a>` tag, up to a `<!-- Sala, … -->` comment,
 *     giving the room name (`Odeon`, `Luna`, `Urania`); `Wkrótce` signals a
 *     pending slot and is NOT treated as a room name.
 *
 * Film-level fields:
 *   - `h3` — title (programme prefixes left intact for downstream normalisation).
 *   - `img[src]` inside `div.events-archive__item-image` — poster (relative
 *     path, prepend BaseUrl).
 *   - `a.events-archive__item-url[href]` — relative link to the event detail
 *     page; prepend BaseUrl to form `filmUrl`.
 *
 * Past events (date before `today`) are silently dropped. Showings are grouped
 * by title so that a film screened over several days yields one `CinemaMovie`
 * with all upcoming `Showtime` entries.
 */
class NckfClient(
  http:             HttpFetch,
  override val cinema: Cinema,
  today:            LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper {

  import NckfClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = parseHtml(http.get(RepertoireUrl), today, cinema)
}

object NckfClient {

  val BaseUrl       = "https://ec1lodz.pl"
  val RepertoireUrl = s"$BaseUrl/narodowe-centrum-kultury-filmowej/repertuar-kina/"

  /** The room name follows the booking anchor but is NOT "Wkrótce"
    * (a placeholder meaning "coming soon" — that slot has no real room yet). */
  private val WkrotceSentinel = "Wkrótce"

  private[cinemas] case class RawSlot(
    title:      String,
    dateTime:   LocalDateTime,
    bookingUrl: Option[String],
    room:       Option[String],
    posterUrl:  Option[String],
    filmUrl:    Option[String]
  )

  private[cinemas] def parseHtml(html: String, today: LocalDate, cinema: Cinema): Seq[CinemaMovie] = {
    val document   = Jsoup.parse(html)
    val items = document.select("div.events-archive__item.cinema.active").asScala.toSeq
    val slots = items.flatMap(parseItem(_, today))

    SlotsToMovies.fold(
      slots,
      titleOf    = _.title,
      showtimeOf = s => Showtime(s.dateTime, s.bookingUrl, s.room),
      distinctBy = s => (s.dateTime, s.room)
    ) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = group.flatMap(_.posterUrl).headOption,
        filmUrl   = group.flatMap(_.filmUrl).headOption,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }

  private def parseItem(item: Element, today: LocalDate): Seq[RawSlot] = {
    val title  = Option(item.selectFirst("h3")).map(_.text.trim).filter(_.nonEmpty)
    val poster = Option(item.selectFirst("div.events-archive__item-image img[src]"))
                   .map(_.attr("src")).filter(_.nonEmpty)
                   .map(p => if (p.startsWith("http")) p else BaseUrl + p)
    val filmUrl = Option(item.selectFirst("a.events-archive__item-url[href]"))
                    .map(_.attr("href")).filter(_.nonEmpty)
                    .map(h => if (h.startsWith("http")) h else BaseUrl + h)

    title match {
      case None => Seq.empty
      case Some(t) =>
        item.select("div.daty-godz-seansow").asScala.toSeq.flatMap { slot =>
          parseSlot(slot, t, poster, filmUrl, today)
        }
    }
  }

  /** Parse one `div.daty-godz-seansow` into a `RawSlot`, returning `None` for
    * past dates or unparseable data. */
  private def parseSlot(
    slot:    Element,
    title:   String,
    poster:  Option[String],
    filmUrl: Option[String],
    today:   LocalDate
  ): Option[RawSlot] = {
    val dateStr = Option(slot.selectFirst("span.daty-godz-seansow__date-full"))
                    .map(_.text.trim).filter(_.nonEmpty)
    val hourStr = Option(slot.selectFirst("div.daty-godz-seansow__hour"))
                    .map(_.text.trim).filter(_.nonEmpty)
    val anchor  = Option(slot.selectFirst("a.daty-godz-seansow__url"))
    val booking = anchor.map(_.attr("href")).filter(_.nonEmpty)

    // The room name is the text node directly after the anchor in the slot div.
    // It may be "Wkrótce" (no confirmed room yet) — treat as None in that case.
    val room: Option[String] = {
      val textAfterAnchor = anchor.flatMap { a =>
        val siblings = a.parent.childNodes.asScala.toList
        val aIndex     = siblings.indexOf(a)
        siblings.drop(aIndex + 1)
          .collect { case tn: TextNode => tn.text.trim }
          .find(_.nonEmpty)
      }
      textAfterAnchor.filter(r => r.nonEmpty && r != WkrotceSentinel)
    }

    for {
      ds <- dateStr
      hs <- hourStr
      d  <- Try(LocalDate.parse(ds)).toOption
      if d >= today
      lt <- ScraperParse.parseHHmm(hs)
    } yield RawSlot(title, LocalDateTime.of(d, lt), booking, room, poster, filmUrl)
  }
}
