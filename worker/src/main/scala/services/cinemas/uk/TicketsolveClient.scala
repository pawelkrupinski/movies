package services.cinemas.uk

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import org.jsoup.parser.Parser
import services.cinemas.common.{AgeRating, CinemaScraper, ScraperParse, SlotsToMovies}
import services.cinemas.pl.OnlyMovieEventsFilter
import tools.{HttpFetch, HttpRead}

import java.time.{LocalDate, OffsetDateTime}
import java.util.Locale
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Ticketsolve — the box office of arts centres in Wales, Northern Ireland and the
 * north of England (Theatr Mwldan Cardigan, the Nerve Centre Derry, Pocklington Arts
 * Centre). Flicks lists them with no times, while Ticketsolve publishes each client's
 * whole programme as one public XML feed at `https://<client>.ticketsolve.com/shows.xml`:
 *
 * {{{
 * <venues><venue id="…"><name><![CDATA[Mwldan 1]]></name><shows>
 *   <show id="…"><name><![CDATA[Sense and Sensibility (U)]]></name>
 *     <event_category>Cinema 2D / Sinema 2D</event_category>
 *     <url>https://mwldan.ticketsolve.com/shows/…/events</url>
 *     <images><image><url size="large">…</url></image></images>
 *     <events><event id="…">
 *       <date_time_iso format="ISO 8601" zone="GMT">2026-10-16T19:30:00+01:00</date_time_iso>
 *       <venue_layout>Mwldan 1</venue_layout><status>available</status>
 *       <url>https://mwldan.ticketsolve.com/shows/…/events/…</url>
 *     </event></events></show>…
 * }}}
 *
 * One request is the venue's whole scrape, every date it sells (Mwldan: eight months).
 *
 * WHICH SHOWS ARE FILMS. Each client names its own `event_category`, so the rule is its
 * own filing: a category naming film or cinema — "Film", "Cinema 2D / Sinema 2D",
 * "Event Cinema", "Access - Cinema Subtitled" — or a broadcast, the opera, ballet and
 * theatre relays, however the client words it (Theatr Colwyn's NT Live is "Recorded Live";
 * Mwldan's "Live Broadcast Events / Achlysuron Darlledu Byw": all 27
 * on 2026-10-12 are NT Live, Royal Ballet & Opera, Met Opera or Exhibition on Screen; its
 * gigs are filed apart, as "Live TM / TM Byw") — or a film society, which may file under
 * its acronym alone (Mwldan's "TMFS / CFFTM", Theatr Mwldan Film Society / Cymdeithas Ffilm:
 * "Hen", "The President's Cake") — unless it files teaching about film: the
 * Nerve Centre's "BFI Film Academy" is online masterclasses. A certificate in the show's
 * name is no sign of a film: Mwldan's live shows carry them too ("Cardi Bach 2026 (U)").
 *
 * @param client the Ticketsolve client name, the feed host's first label.
 */
class TicketsolveClient(
  http:   HttpFetch,
  client: String,
  override val cinema: Cinema,
  today:  => LocalDate
) extends CinemaScraper with OnlyMovieEventsFilter {

  import TicketsolveClient._

  private val feedUrl = TicketsolveClient.feedUrl(client)

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(feedUrl)
  override def sourceUrl: Option[String] = Some(feedUrl)

  protected def fetchUnfiltered(): Seq[CinemaMovie] = parse(HttpRead.page(http, feedUrl), cinema, today)
}

object TicketsolveClient {

  def feedUrl(client: String): String = s"https://$client.ticketsolve.com/shows.xml"

  /** A category naming film or a relay, word-bounded: "Film", "Cinema 2D / Sinema 2D",
   *  "Event Cinema", "Access - Cinema Subtitled", "Live Broadcast Events", "Recorded Live",
   *  "Satellite Live". "Live" alone is no relay: it files the gigs too ("Live TM / TM Byw"). */
  private val FilmWord =
    """(?i)\b(films?|cinema|sinema|screens?|screenings?|movies?|broadcasts?|relays?|satellite|recorded\s+live)\b""".r

  /** A film society's acronym, which names no film word: "TMFS" in "TMFS / CFFTM". Upper
   *  case only, so it reads an acronym, not a word ending in "fs". */
  private val FilmSocietyAcronym = """\b[A-Z]{0,4}FS\b""".r

  /** …unless it files teaching about film rather than a screening ("BFI Film Academy"). */
  private val NotAScreening = """(?i)\b(academy|courses?|workshops?|masterclass(es)?|talks?)\b""".r

  private def isFilm(category: String): Boolean =
    (FilmWord.findFirstIn(category).isDefined || FilmSocietyAcronym.findFirstIn(category).isDefined) &&
      NotAScreening.findFirstIn(category).isEmpty

  /** "Sense and Sensibility (U)", "Hay Fever (12A tbc)", "Manon (15 As Live)": the BBFC
   *  certificate the box office appends to the title. */
  private val TitleCertificate = """\s*\(\s*(U|PG|12A?|15|18)\b[^)]*\)\s*$""".r

  /** Event statuses that mean the screening is not happening. */
  private val Cancelled = Set("cancelled", "canceled", "postponed")

  private case class Slot(show: Element, title: String, certificate: Option[String], showtime: Showtime)

  /** The feed → one [[CinemaMovie]] per film with a screening from `today` on. A feed
   *  without its `<venues>` root throws: that is an error page, not an empty programme. */
  def parse(xml: String, cinema: Cinema, today: LocalDate): Seq[CinemaMovie] = {
    val feed = Jsoup.parse(xml, "", Parser.xmlParser())
    require(feed.selectFirst("venues") != null, "no <venues> root in the Ticketsolve feed")

    val slots = for {
      show  <- feed.select("show").asScala.toSeq
      category = child(show, "event_category").getOrElse("") if isFilm(category)
      name  <- child(show, "name").toSeq
      event <- show.select("events > event").asScala.toSeq
      if !child(event, "status").map(_.toLowerCase(Locale.ROOT)).exists(Cancelled)
      start <- child(event, "date_time_iso").flatMap(s => Try(OffsetDateTime.parse(s).toLocalDateTime).toOption).toSeq
      if !start.toLocalDate.isBefore(today)
      title  = TitleCertificate.replaceFirstIn(name, "").replaceAll("\\s+", " ").trim if title.nonEmpty
    } yield Slot(show, title, TitleCertificate.findFirstMatchIn(name).map(_.group(1)), Showtime(
      dateTime   = start,
      bookingUrl = child(event, "url"),
      room       = child(event, "venue_layout"),
      format     = if (ScraperParse.mentionsSubtitles(category)) List("SUB") else Nil)) // "Access - Cinema Subtitled"

    SlotsToMovies.fold(slots, _.title, _.showtime) { (title, group, showtimes) =>
      val show = group.head.show
      CinemaMovie(
        movie       = Movie(title, rawTitle = child(show, "name").filterNot(_ == title)),
        cinema      = cinema,
        posterUrl   = Option(show.selectFirst("images url[size=large]")).map(_.text.trim).filter(_.nonEmpty),
        filmUrl     = child(show, "url"),
        synopsis    = child(show, "description").map(Jsoup.parse(_).text.trim).filter(_.nonEmpty),
        cast        = Seq.empty,
        director    = Seq.empty,
        showtimes   = showtimes,
        externalIds = Option(show.attr("id")).filter(_.nonEmpty).map(id => Map("ticketsolve" -> id)).getOrElse(Map.empty),
        ageRating   = group.flatMap(_.certificate).headOption.flatMap(AgeRating.normalize)
      )
    }
  }

  /** The trimmed text of the element's own child named `tag`, not a descendant's: a
   *  show's `<name>` and `<url>`, never its events'. */
  private def child(el: Element, tag: String): Option[String] =
    el.children.asScala.find(_.tagName == tag).map(_.text.trim).filter(_.nonEmpty)
}
