package services.cinemas.pl

import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Generic client for any cinema running on the visualTicket platform by
 * visualnet.pl ("System Sprzedaży Biletów visualTicket"). The platform
 * serves all upcoming events for an institution on a single server-rendered
 * HTML page at `<baseUrl>/` (no JavaScript required). Each `div.event-item`
 * carries `data-date` (YYYY-MM-DD), `data-time` (HH:MM), and
 * `data-location-id` attributes; these are self-contained — the category
 * label and event title are children of the same div. The location id
 * identifies one physical venue when the institution hosts multiple
 * (e.g. Pałac Kultury Zagłębia operates both Kino Kadr and a theatre stage).
 *
 * Titles arrive with a ` - seans filmowy` suffix (or ` - seans filmowy dla
 * dzieci` for children's screenings) which is stripped before normalisation.
 *
 * Known venues:
 *   - Kino Studyjne Kadr (Dąbrowa Górnicza) — bilety.palac.art.pl, locationId=2
 *
 * One instance per venue; adding a new visualTicket-hosted cinema is a new
 * catalog line with the appropriate `baseUrl` and `locationId`.
 *
 * @param http       HTTP client (swap for [[clients.tools.FakeHttpFetch]] in tests).
 * @param baseUrl    Scheme + host of the venue's visualTicket portal, no trailing slash.
 * @param cinema     [[Cinema]] tag attached to every [[CinemaMovie]].
 * @param locationId Numeric location identifier for this venue within the portal.
 *                   Use [[VisualTicketClient.AnyLocation]] when the portal hosts
 *                   exactly one venue and no filtering is needed.
 */
class VisualTicketClient(
  http:       HttpFetch,
  baseUrl:    String,
  override val cinema: Cinema,
  locationId: Int = VisualTicketClient.AnyLocation
) extends CinemaScraper with OnlyMovieEventsFilter {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(baseUrl)
  override def sourceUrl: Option[String] = Some(baseUrl)

  protected def fetchUnfiltered(): Seq[CinemaMovie] =
    VisualTicketClient.parse(Try(http.get(s"$baseUrl/")).getOrElse(""), cinema, locationId, baseUrl)
}

object VisualTicketClient {

  /** Sentinel value meaning "accept any location-id" — for single-venue portals. */
  val AnyLocation: Int = -1

  private val SeansFimowyRe = """(?i)\s*-\s*seans filmowy.*$""".r

  private[cinemas] def parse(html: String, cinema: Cinema, locationId: Int, baseUrl: String = ""): Seq[CinemaMovie] = {
    if (html.isEmpty) return Seq.empty

    val doc = Jsoup.parse(html, if (baseUrl.nonEmpty) baseUrl else "https://bilety.palac.art.pl")

    // Each screening is a self-contained `div.event-item` block whose
    // children carry all the information we need:
    //   div.event-group-label  — event category ("film", "film dla dzieci", …)
    //   data-date / data-time  — screening date and wall-clock start time
    //   data-location-id       — venue id within the institution
    //   h3.event-title > span  — raw event title
    //   a.btn-buy              — per-screening ticket booking link
    val slots = doc.select("div.event-item").asScala.toSeq.flatMap { item =>
      val loc = Try(item.attr("data-location-id").toInt).getOrElse(-1)
      if (locationId != AnyLocation && loc != locationId) None
      else {
        val category = Option(item.selectFirst("div.event-group-label"))
          .map(_.text.trim.toLowerCase)
          .getOrElse("")
        if (!category.contains("film")) None
        else for {
          rawDate  <- Option(item.attr("data-date")).filter(_.nonEmpty)
          rawTime  <- Option(item.attr("data-time")).filter(_.nonEmpty)
          date     <- Try(LocalDate.parse(rawDate)).toOption
          time     <- Try(LocalTime.parse(rawTime)).toOption
          h3       <- Option(item.selectFirst("h3.event-title"))
          span     <- Option(h3.selectFirst("span[aria-hidden=true]"))
          rawTitle  = SeansFimowyRe.replaceFirstIn(span.text.trim, "").trim
          if rawTitle.nonEmpty
          bookingUrl = Option(item.selectFirst("a.btn-buy")).map(_.attr("abs:href")).filter(_.nonEmpty)
        } yield (rawTitle, LocalDateTime.of(date, time), bookingUrl)
      }
    }

    SlotsToMovies.fold(
      slots,
      titleOf    = _._1,
      showtimeOf = s => Showtime(s._2, s._3),
      distinctBy = s => (s.dateTime, s.bookingUrl)
    ) { (title, _, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }
}
