package services.cinemas.pl

import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import models._
import tools.HttpFetch
import services.cinemas.common.{CinemaScraper, SlotsToMovies}

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Centrum 3D (Przemyśl) — the film screen run by Centrum Kulturalne w Przemyślu.
 * Previously scraped from Filmweb (cinemaId 1786), which went silently empty
 * (the venue is no longer maintained on Filmweb) even while the venue's OWN site
 * keeps publishing a full repertoire — so the Filmweb-backed scrape recorded a
 * white "0 showtimes" bar. This client reads the venue's own site directly.
 *
 * The repertoire at `/kino-centrum/repertuar` is an IcAgenda (Joomla events
 * component) list. Each screening is one `div.ic-event-div` carrying:
 *   - `div.iceventlist-title a` → the film title, whose `href` is the IcAgenda
 *     event URL `/component/icagenda/<id>-<slug>/YYYY-MM-DD-HH-MM` — the trailing
 *     segment IS the screening date + start time, so one anchor = one showtime.
 *   - `span.iceventlist-desc` → a short synopsis teaser (optional).
 *
 * Titles are served ALL-CAPS with format words glued on ("DRZEWO MAGII dubbing");
 * they are left verbatim here — the ingest choke point (`MovieCache
 * .recordCinemaScrape`) recases them and strips the format tokens centrally.
 * The event page carries no per-screening booking deep-link, so the event URL
 * itself is the showtime's link (it opens the bookable event page).
 */
class KinoCentrum3DPrzemyslClient(http: HttpFetch, override val cinema: Cinema)
    extends CinemaScraper {

  import KinoCentrum3DPrzemyslClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(RepertoireUrl)

  def fetch(): Seq[CinemaMovie] = {
    val slots = Jsoup.parse(http.get(RepertoireUrl))
      .select("div.ic-event-div").asScala.toSeq.flatMap(parseEvent)

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, Some(s.url))) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = group.sortBy(_.dateTime).map(_.url).headOption,
        synopsis  = group.flatMap(_.synopsis).headOption,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }
}

object KinoCentrum3DPrzemyslClient {

  val BaseUrl        = "https://ck.przemysl.pl"
  val RepertoireUrl  = s"$BaseUrl/kino-centrum/repertuar"

  // The IcAgenda event href ends in the screening's own date + time:
  // `/component/icagenda/1027-robin-hood-koniec-legendy/2026-07-28-15-00`.
  private val DateTimeFmt = DateTimeFormatter.ofPattern("yyyy-MM-dd-HH-mm")
  private val DateTimePat = """(\d{4}-\d{2}-\d{2}-\d{2}-\d{2})\b""".r

  private case class RawSlot(title: String, dateTime: LocalDateTime, url: String, synopsis: Option[String])

  /** One IcAgenda event block → one screening slot (title + date/time off the
   *  event href). None when the block has no title anchor or no parseable
   *  date-time (a stray non-screening entry). */
  private def parseEvent(event: Element): Option[RawSlot] =
    for {
      anchor   <- Option(event.selectFirst("div.iceventlist-title a"))
      title    <- Option(anchor.text.trim).filter(_.nonEmpty)
      href      = anchor.attr("href")
      dateTime <- DateTimePat.findFirstIn(href).flatMap(s => Try(LocalDateTime.parse(s, DateTimeFmt)).toOption)
    } yield {
      val url = if (href.startsWith("http")) href else BaseUrl + href
      val synopsis = Option(event.selectFirst("span.iceventlist-desc")).map(_.text.trim).filter(_.length > 20)
      RawSlot(title, dateTime, url, synopsis)
    }
}
