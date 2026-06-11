package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.concurrent.duration._
import scala.util.Try

/**
 * Kino Kępa at PROM Kultury Saska Kępa (Warszawa). The "kino-kepa" category
 * page links to one event page per screening; each event page carries its
 * date+time, a `reż. … , Country Year, NNN'` metadata line, the booking link
 * and synopsis. Films are grouped by their (de-prefixed) title.
 */
class PromKepaClient(http: HttpFetch) extends CinemaScraper {

  val cinema: Cinema = KinoKepa

  private val BaseUrl    = "https://promkultury.pl"
  private val ListingUrl = s"$BaseUrl/events/category/kino-kepa/"
  private val EventPat   = """/wydarzenie/([a-z0-9-]+)""".r

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val listing = http.get(ListingUrl)
    val slugs   = EventPat.findAllMatchIn(listing).map(_.group(1)).toSeq.distinct

    val pages = ParallelDetailFetch.keyed("prom-kepa-events", slugs, 1.minute)(s => s"$BaseUrl/wydarzenie/$s") { url =>
      Try(http.get(url)).toOption
    }
    val events = slugs.flatMap(s => pages.getOrElse(s, None).flatMap(html => PromKepaClient.parseEvent(html, s)))

    // The source formats the same film inconsistently (quoted vs ALL-CAPS), so
    // group case-insensitively and display the best-cased variant.
    events.groupBy(_.title.toLowerCase).toSeq.map { case (_, group) =>
      val primary   = group.head
      val title     = group.map(_.title).find(_.exists(_.isLower)).getOrElse(primary.title)
      val showtimes = group.flatMap(_.showtime).distinctBy(s => (s.dateTime, s.bookingUrl)).sortBy(_.dateTime)
      CinemaMovie(
        movie     = Movie(title = title, runtimeMinutes = primary.runtime, releaseYear = primary.year, countries = primary.countries),
        cinema    = cinema,
        posterUrl = primary.poster,
        filmUrl   = Some(s"$BaseUrl/wydarzenie/${primary.slug}"),
        synopsis  = primary.synopsis,
        cast      = Seq.empty,
        director  = primary.director,
        showtimes = showtimes
      )
    }.filter(_.showtimes.nonEmpty)
  }
}

object PromKepaClient {

  private val DateTimeFmt = DateTimeFormatter.ofPattern("d.MM.yyyy, HH:mm")
  private val TimelinePat = """(\d{1,2}\.\d{2}\.\d{4},\s*\d{1,2}:\d{2})""".r
  // "reż. David Frankel, USA 2026, 120'"
  private val MetaPat = """reż\.\s*(.+?),\s*(.+?)\s+((?:19|20)\d{2}),\s*(\d+)['’]""".r

  private case class Event(slug: String, title: String, showtime: Option[Showtime], runtime: Option[Int],
                           year: Option[Int], countries: Seq[String], director: Seq[String],
                           synopsis: Option[String], poster: Option[String])

  private def parseEvent(html: String, slug: String): Option[Event] = {
    val doc   = Jsoup.parse(html)
    val title = Option(doc.selectFirst("h3.event_title")).map(_.text.trim)
      .map(_.replaceFirst("(?i)^KINO KĘPA:\\s*", "").replaceAll("[\"„”“]", "").trim).filter(_.nonEmpty)

    title.map { t =>
      val dt = TimelinePat.findFirstMatchIn(Option(doc.selectFirst("p.timeline")).map(_.text).getOrElse(""))
        .flatMap(m => Try(LocalDateTime.parse(m.group(1).replaceAll(",\\s*", ", "), DateTimeFmt)).toOption)
      val booking = Option(doc.selectFirst("p.ticket a.cfs-hyperlink[href]")).map(_.attr("href")).filter(_.nonEmpty)
      val meta = MetaPat.findFirstMatchIn(Option(doc.selectFirst("p.repertuar_description")).map(_.text).getOrElse(""))
      Event(
        slug      = slug,
        title     = t,
        showtime  = dt.map(d => Showtime(d, booking, None, Nil)),
        runtime   = meta.map(_.group(4).toInt),
        year      = meta.map(_.group(3).toInt),
        countries = meta.toSeq.flatMap(_.group(2).split("[,/]").map(_.trim).filter(_.nonEmpty)),
        director  = meta.toSeq.flatMap(_.group(1).split(",").map(_.trim).filter(_.nonEmpty)),
        synopsis  = Option(doc.selectFirst("div.single_content")).map(_.text.trim)
                      .map(_.replaceAll("(?i)\\s*bilet[^.]*\\d+\\s*zł.*$", "").trim).filter(_.length > 20),
        poster    = Option(doc.selectFirst("div.image_item img.wp-post-image[src]")).map(_.attr("src")).filter(_.nonEmpty)
      )
    }
  }
}
