package services.cinemas

import models._
import org.jsoup.Jsoup
import tools.{HttpFetch, ParallelDetailFetch}

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Cinemas hosted on the Bilety24 platform (e.g. Kino Luna, Kino Elektronik).
 * A listing page links to per-film event pages (`/wydarzenie/?id=N`); each
 * event page carries the film's whole-week schedule as `b24-button` buy links
 * whose `title` encodes the absolute date + time, plus a `movie-parameters`
 * genres/runtime line and a description block. Parameterised by base URL +
 * cinema so one client serves every Bilety24-hosted venue.
 *
 * (DCF is a *different* Bilety24 integration — repertoire on its own domain
 * with `aria-label` slots carrying the auditorium — so it keeps its own
 * `DcfClient`.)
 */
class Bilety24Client(
  http:        HttpFetch,
  baseUrl:     String,
  override val cinema: Cinema,
  listingPath: String = "/repertuar/"
) extends CinemaScraper {

  private val EventLinkPat = """/wydarzenie/\?id=(\d+)""".r

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(baseUrl)
  override def sourceUrl: Option[String] = Some(baseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val listing  = http.get(baseUrl + listingPath)
    val eventIds = EventLinkPat.findAllMatchIn(listing).map(_.group(1)).toSeq.distinct

    val pages = ParallelDetailFetch.keyed("bilety24-events", eventIds, 1.minute)(id => s"$baseUrl/wydarzenie/?id=$id") { url =>
      Try(http.get(url)).toOption
    }
    eventIds.flatMap(id => pages.getOrElse(id, None).flatMap(html => Bilety24Client.parseEvent(html, cinema, baseUrl, id)))
  }
}

object Bilety24Client {

  // Buy-button title: "Kup bilet - Film: <Title> - YYYY-MM-DD HH:MM - <City>"
  private val ButtonTitlePat = """Kup bilet - Film:\s*(.+?)\s*-\s*(\d{4}-\d{2}-\d{2})\s+(\d{2}:\d{2})\s*-""".r
  private val DateTimeFmt     = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
  private val RuntimePat      = """(\d+)\s*min""".r

  def parseEvent(html: String, cinema: Cinema, baseUrl: String, eventId: String): Option[CinemaMovie] = {
    val document = Jsoup.parse(html)

    val title = Option(document.selectFirst("div.title-name[title]")).map(_.attr("title").trim)
      .orElse(Option(document.selectFirst(".title-name")).map(_.text.trim))
      .filter(_.nonEmpty)

    // Buy buttons are rendered twice (desktop + mobile) — dedup by booking URL.
    val slots = document.select("a.b24-button[title^=\"Kup bilet - Film:\"]").asScala.toSeq.flatMap { a =>
      val href = a.attr("href")
      // Skip disabled/placeholder buttons (href="#") — only real kup-bilety
      // links are bookable slots.
      // Active buttons link to a real URL (/kup-bilety/ or /b24-do-miejsc-numerowanych-i-nienumerowanych/);
      // inactive/disabled buttons use href="#".
      if (href == "#") None
      else ButtonTitlePat.findFirstMatchIn(a.attr("title")).flatMap { m =>
        Try(LocalDateTime.parse(s"${m.group(2)} ${m.group(3)}", DateTimeFmt)).toOption.map { dt =>
          val booking = if (href.startsWith("http")) href else baseUrl + href
          val format  = Option(a.selectFirst("span.b24-button__format")).map(_.text.trim)
                          .filter(_.nonEmpty).map(_.split("\\s+").toList.filter(_.nonEmpty)).getOrElse(Nil)
          Showtime(dt, Some(booking), None, format)
        }
      }
    }.distinctBy(_.dateTime).sortBy(_.dateTime)

    val parameters  = Option(document.selectFirst("p.movie-parameters")).map(_.text.trim).getOrElse("")
    val segs    = parameters.split("\\|").map(_.trim).filter(_.nonEmpty).toSeq
    val runtime = segs.flatMap(s => RuntimePat.findFirstMatchIn(s).map(_.group(1).toInt)).headOption
    val genres  = segs.filterNot(s => RuntimePat.findFirstMatchIn(s).isDefined)
                      .map(tools.TextNormalization.titleCaseIfAllLower)
    val synopsis = Option(document.selectFirst("div.title-description-content")).map(_.text.trim).filter(_.length > 20)
    // Some b24-image slots are generic SVG placeholders ("PAN-BILET…svg"); take
    // the first real raster image, else fall back to the og:image poster.
    val poster   = document.select("img.b24-image").asScala.toSeq.map(_.attr("src"))
                     .find(s => s.nonEmpty && !s.toLowerCase.endsWith(".svg"))
                     .orElse(Option(document.selectFirst("meta[property=og:image]")).map(_.attr("content")).filter(_.nonEmpty))

    for {
      t <- title if slots.nonEmpty
    } yield CinemaMovie(
      movie     = Movie(title = t, runtimeMinutes = runtime, releaseYear = None, genres = genres),
      cinema    = cinema,
      posterUrl = poster,
      filmUrl   = Some(s"$baseUrl/wydarzenie/?id=$eventId"),
      synopsis  = synopsis,
      cast      = Seq.empty,
      director  = Seq.empty,
      showtimes = slots
    )
  }
}
