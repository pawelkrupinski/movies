package services.cinemas.pl

import services.movies.TitleNormalizer
import models._
import tools.HttpFetch
import org.jsoup.Jsoup
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper}

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
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
) extends ChunkedCinemaScraper {

  private val EventLinkPat = """/wydarzenie/\?id=(\d+)""".r

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(baseUrl)
  override def sourceUrl: Option[String] = Some(baseUrl)

  /** The repertoire page links one `/wydarzenie/?id=N` per event = one chunk per
   *  event. */
  def planChunks(): Seq[String] =
    EventLinkPat.findAllMatchIn(http.get(baseUrl + listingPath)).map(_.group(1)).toSeq.distinct

  /** One event page → its film (0 or 1). A throw reschedules just this event's
   *  chunk. The default `reduceChunks` groups by `filmUrl` (the unique event URL),
   *  so each event stays its own entry exactly as the old flat scrape produced. */
  def fetchChunk(eventId: String): Seq[CinemaMovie] =
    Bilety24Client.parseEvent(http.get(s"$baseUrl/wydarzenie/?id=$eventId"), cinema, baseUrl, eventId).toSeq
}

object Bilety24Client {

  // Buy-button title: "Kup bilet - Film: <Title> - YYYY-MM-DD HH:MM - <City>"
  private val ButtonTitlePat = """Kup bilet - Film:\s*(.+?)\s*-\s*(\d{4}-\d{2}-\d{2})\s+(\d{2}:\d{2})\s*-""".r
  private val DateTimeFmt     = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
  private val RuntimePat      = """(\d+)\s*min""".r

  def parseEvent(html: String, cinema: Cinema, baseUrl: String, eventId: String): Option[CinemaMovie] = {
    val document = Jsoup.parse(html)

    // Peel a language/format suffix ("Supergirl/dubbing", "… /napisy") off the
    // title into format tokens, so the dub/subtitle editions collapse onto one
    // clean-titled film instead of each fragmenting into its own row. The buy
    // button carries its own per-screening format when present; the title token
    // is the fallback for the (common) case where the button format span is empty.
    val titled   = Option(document.selectFirst("div.title-name[title]")).map(_.attr("title").trim)
      .orElse(Option(document.selectFirst(".title-name")).map(_.text.trim))
      .map(t => TitleNormalizer.cinemaClean(cinema.slug, t))
      .filter(_.nonEmpty)
      .map(ScraperParse.extractFormatTags)
    val title    = titled.map(_._1)
    val titleFmt = titled.map(_._2).getOrElse(Nil)

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
          val buttonFmt = Option(a.selectFirst("span.b24-button__format")).map(_.text.trim)
                          .filter(_.nonEmpty).map(_.split("\\s+").toList.filter(_.nonEmpty)).getOrElse(Nil)
          Showtime(dt, Some(booking), None, if (buttonFmt.nonEmpty) buttonFmt else titleFmt)
        }
      }
    }.distinctBy(_.dateTime).sortBy(_.dateTime)

    val parameters  = Option(document.selectFirst("p.movie-parameters")).map(_.text.trim).getOrElse("")
    val segs    = parameters.split("\\|").map(_.trim).filter(_.nonEmpty).toSeq
    val runtime = segs.flatMap(s => RuntimePat.findFirstMatchIn(s).map(_.group(1).toInt)).headOption
    val genres  = segs.filterNot(s => RuntimePat.findFirstMatchIn(s).isDefined)
                      .map(tools.TextNormalization.titleCaseIfAllLower)
    // The description block also carries a "czytaj więcej" toggle, an empty
    // `p.read-more` placeholder, and—on cycle/concert events—organiser links
    // (Instagram/Facebook handles, "Więcej: www…"). Drop the anchors and the
    // placeholder and strip any plain-text URLs left behind.
    val synopsis = Option(document.selectFirst("div.title-description-content"))
      .map(ScraperParse.cleanSynopsis(_, "a", "p.read-more")).filter(_.length > 20)
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
