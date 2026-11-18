package services.cinemas.pl

import java.util.Locale

import services.cinemas.common.ScraperParse
import services.titles.FormatTags
import services.titles.TitleNormalizer
import models._
import tools.{HttpFetch, HttpRead}
import org.jsoup.Jsoup
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper, FilmDetail, VenueCredits}

import scala.jdk.CollectionConverters._

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
  listingPath: String = "/repertuar/",
  titles:      TitleNormalizer
) extends ChunkedCinemaScraper {

  private val EventLinkPat = """/wydarzenie/\?id=(\d+)""".r

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(baseUrl)
  override def sourceUrl: Option[String] = Some(baseUrl)

  /** The repertoire page links one `/wydarzenie/?id=N` per event = one chunk per
   *  event. */
  def planChunks(): Seq[String] =
    EventLinkPat.findAllMatchIn(HttpRead.page(http, baseUrl + listingPath)).map(_.group(1)).toSeq.distinct

  /** One event page → its film (0 or 1). A throw reschedules just this event's
   *  chunk. The default `reduceChunks` groups by `filmUrl` (the unique event URL),
   *  so each event stays its own entry exactly as the old flat scrape produced. */
  def fetchChunk(eventId: String): Seq[CinemaMovie] =
    Bilety24Client.parseEvent(HttpRead.page(http, s"$baseUrl/wydarzenie/?id=$eventId"), cinema, baseUrl, eventId, titles).toSeq
}

object Bilety24Client {

  // Buy-button title: "Kup bilet - Film: <Title> - YYYY-MM-DD HH:MM - <City>"
  private val ButtonTitlePat = raw"""Kup bilet - Film:\s*(.+?)\s*-\s*(\d{4}-\d{2}-\d{2})\s+(${ScraperParse.IsoClockText})\s*-""".r
  private val RuntimePat      = """(\d+)\s*min""".r

  /** A Bilety24 event page's `p.movie-parameters` pipe line ("Dramat | Komedia | 120 min"): the runtime, and every
   *  other segment a genre (title-cased when the page shouts it). Shared by every venue on a Bilety24 shop. */
  private[pl] def movieParameters(document: org.jsoup.nodes.Document): (Option[Int], Seq[String]) = {
    val parameters = Option(document.selectFirst("p.movie-parameters")).map(_.text.trim).getOrElse("")
    val segments   = parameters.split("\\|").map(_.trim).filter(_.nonEmpty).toSeq
    val runtime    = segments.flatMap(s => RuntimePat.findFirstMatchIn(s).map(_.group(1).toInt)).headOption
    val genres     = segments.filterNot(s => RuntimePat.findFirstMatchIn(s).isDefined).map(tools.TextNormalization.titleCaseIfAllLower)
    (runtime, genres)
  }

  def parseEvent(html: String, cinema: Cinema, baseUrl: String, eventId: String, titles: TitleNormalizer): Option[CinemaMovie] = {
    val document = Jsoup.parse(html)
    val titled   = titleAndFormat(document, cinema, titles)
    val slots    = showtimes(document, baseUrl, titled.map(_._2).getOrElse(Nil))

    val (runtime, genres) = movieParameters(document)
    // The description block also carries a "czytaj więcej" toggle, an empty
    // `p.read-more` placeholder, and—on cycle/concert events—organiser links
    // (Instagram/Facebook handles, "Więcej: www…"). Drop the anchors and the
    // placeholder and strip any plain-text URLs left behind.
    val description = Option(document.selectFirst("div.title-description-content"))
    val synopsis = description.map(ScraperParse.cleanSynopsis(_, "a", "p.read-more")).filter(_.length > 20)
    // What the venue typed of the film into the same box ("Reżyseria: …", "Czas trwania: 45 minut").
    val credits  = description.map(d => VenueCredits.parse(ScraperParse.blockLinesOf(d))).getOrElse(FilmDetail())

    for {
      t <- titled.map(_._1) if slots.nonEmpty
    } yield CinemaMovie(
      movie     = Movie(title = t, runtimeMinutes = runtime.orElse(credits.runtimeMinutes), releaseYear = credits.releaseYear,
                        countries = credits.countries, genres = genres, originalTitle = credits.originalTitle),
      cinema    = cinema,
      posterUrl = poster(document),
      filmUrl   = Some(s"$baseUrl/wydarzenie/?id=$eventId"),
      synopsis  = synopsis,
      cast      = credits.cast,
      director  = credits.director,
      showtimes = slots
    )
  }

  // Peel a language/format suffix ("Supergirl/dubbing", "… /napisy") off the
  // title into format tokens, so the dub/subtitle editions collapse onto one
  // clean-titled film instead of each fragmenting into its own row. The buy
  // button carries its own per-screening format when present; the title token
  // is the fallback for the (common) case where the button format span is empty.
  private def titleAndFormat(document: org.jsoup.nodes.Document, cinema: Cinema, titles: TitleNormalizer): Option[(String, List[String])] =
    Option(document.selectFirst("div.title-name[title]")).map(_.attr("title").trim)
      .orElse(Option(document.selectFirst(".title-name")).map(_.text.trim))
      .map(t => titles.cinemaClean(cinema.slug, t))
      .filter(_.nonEmpty)
      .map(FormatTags.extractFormatTags)

  // Buy buttons are rendered twice (desktop + mobile) — dedup by booking URL.
  private def showtimes(document: org.jsoup.nodes.Document, baseUrl: String, titleFormat: List[String]): Seq[Showtime] =
    document.select("a.b24-button[title^=\"Kup bilet - Film:\"]").asScala.toSeq
      .flatMap(showtimeOf(_, baseUrl, titleFormat))
      .distinctBy(_.dateTime).sortBy(_.dateTime)

  // Skip disabled/placeholder buttons (href="#") — only real kup-bilety
  // links are bookable slots.
  // Active buttons link to a real URL (/kup-bilety/ or /b24-do-miejsc-numerowanych-i-nienumerowanych/);
  // inactive/disabled buttons use href="#".
  private def showtimeOf(a: org.jsoup.nodes.Element, baseUrl: String, titleFormat: List[String]): Option[Showtime] = {
    val href = a.attr("href")
    if (href == "#") None
    else ButtonTitlePat.findFirstMatchIn(a.attr("title")).flatMap { m =>
      ScraperParse.parseDateTime(s"${m.group(2)} ${m.group(3)}").map { dt =>
        val booking   = if (href.startsWith("http")) href else baseUrl + href
        val buttonFmt = Option(a.selectFirst("span.b24-button__format")).map(_.text.trim)
                        .filter(_.nonEmpty).map(_.split("\\s+").toList.filter(_.nonEmpty)).getOrElse(Nil)
        Showtime(dt, Some(booking), None, if (buttonFmt.nonEmpty) buttonFmt else titleFormat)
      }
    }
  }

  // Some b24-image slots are generic SVG placeholders ("PAN-BILET…svg"); take
  // the first real raster image, else fall back to the og:image poster.
  private def poster(document: org.jsoup.nodes.Document): Option[String] =
    document.select("img.b24-image").asScala.toSeq.map(_.attr("src"))
      .find(s => s.nonEmpty && !s.toLowerCase(Locale.ROOT).endsWith(".svg"))
      .orElse(ScraperParse.ogImage(document))
}
