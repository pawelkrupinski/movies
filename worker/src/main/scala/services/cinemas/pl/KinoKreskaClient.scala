package services.cinemas.pl

import services.cinemas.common.ScraperParse
import play.api.libs.json.Json
import models._
import tools.{HttpFetch, HttpRead}
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import services.cinemas.common.{CinemaScraper, DetailEnricher, DetailFetchOutcome, FilmDetail, SlotsToMovies, VenueCredits}

import java.time.{LocalDate, LocalDateTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Kreska — the cinema run by Studio Filmów Rysunkowych in Bielsko-Biała.
 * Screenings are served via the SFR CMS through a JSON/HTML hybrid endpoint
 * that returns rendered `<li>` tiles per screening:
 *
 *   POST /heroapp/terms/rest/load
 *   Body: offset=0&itemsPerLoading=200&filters[category]=Repertuar kinowy&orderBy=&lang=pl
 *
 * The response is JSON with `status: 0` (success) and an `items` field
 * containing raw HTML. Each tile is a `<li class="ajax-loading-items-manager__item">` with:
 *   - `div.std-horizontal-tile__time div` (first child)  → time `HH:MM`
 *   - `div.std-horizontal-tile__time div` (second child) → date `YYYY-MM-DD`
 *   - `a.std-link.std-link--hudge`                       → film title + per-event href
 *   - `a.std-button`                                     → booking link
 *
 * The `filters[category]=Repertuar kinowy` narrows results to cinema screenings
 * only, excluding the permanent "Wystawa Stała w OKU" exhibition slots and other
 * non-film events.
 *
 * Previously served from Filmweb; migrated when Filmweb stopped carrying the
 * venue's repertoire reliably.
 *
 * Each tile links the film's `/wydarzenie/<id>/<slug>` page, whose `div.std-text`
 * states what the venue knows of the film — a "reż. Natxo Leuza, Hiszpania 2025,
 * 85'" credit line or a "kraj, rok: … / Reżyseria: … / Obsada: … / Czas trwania:
 * 93’" block — above the synopsis. That page is the deferred detail
 * ([[DetailEnricher]], read by [[VenueCredits]]); its facts are the venue's own, so
 * the listing waits for it (`defersTmdbResolution`, the default). The page's
 * `og:image` is the site's default cover; the event's own image is its poster.
 */
class KinoKreskaClient(
  http:  HttpFetch,
  override val cinema: Cinema,
  today: => LocalDate
) extends CinemaScraper with DetailEnricher {

  import KinoKreskaClient._

  override val detailGroup: String = "kino-kreska"

  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.page(http, ref).map(html => parseDetail(Jsoup.parse(html, BaseUrl)))

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(s"$BaseUrl/kino-kreska")

  def fetch(): Seq[CinemaMovie] = {
    // A failed POST or an unparseable answer propagates: swallowed into "" it read as a
    // venue with no screenings — a white scrape instead of a red one.
    val json = HttpRead.postPage(http, TermsUrl, PostBody, "application/x-www-form-urlencoded")
    if (json.isEmpty) return Seq.empty

    val itemsHtml = (Json.parse(json) \ "items").asOpt[String].getOrElse("")
    if (itemsHtml.isEmpty) return Seq.empty

    parse(itemsHtml, cinema)
  }
}

object KinoKreskaClient {

  val BaseUrl  = "https://www.sfr.pl"
  val TermsUrl = s"$BaseUrl/heroapp/terms/rest/load"

  // Percent-encoded form body: filters[category]=Repertuar kinowy, returns only cinema events.
  val PostBody = "offset=0&itemsPerLoading=200&filters%5Bcategory%5D=Repertuar+kinowy&orderBy=&lang=pl"

  private case class RawSlot(
    title:    String,
    dateTime: LocalDateTime,
    booking:  Option[String],
    filmUrl:  Option[String]
  )

  def parse(itemsHtml: String, cinema: Cinema): Seq[CinemaMovie] = {
    val document = Jsoup.parse(itemsHtml, BaseUrl)
    val slots = document.select("li.ajax-loading-items-manager__item").asScala.toSeq.flatMap { li =>
      val timeDivs = li.select("div.std-horizontal-tile__time div").asScala.toSeq
      for {
        timeStr <- timeDivs.headOption.map(_.text.trim)
        dateStr <- timeDivs.drop(1).headOption.map(_.text.trim)
        time    <- ScraperParse.parseHHmm(timeStr)
        date    <- Try(LocalDate.parse(dateStr)).toOption
        titleEl <- Option(li.selectFirst("a.std-link.std-link--hudge"))
        title    = titleEl.text.trim if title.nonEmpty
      } yield RawSlot(
        title    = title,
        dateTime = LocalDateTime.of(date, time),
        booking  = Option(li.selectFirst("a.std-button")).map(_.attr("abs:href")).filter(_.nonEmpty),
        filmUrl  = Option(titleEl.attr("abs:href")).filter(_.nonEmpty)
      )
    }

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking)) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(ScraperParse.stripFormatTags(title)),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = group.flatMap(_.filmUrl).headOption,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }

  /** A `/wydarzenie/<id>/<slug>` page: the venue's credits and synopsis in `div.std-text`
   *  (its "ZOBACZ ZWIASTUN" trailer link dropped), the event's own image beside them. */
  private[cinemas] def parseDetail(document: Document): FilmDetail = {
    val lines = Option(document.selectFirst("div.std-text")).toSeq.flatMap { text =>
      val kept = text.clone()
      kept.select("a").remove()
      ScraperParse.blockLinesOf(kept)
    }
    val prose = lines.filterNot(line => VenueCredits.statesFacts(line) || LabelLine.matches(line) || line == "ZOBACZ ZWIASTUN")
    VenueCredits.parse(lines).copy(
      synopsis  = Some(prose.mkString("\n\n")).filter(_.length > 20),
      posterUrl = Option(document.selectFirst("img.sticky-image__image")).map(_.attr("abs:src")).filter(_.nonEmpty))
  }

  /** "Gatunek: dramat", "kategoria wiekowa: 16+" — a short labelled line, never the synopsis. */
  private val LabelLine = """^[\p{L} ,.]{2,30}:\s*\S.{0,80}$""".r
}
