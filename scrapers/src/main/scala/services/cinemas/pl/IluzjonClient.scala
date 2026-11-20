package services.cinemas.pl

import java.time.{LocalDate, LocalDateTime}

import scala.jdk.CollectionConverters.*

import models.*
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.ScraperParse
import services.cinemas.common.SlotsToMovies
import services.cinemas.common.SynopsisProse
import services.cinemas.common.{SingleSiteScraper, DetailEnricher, DetailFetchOutcome, FilmDetail}
import tools.{HttpFetch, HttpRead}

/**
 * Kino Iluzjon (Filmoteka Narodowa, Warszawa). The `/repertuar.html` page is a
 * sequence of day sections (an `h3` date header followed by rows); each row is
 * a screening with its time, title, room, booking link and a country/year line.
 * Richer metadata (runtime, director, full country list, synopsis) lives on the
 * per-film `/filmy/info/<id>/<slug>.html` page. Listing dates are absolute
 * day + Polish month name with no year, so `today` is injected for the year.
 *
 * Two-phase fetch: the repertoire page yields one row per (film, showtime) with
 * title, time, poster, and the per-film detail-page URL stored in `filmUrl`.
 * The detail page is then fetched per unique film for the metadata the listing
 * doesn't expose — runtime, director, countries, synopsis, original title.
 */
class IluzjonClient(http: HttpFetch, today: => LocalDate
) extends SingleSiteScraper with DetailEnricher {


  val cinema: Cinema = KinoIluzjon

  private val BaseUrl     = "https://www.iluzjon.fn.org.pl"
  private val ListingUrl  = s"$BaseUrl/repertuar.html"
  private val FilmIdPat   = """filmy/info/(\d+)/""".r
  private val TimeTitlePat = raw"""^${ScraperParse.ClockParts}\s*-\s*(.+)$$""".r

  private case class RawSlot(filmId: String, title: String, dateTime: LocalDateTime, room: Option[String],
                             booking: Option[String], poster: Option[String], detailPath: String)

  protected def siteUrl: String = BaseUrl

  def fetch(): Seq[CinemaMovie] = {
    val document = Jsoup.parse(HttpRead.page(http, ListingUrl))

    // h3 date headers and screening rows in document order; fold to attach each
    // row to the most recent date.
    val slots = ScraperParse.underDayHeaders(document.select("h3, tr:has(span.hour)").asScala) {
      case h3 if h3.tagName == "h3" => IluzjonClient.parseDate(h3.text, today)
    }(rowSlot)

    SlotsToMovies.fold(slots, _.filmId, s => Showtime(s.dateTime, s.booking, s.room, Nil), order = SlotsToMovies.MovieOrder.asGrouped) { (_, group, showtimes) =>
      val primary = group.head
      CinemaMovie(
        movie     = Movie(title = primary.title),
        cinema    = cinema,
        posterUrl = primary.poster,
        filmUrl   = Some(primary.detailPath),
        showtimes = showtimes
      )
    }
  }

  override val detailGroup: String = "iluzjon"

  /** Deferred per-film detail fetch — the EnrichDetails task calls this with the
   *  movie's filmUrl (e.g. `https://www.iluzjon.fn.org.pl/filmy/info/<id>/<slug>.html`).
   *  None on fetch failure so the task stays stale and is retried rather than
   *  recording an empty result as fresh.
   *
   *  A durable 404/410 escapes rather than folding into None, so a page that is
   *  gone for good gets stamped instead of retried every tick — see [[DetailFetchOutcome]]. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.page(http, ref).map { html =>
      val detail = IluzjonClient.parseDetail(html)
      FilmDetail(
        synopsis       = detail.synopsis,
        cast           = detail.cast,
        director       = detail.director,
        runtimeMinutes = detail.runtimeMinutes,
        releaseYear    = detail.year,
        originalTitle  = detail.originalTitle,
        countries      = detail.countries,
        posterUrl      = detail.poster
      )
    }

  private def rowSlot(row: Element, date: LocalDate): Option[RawSlot] =
    Option(row.selectFirst("span.hour a")).flatMap { link =>
      val href = link.attr("href")
      for {
        filmId <- FilmIdPat.findFirstMatchIn(href).map(_.group(1))
        m      <- TimeTitlePat.findFirstMatchIn(link.text.trim)
        t      <- ScraperParse.clockAt(m, 1)
      } yield {
        val title   = m.group(3).trim
        val room    = Option(row.selectFirst("div.location")).map(_.text.trim.replaceAll("[()]", "").trim).filter(_.nonEmpty)
        val booking = Option(row.selectFirst("div.info-add a[href*=bilety.iluzjon]")).map(_.attr("href")).filter(_.nonEmpty)
        val poster  = Option(row.selectFirst("div.image img[src]")).map(_.attr("src")).filter(_.nonEmpty).map(ScraperParse.onSite(BaseUrl))
        val detailP = ScraperParse.onSite(BaseUrl)(href.takeWhile(_ != '?'))
        RawSlot(filmId, title, date.atTime(t), room, booking, poster, detailP)
      }
    }
}

object IluzjonClient {

  /** "5 Czerwca - Piątek" (the h3 uses the bare nominative "5 Czerwiec" as well
   *  as the genitive) → an absolute date; year from `today`, rolling forward
   *  when the month is already behind us. */
  def parseDate(raw: String, today: LocalDate): Option[LocalDate] =
    ScraperParse.parseDayMonth(raw).flatMap(ScraperParse.upcomingMonthDate(_, today))

  final case class Detail(
    runtimeMinutes: Option[Int],
    year:           Option[Int],
    countries:      Seq[String],
    director:       Seq[String],
    cast:           Seq[String],
    originalTitle:  Option[String],
    synopsis:       Option[String],
    poster:         Option[String]
  )
  object Detail { val empty: Detail = Detail(None, None, Seq.empty, Seq.empty, Seq.empty, None, None, None) }

  def parseDetail(html: String): Detail = {
    val document = Jsoup.parse(html)
    Detail(
      runtimeMinutes = ScraperParse.ddField(document, "czas trwania").flatMap(ScraperParse.firstNumberIn),
      year           = ScraperParse.ddField(document, "rok produkcji").flatMap(ScraperParse.firstFourDigitsIn),
      countries      = ScraperParse.ddItems(document, "kraj produkcji"),
      director       = ScraperParse.ddItems(document, "reżyseria"),
      cast           = ScraperParse.ddItems(document, "obsada"),
      originalTitle  = ScraperParse.ddField(document, "tytuł oryg"),
      synopsis       = Option(document.selectFirst("h4:contains(Opis filmu) + div.content")).map(SynopsisProse.blockText(_).trim)
                         .orElse(Option(document.selectFirst("div.content p")).map(SynopsisProse.blockText(_).trim)).filter(_.length > 20),
      poster         = Option(document.selectFirst("div.plakat img[src]")).map(_.attr("src")).filter(_.nonEmpty)
                         .map(ScraperParse.onSite("https://www.iluzjon.fn.org.pl"))
    )
  }
}
