package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import tools.{CachingDetailFetch, HttpFetch}

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.jdk.CollectionConverters._
import scala.util.Try

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
class IluzjonClient(http: HttpFetch, today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))) extends CinemaScraper with DetailEnricher {

  // Static film detail pages cached across passes; the repertoire listing keeps
  // the live `http` since its showtimes change every pass.
  private val detailHttp = new CachingDetailFetch(http)

  val cinema: Cinema = KinoIluzjon

  private val BaseUrl     = "https://www.iluzjon.fn.org.pl"
  private val ListingUrl  = s"$BaseUrl/repertuar.html"
  private val FilmIdPat   = """filmy/info/(\d+)/""".r
  private val TimeTitlePat = """^(\d{1,2}):(\d{2})\s*-\s*(.+)$""".r

  private case class RawSlot(filmId: String, title: String, dateTime: LocalDateTime, room: Option[String],
                             booking: Option[String], poster: Option[String], detailPath: String)

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = fetchBare()

  private def fetchBare(): Seq[CinemaMovie] = {
    val doc = Jsoup.parse(http.get(ListingUrl))

    // h3 date headers and screening rows in document order; fold to attach each
    // row to the most recent date.
    var currentDate: Option[LocalDate] = None
    val slots = doc.select("h3, tr:has(span.hour)").asScala.toSeq.flatMap { el =>
      if (el.tagName == "h3") { currentDate = IluzjonClient.parseDate(el.text, today); Seq.empty }
      else currentDate.toSeq.flatMap(d => rowSlot(el, d))
    }

    val byFilm = slots.groupBy(_.filmId)
    byFilm.toSeq.flatMap { case (_, group) =>
      val primary   = group.head
      val showtimes = group.distinctBy(s => (s.dateTime, s.booking)).sortBy(_.dateTime)
                       .map(s => Showtime(s.dateTime, s.booking, s.room, Nil))
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title = primary.title),
        cinema    = cinema,
        posterUrl = primary.poster,
        filmUrl   = Some(primary.detailPath),
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      ))
    }
  }

  override val detailGroup: String = "iluzjon"

  /** Deferred per-film detail fetch — the EnrichDetails task calls this with the
   *  movie's filmUrl (e.g. `https://www.iluzjon.fn.org.pl/filmy/info/<id>/<slug>.html`).
   *  None on fetch failure so the task stays stale and is retried rather than
   *  recording an empty result as fresh. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    Try(detailHttp.get(ref)).toOption.map { html =>
      val d = IluzjonClient.parseDetail(html)
      FilmDetail(
        synopsis       = d.synopsis,
        cast           = d.cast,
        director       = d.director,
        runtimeMinutes = d.runtimeMinutes,
        releaseYear    = d.year,
        originalTitle  = d.originalTitle,
        countries      = d.countries,
        posterUrl      = d.poster
      )
    }

  private def rowSlot(row: Element, date: LocalDate): Option[RawSlot] =
    Option(row.selectFirst("span.hour a")).flatMap { link =>
      val href = link.attr("href")
      for {
        filmId <- FilmIdPat.findFirstMatchIn(href).map(_.group(1))
        m      <- TimeTitlePat.findFirstMatchIn(link.text.trim)
        t      <- Try(java.time.LocalTime.of(m.group(1).toInt, m.group(2).toInt)).toOption
      } yield {
        val title   = m.group(3).trim
        val room    = Option(row.selectFirst("div.location")).map(_.text.trim.replaceAll("[()]", "").trim).filter(_.nonEmpty)
        val booking = Option(row.selectFirst("div.info-add a[href*=bilety.iluzjon]")).map(_.attr("href")).filter(_.nonEmpty)
        val poster  = Option(row.selectFirst("div.image img[src]")).map(_.attr("src")).filter(_.nonEmpty).map(abs)
        val detailP = if (href.startsWith("http")) href.takeWhile(_ != '?') else s"$BaseUrl/${href.stripPrefix("/").takeWhile(_ != '?')}"
        RawSlot(filmId, title, date.atTime(t), room, booking, poster, detailP)
      }
    }

  private def abs(u: String): String = if (u.startsWith("http")) u else s"$BaseUrl/${u.stripPrefix("/")}"
}

object IluzjonClient {

  // Genitive forms shared via ScraperParse; the h3 also uses the bare
  // nominative ("5 Czerwiec"), so extend the shared map with those.
  private val Months = ScraperParse.PolishMonths ++ Map(
    "styczeń" -> 1, "luty" -> 2, "marzec" -> 3, "kwiecień" -> 4, "maj" -> 5, "czerwiec" -> 6,
    "lipiec" -> 7, "sierpień" -> 8, "wrzesień" -> 9, "październik" -> 10, "listopad" -> 11, "grudzień" -> 12
  )
  private val DayMonthPat = """(\d{1,2})\s+([A-Za-ząćęłńóśźżĄĆĘŁŃÓŚŹŻ]+)""".r

  /** "5 Czerwca - Piątek" → an absolute date; year from `today`, rolling forward
   *  when the month is already behind us. */
  def parseDate(raw: String, today: LocalDate): Option[LocalDate] =
    DayMonthPat.findFirstMatchIn(raw).flatMap { m =>
      Months.get(m.group(2).toLowerCase).flatMap { mon =>
        val year = if (mon < today.getMonthValue) today.getYear + 1 else today.getYear
        Try(LocalDate.of(year, mon, m.group(1).toInt)).toOption
      }
    }

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

  private def dd(doc: org.jsoup.nodes.Document, label: String): Option[String] =
    ScraperParse.ddField(doc, label)

  def parseDetail(html: String): Detail = {
    val doc = Jsoup.parse(html)
    Detail(
      runtimeMinutes = dd(doc, "czas trwania").flatMap(s => """(\d+)""".r.findFirstMatchIn(s).map(_.group(1).toInt)),
      year           = dd(doc, "rok produkcji").flatMap(s => """(\d{4})""".r.findFirstMatchIn(s).map(_.group(1).toInt)),
      countries      = dd(doc, "kraj produkcji").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
      director       = dd(doc, "reżyseria").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
      cast           = dd(doc, "obsada").toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty)),
      originalTitle  = dd(doc, "tytuł oryg"),
      synopsis       = Option(doc.selectFirst("h4:contains(Opis filmu) + div.content")).map(_.text.trim)
                         .orElse(Option(doc.selectFirst("div.content p")).map(_.text.trim)).filter(_.length > 20),
      poster         = Option(doc.selectFirst("div.plakat img[src]")).map(_.attr("src")).filter(_.nonEmpty)
                         .map(u => if (u.startsWith("http")) u else s"https://www.iluzjon.fn.org.pl/${u.stripPrefix("/")}")
    )
  }
}
