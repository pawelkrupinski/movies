package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import play.api.libs.json.Json
import tools.{CachingDetailFetch, HttpFetch, ParallelDetailFetch}

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Nowe Horyzonty (Wrocław) — the largest arthouse cinema in Poland, with
 * nine screens and ~40 screenings a day. The visible `program.s` page only
 * shows the nearest advance-sale slot per film (a teaser list), so scraping it
 * caught a small fraction of the schedule. The full daily repertoire is served
 * by the `rep.json?dzien=DD-MM-YYYY` AJAX endpoint the day-picker calls
 * (`wczytajRepertuarNaDzien` → `ajaxRepertuar` in the site's JS): one day per
 * request, every screening of that day in a `lista` HTML blob. We fan out one
 * request per day across a week and read the full schedule off those blobs.
 *
 * Each `div.boks` in `lista` is a film (`op.s?id=` link + `a.tyt` title +
 * `span.ilustr` poster); its `div.seanserep a.xseans` anchors carry one slot
 * each — `eventId=` booking link, `HH:mm` as the link text, the date taken from
 * the day we requested. The per-film `op.s?id=` page adds runtime / year /
 * countries / genres / director / synopsis. `today` is injected so the day
 * window (and thus the fixture replay) is deterministic.
 */
class NoweHoryzontyClient(http: HttpFetch, today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw")), deferDetail: Boolean = false) extends CinemaScraper with DetailEnricher {

  // Static op.s detail pages cached across passes; day blobs keep the live http.
  private val detailHttp = new CachingDetailFetch(http)

  val cinema: Cinema = KinoNoweHoryzonty
  override val detailGroup: String = "nowe-horyzonty"

  private val BaseUrl     = "https://www.kinonh.pl"
  // A constant forwardback keeps the recorded fixture URL stable between
  // recording and replay (FakeHttpFetch keys fixtures by the query string).
  private val Forwardback = s"$BaseUrl/program.s"
  private val DayFmt      = DateTimeFormatter.ofPattern("dd-MM-yyyy")
  private val WindowDays  = 7
  private val FilmIdPat   = """op\.s\?id=(\d+)""".r

  private case class RawSlot(filmId: String, title: String, poster: Option[String], eventId: String,
                             dateTime: LocalDateTime, bookingUrl: String)

  private def dayUrl(date: LocalDate): String =
    s"$BaseUrl/rep.json?dzien=${date.format(DayFmt)}&forwardback=$Forwardback"

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)

  // When deferDetail is on, fetch() returns BARE movies (showtimes + poster + the
  // per-film op.s detail URL) and the detail is filled later by an EnrichDetails
  // task via `fetchFilmDetail`; off (default), it enriches inline as before.
  def fetch(): Seq[CinemaMovie] = {
    val bare = fetchBare()
    if (deferDetail) bare else enrichInline(bare)
  }

  private def fetchBare(): Seq[CinemaMovie] = {
    val days     = (0 until WindowDays).map(today.plusDays(_)).toList
    val dayLists = ParallelDetailFetch.keyed("nowe-horyzonty-days", days, 1.minute, maxConcurrent = 1)(dayUrl) { url =>
      Try(http.get(url)).toOption.flatMap(listaHtml)
    }
    val slots = days.flatMap(d => dayLists.getOrElse(d, None).toSeq.flatMap(parseDay(_, d)))

    slots.groupBy(_.filmId).toSeq.flatMap { case (filmId, group) =>
      val primary    = group.head
      val showtimes  = group.distinctBy(_.eventId).sortBy(_.dateTime)
                         .map(s => Showtime(s.dateTime, Some(s.bookingUrl), None, Nil))
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title = primary.title),
        cinema    = cinema,
        posterUrl = group.flatMap(_.poster).headOption,
        filmUrl   = Some(s"$BaseUrl/op.s?id=$filmId"),
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      ))
    }
  }

  // Inline path: fetch each film's detail in parallel through the same
  // `fetchFilmDetail` the deferred path uses, then merge non-destructively.
  private def enrichInline(movies: Seq[CinemaMovie]): Seq[CinemaMovie] = {
    val urls = movies.flatMap(_.filmUrl).distinct
    if (urls.isEmpty) movies
    else {
      val metas = ParallelDetailFetch("nowe-horyzonty-details", urls, 1.minute)(u => fetchFilmDetail(u))
      movies.map(m => m.filmUrl.flatMap(metas.get).flatten.map(_.applyTo(m)).getOrElse(m))
    }
  }

  /** Deferred per-film detail fetch — the EnrichDetails task calls this with the
   *  movie's filmUrl (`https://www.kinonh.pl/op.s?id=<id>`). None on fetch failure
   *  so the task stays stale and is retried. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    Try(detailHttp.get(ref)).toOption.map { html =>
      val d = NoweHoryzontyClient.parseDetail(html)
      FilmDetail(
        synopsis       = d.synopsis,
        director       = d.director,
        runtimeMinutes = d.runtimeMinutes,
        releaseYear    = d.year,
        originalTitle  = d.originalTitle,
        countries      = d.countries,
        genres         = d.genres,
        posterUrl      = d.poster
      )
    }

  /** Pull the `lista` HTML blob out of a `rep.json` response. */
  private def listaHtml(body: String): Option[String] =
    Try((Json.parse(body) \ "lista").asOpt[String]).toOption.flatten.filter(_.trim.nonEmpty)

  /** Parse one day's `lista` blob: each `div.boks` is a film, its
   *  `div.seanserep a.xseans` anchors are that day's slots. The date is the
   *  `date` we requested — the blob carries only the `HH:mm` time per slot. */
  private def parseDay(lista: String, date: LocalDate): Seq[RawSlot] =
    Jsoup.parse(lista).select("div.boks").asScala.toSeq.flatMap { card =>
      val link = Option(card.selectFirst("a.tyt[href^=\"op.s?id=\"]"))
                   .orElse(Option(card.selectFirst("a[href^=\"op.s?id=\"]")))
      (for {
        a      <- link
        filmId <- FilmIdPat.findFirstMatchIn(a.attr("href")).map(_.group(1))
        title   = a.text.trim if title.nonEmpty
      } yield card.select("div.seanserep a.xseans[href]").asScala.toSeq.flatMap { slot =>
        val href = slot.attr("href")
        for {
          eventId <- NoweHoryzontyClient.EventIdPat.findFirstMatchIn(href).map(_.group(1))
          time    <- ScraperParse.parseHHmm(slot.text.trim)
        } yield RawSlot(filmId, title, posterOf(card), eventId, date.atTime(time),
                        if (href.startsWith("http")) href else s"$BaseUrl/$href")
      }).getOrElse(Seq.empty)
    }

  /** The poster lives in an inline `<style>` block inside `span.ilustr`
   *  (`background-image: url(...)`), not a `style=` attribute. */
  private def posterOf(card: Element): Option[String] =
    Option(card.selectFirst("span.ilustr")).map(_.html)
      .flatMap(ScraperParse.cssUrl)
      .map(u => if (u.startsWith("http")) u else s"$BaseUrl/${u.stripPrefix("/")}")
}

object NoweHoryzontyClient {

  private val EventIdPat = """eventId=(\d+)""".r

  final case class Detail(
    runtimeMinutes: Option[Int],
    year:           Option[Int],
    originalTitle:  Option[String],
    countries:      Seq[String],
    genres:         Seq[String],
    director:       Seq[String],
    synopsis:       Option[String],
    poster:         Option[String]
  )
  object Detail { val empty: Detail = Detail(None, None, None, Seq.empty, Seq.empty, Seq.empty, None, None) }

  private val RuntimePat = """(\d+)""".r
  private val YearPat    = """\b((?:19|20)\d{2})\b""".r

  private def crrow(doc: org.jsoup.nodes.Document, label: String): Option[String] =
    doc.select("div.crrow").asScala.find(_.text.toLowerCase.contains(label))
      .map(_.text.replaceFirst(s"(?i)^[^:]*:\\s*", "").trim).filter(_.nonEmpty)

  /** Parse the op.s film page for metadata. Selectors mirror the page's
   *  `czas:` / `produkcja:` / `gatunek:` credit rows plus the synopsis block. */
  def parseDetail(html: String): Detail = {
    val doc = Jsoup.parse(html)
    val runtime = crrow(doc, "czas").flatMap(s => RuntimePat.findFirstMatchIn(s).map(_.group(1).toInt))
    val prod    = crrow(doc, "produkcja")
    val year    = prod.flatMap(s => YearPat.findFirstMatchIn(s).map(_.group(1).toInt))
    val countries = prod.map(s => YearPat.replaceAllIn(s, "")).map(_.trim.stripSuffix(","))
                      .toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
    val genreRaw = crrow(doc, "gatunek")
      .orElse(Option(doc.selectFirst("h4:contains(gatunek)")).map(_.text))
      .map(_.replaceFirst("(?i)^[^:]*:\\s*", "").trim)
      // The genre run is sometimes followed by an age-rating clause in the same
      // element ("Dramat, Kryminał Kategoria Wiekowa: 16+") — drop it.
      .map(_.split("(?i)kategoria").head.trim).filter(_.nonEmpty)
    val genres  = genreRaw.toSeq.flatMap(_.split("[,/]").map(_.trim).filter(_.nonEmpty))
                    .map(tools.TextNormalization.titleCaseIfAllLower)
    val director = Option(doc.selectFirst("h4:contains(reż.) a")).map(_.text.trim)
                    .filter(_.nonEmpty).toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
    val original = Option(doc.selectFirst("h4.tytulorg")).map(_.text.trim).filter(_.nonEmpty)
    val synopsis = Option(doc.selectFirst("div.txt.wciecia.opisf p")).map(_.text.trim).filter(_.length > 20)
    val poster   = Option(doc.selectFirst("div.plakat img[src]")).map(_.attr("src"))
                    .filter(_.nonEmpty).map(u => if (u.startsWith("http")) u else s"https://www.kinonh.pl/${u.stripPrefix("/")}")
    Detail(runtime, year, original, countries, genres, director, synopsis, poster)
  }
}
