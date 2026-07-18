package services.cinemas.pl

import play.api.libs.json.Json
import models._
import org.jsoup.Jsoup
import tools.{CachingDetailFetch, HttpFetch}
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper, DetailEnricher, FilmDetail}

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, ZoneId}
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
class NoweHoryzontyClient(http: HttpFetch, today: LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))) extends ChunkedCinemaScraper with DetailEnricher {

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

  private case class RawSlot(filmId: String, title: String, eventId: String,
                             dateTime: LocalDateTime, bookingUrl: String)

  private def dayUrl(date: LocalDate): String =
    s"$BaseUrl/rep.json?dzien=${date.format(DayFmt)}&forwardback=$Forwardback"

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  /** A fixed one-week window — no nav fetch needed; one chunk per day. */
  def planChunks(): Seq[String] = (0 until WindowDays).map(today.plusDays(_).toString)

  /** One day's `rep.json` blob → that day's films (slots grouped by film id). A
   *  throw reschedules just this day's chunk task. */
  def fetchChunk(date: String): Seq[CinemaMovie] = {
    val d = LocalDate.parse(date)
    moviesFrom(listaHtml(http.get(dayUrl(d))).toSeq.flatMap(parseDay(_, d)))
  }

  private def moviesFrom(slots: Seq[RawSlot]): Seq[CinemaMovie] =
    slots.groupBy(_.filmId).toSeq.flatMap { case (filmId, group) =>
      val primary    = group.head
      val showtimes  = group.distinctBy(_.eventId).sortBy(_.dateTime)
                         .map(s => Showtime(s.dateTime, Some(s.bookingUrl), None, Nil))
      if (showtimes.isEmpty) None
      // No listing poster on purpose: the `span.ilustr` background-image is a
      // gallery still (`glw_…_mini.jpg`), not the film poster. We leave it None
      // so detail enrichment supplies the real `div.plakat` poster — the merge
      // (`slot.posterUrl.orElse(detail)`) keeps any listing value, so emitting
      // the still here would permanently shadow the correct poster.
      else Some(CinemaMovie(
        movie     = Movie(title = primary.title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = Some(s"$BaseUrl/op.s?id=$filmId"),
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      ))
    }

  /** Deferred per-film detail fetch — the EnrichDetails task calls this with the
   *  movie's filmUrl (`https://www.kinonh.pl/op.s?id=<id>`). None on fetch failure
   *  so the task stays stale and is retried. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    Try(detailHttp.get(ref)).toOption.map { html =>
      val detail = NoweHoryzontyClient.parseDetail(html)
      FilmDetail(
        synopsis       = detail.synopsis,
        director       = detail.director,
        runtimeMinutes = detail.runtimeMinutes,
        releaseYear    = detail.year,
        originalTitle  = detail.originalTitle,
        countries      = detail.countries,
        genres         = detail.genres,
        posterUrl      = detail.poster
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
        } yield RawSlot(filmId, title, eventId, date.atTime(time),
                        if (href.startsWith("http")) href else s"$BaseUrl/$href")
      }).getOrElse(Seq.empty)
    }
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

  // A bare foreign-language label (`FR:` / `EN:` …) inside the synopsis container
  // marks the start of a translated copy of the plot — text + everything after it
  // isn't part of the Polish synopsis. The leading `&nbsp;` survives jsoup `.text`
  // as U+00A0, so callers normalise it before matching.
  private val ForeignLangLabel = """(?i)^(fr|en|eng|de|es|it)\s*:$""".r

  /** Extract the Polish synopsis prose from the `div.txt.wciecia.opisf` block.
   *  The block also wraps a `gatunek:` genre `<h4>` (and other event-note `<h4>`s)
   *  plus, for some films, a foreign-language version of the plot behind a bare
   *  `FR:` / `EN:` label `<h4>`. Drop everything from the foreign label onward, then
   *  let [[ScraperParse.cleanSynopsis]] strip the `<h4>` labels and join the
   *  remaining `<p>` paragraphs with blank lines — the old `selectFirst("… p")`
   *  kept only the FIRST paragraph, truncating multi-paragraph synopses. */
  private def synopsisProse(container: org.jsoup.nodes.Element): String = {
    val el = container.clone()
    el.children.asScala
      .find(c => c.tagName == "h4" && ForeignLangLabel.matches(c.text.replace('\u00a0', ' ').trim))
      .foreach { marker =>
        var sib = marker.nextElementSibling()
        while (sib != null) { val next = sib.nextElementSibling(); sib.remove(); sib = next }
        marker.remove()
      }
    ScraperParse.cleanSynopsis(el, "h4")
  }

  private def crrow(document: org.jsoup.nodes.Document, label: String): Option[String] =
    document.select("div.crrow").asScala.find(_.text.toLowerCase.contains(label))
      .map(_.text.replaceFirst(s"(?i)^[^:]*:\\s*", "").trim).filter(_.nonEmpty)

  /** Parse the op.s film page for metadata. Selectors mirror the page's
   *  `czas:` / `produkcja:` / `gatunek:` credit rows plus the synopsis block. */
  def parseDetail(html: String): Detail = {
    val document = Jsoup.parse(html)
    val runtime = crrow(document, "czas").flatMap(s => RuntimePat.findFirstMatchIn(s).map(_.group(1).toInt))
    val prod    = crrow(document, "produkcja")
    val year    = prod.flatMap(s => YearPat.findFirstMatchIn(s).map(_.group(1).toInt))
    val countries = prod.map(s => YearPat.replaceAllIn(s, "")).map(_.trim.stripSuffix(","))
                      .toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
    val genreRaw = crrow(document, "gatunek")
      .orElse(Option(document.selectFirst("h4:contains(gatunek)")).map(_.text))
      .map(_.replaceFirst("(?i)^[^:]*:\\s*", "").trim)
      // The genre run is sometimes followed by an age-rating clause in the same
      // element ("Dramat, Kryminał Kategoria Wiekowa: 16+") — drop it.
      .map(_.split("(?i)kategoria").head.trim).filter(_.nonEmpty)
    val genres  = genreRaw.toSeq.flatMap(_.split("[,/]").map(_.trim).filter(_.nonEmpty))
                    .map(tools.TextNormalization.titleCaseIfAllLower)
    val director = Option(document.selectFirst("h4:contains(reż.) a")).map(_.text.trim)
                    .filter(_.nonEmpty).toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
    val original = Option(document.selectFirst("h4.tytulorg")).map(_.text.trim).filter(_.nonEmpty)
    val synopsis = Option(document.selectFirst("div.txt.wciecia.opisf")).map(synopsisProse).filter(_.length > 20)
    val poster   = Option(document.selectFirst("div.plakat img[src]")).map(_.attr("src"))
                    .filter(_.nonEmpty).map(u => if (u.startsWith("http")) u else s"https://www.kinonh.pl/${u.stripPrefix("/")}")
    Detail(runtime, year, original, countries, genres, director, synopsis, poster)
  }
}
