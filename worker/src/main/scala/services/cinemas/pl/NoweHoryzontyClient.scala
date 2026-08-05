package services.cinemas.pl

import play.api.libs.json.Json
import models._
import org.jsoup.Jsoup
import tools.{CachingDetailFetch, HttpFetch}
import services.cinemas.common.{ChunkedCinemaScraper, CinemaScraper, DetailEnricher, DetailFetchOutcome, FilmDetail, ScrapeHorizon}

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
  private val FilmIdPat   = """op\.s\?id=(\d+)""".r

  private case class RawSlot(filmId: String, title: String, eventId: String,
                             dateTime: LocalDateTime, bookingUrl: String)

  private def dayUrl(date: LocalDate): String =
    s"$BaseUrl/rep.json?dzien=${date.format(DayFmt)}&forwardback=$Forwardback"

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  /** Walk forward until the programme runs out, instead of fetching a fixed week.
   *
   *  `rep.json` answers for ANY date — it served Besson's "Joanna d'Arc" on
   *  2026-08-27 while the scrape only ever asked for seven days — so a fixed window
   *  silently hid every screening past it. That is the horizon cap `ScrapeHorizon`
   *  exists to forbid, and it is not a small tail here: this is the country's
   *  largest arthouse, and what lives past a week is precisely its retrospectives
   *  and Mistrzowie Kina cycles. The site's own day-picker advertises only five
   *  days, so there is no nav to read — the days have to be probed.
   *
   *  Bounded by `ScrapeHorizon.MaxDays` and stopped by [[MaxEmptyDays]] consecutive
   *  blank days, so a venue that keeps publishing keeps being read while a dormant
   *  one costs a fortnight of small requests and no more. A day that FAILS to fetch
   *  counts as blank for the stop rule, exactly as `MsiClient` treats a failed
   *  month: a missing day is indistinguishable from a quiet one, and treating it as
   *  "keep going" would walk two years on every blip.
   *
   *  Days are grouped [[DaysPerChunk]] to a chunk so widening the window costs
   *  chunk TASKS in weeks rather than days — the fan-out that
   *  `project_scrape_caps_count_venues_not_tasks` is about. */
  def planChunks(): Seq[String] = {
    val lastDay = today.plusDays(ScrapeHorizon.MaxDays)
    var day      = today
    var emptyRun = 0
    val live     = Seq.newBuilder[LocalDate]
    while (!day.isAfter(lastDay) && emptyRun < NoweHoryzontyClient.MaxEmptyDays) {
      val hasFilms = Try(http.get(dayUrl(day))).toOption.flatMap(listaHtml).exists(FilmIdPat.findFirstIn(_).isDefined)
      if (hasFilms) { live += day; emptyRun = 0 } else emptyRun += 1
      day = day.plusDays(1)
    }
    live.result().map(_.toString).grouped(NoweHoryzontyClient.DaysPerChunk).map(_.mkString(",")).toSeq
  }

  /** One chunk's days → their films (slots grouped by film id). A throw
   *  reschedules just this chunk's task. */
  def fetchChunk(key: String): Seq[CinemaMovie] =
    moviesFrom(key.split(",").toSeq.map(LocalDate.parse).flatMap { d =>
      listaHtml(http.get(dayUrl(d))).toSeq.flatMap(parseDay(_, d))
    })

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
   *  so the task stays stale and is retried.
   *
   *  A durable 404/410 escapes rather than folding into None, so a page that is
   *  gone for good gets stamped instead of retried every tick — see [[DetailFetchOutcome]]. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    DetailFetchOutcome.transientToNone(detailHttp.get(ref)).map { html =>
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

  /** How many consecutive blank days end the walk. A fortnight clears the gap a
   *  cycle-driven arthouse leaves between series — its programme is not a
   *  continuous run — while a dormant venue still costs fourteen small requests.
   *  A stop rule, not a horizon: `ScrapeHorizon.MaxDays` is the bound. */
  val MaxEmptyDays = 14

  /** Days per chunk task. Widening the window from one week to the whole
   *  programme must not multiply the chunk-task fan-out day for day. */
  val DaysPerChunk = 7


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
