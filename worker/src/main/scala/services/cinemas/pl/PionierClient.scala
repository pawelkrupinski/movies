package services.cinemas.pl

import tools.{CachingDetailFetch, HttpFetch}
import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Document, Element, TextNode}
import services.cinemas.common.{CinemaScraper, DetailEnricher, FilmDetail, SlotsToMovies}

import java.time.{LocalDate, LocalDateTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Pionier 1907 (Szczecin) — the oldest continuously-operating cinema in
 * the world, run by Dom Kultury "Krzemień". Its WordPress repertoire page at
 * `/repertuar/` is a single server-rendered list covering the whole programme
 * (no pagination): a flat `div.repertuar-list` whose children alternate between
 *   - `h2.naglowek[id=dzien_YYYY-MM-DD]` — a day heading that carries the ISO
 *     date in its `id` and applies to every screening until the next heading,
 *   - `div.event_box` — one screening, with
 *       * `h3 a span`            → film title,
 *       * `h3 a[href]`           → the per-film `/event/<slug>` detail page,
 *       * `h2` text node (`HH:MM`) before the `<br>` → start time, and
 *         `small.sala`           → auditorium (Sala Czerwona / Kiniarnia / …),
 *       * `a.bilet[href]`        → the biletomat.pl booking URL.
 *
 * Headings and event boxes are SIBLINGS (the box isn't nested under the day),
 * so the parser walks the list in document order and carries the last seen
 * date forward onto the boxes that follow it — the same shape as Kino Sfinks's
 * carry-forward, but keyed off the heading's `id` rather than a blank cell.
 *
 * The listing carries no film identity beyond the (often retrospective-prefixed)
 * title — no year, director or cast — so a yearless arthouse title that collides
 * with a same-named TMDB entry can't be disambiguated and fails to resolve
 * ("Federico Fellini: Słodkie życie" → bare "Słodkie życie" matched both the
 * 1960 Fellini film and a 2024 one → tmdbNoMatch). The per-film `/event/<slug>`
 * page DOES carry that identity: a `<p>` block of `<b>Label: </b>value<br/>`
 * pairs (`Reżyseria`, `Rok`, `Gatunek`, `Produkcja`, `Obsada`, `Czas trwania`).
 * We defer it to an `EnrichDetails` task ([[fetchFilmDetail]]); the production
 * year alone is enough to break the collision, so `defersTmdbResolution` stays
 * true (the default) — the row holds until the detail lands, then resolves with
 * the year in hand. (Synopsis is left to TMDB: this page interleaves an awards
 * dump with the prose, so a clean cut isn't reliable except where the page puts
 * the synopsis in its own `<p>`.)
 */
class PionierClient(http: HttpFetch, override val cinema: Cinema = KinoPionier)
    extends CinemaScraper with DetailEnricher {

  import PionierClient._

  // Event pages are static across passes for a live film, so cache them.
  private val detailHttp = new CachingDetailFetch(http)

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(PageUrl)

  override val detailGroup: String = "kino-pionier"

  /** Deferred per-film detail — director, production year, countries, genres,
   *  cast and runtime off the `<b>Label:</b>value` block, plus a stand-alone
   *  synopsis paragraph when the page keeps one. None on a fetch failure so the
   *  task stays stale and retries rather than recording an empty result. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    Try(detailHttp.get(ref)).toOption.map(html => parseDetail(Jsoup.parse(html)))

  def fetch(): Seq[CinemaMovie] = {
    val document = Jsoup.parse(http.get(PageUrl))
    val slots = parseSlots(document)

    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking, s.room)) { (title, group, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
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
}

object PionierClient {

  val BaseUrl = "https://pionier1907.pl"
  val PageUrl = s"$BaseUrl/repertuar/"

  private val DayIdPat = """dzien_(\d{4}-\d{2}-\d{2})""".r
  private val YearTokenPat = """\b(?:19|20)\d{2}\b""".r

  private case class RawSlot(
    title:    String,
    dateTime: LocalDateTime,
    room:     Option[String],
    booking:  Option[String],
    filmUrl:  Option[String]
  )

  /** Walk `.repertuar-list` children in document order: day headings set the
    * current date, event boxes inherit it. */
  private def parseSlots(document: org.jsoup.nodes.Document): Seq[RawSlot] = {
    var carriedDate: Option[LocalDate] = None
    val nodes = document.select("div.repertuar-list").asScala.headOption
      .map(_.children.asScala.toSeq)
      .getOrElse(Seq.empty)

    nodes.flatMap { element =>
      if (element.hasClass("naglowek")) {
        carriedDate = headingDate(element)
        None
      } else if (element.hasClass("event_box")) {
        for {
          date  <- carriedDate
          title <- Option(element.selectFirst("h3 a span")).map(_.text.trim).filter(_.nonEmpty)
          time  <- Option(element.selectFirst("h2")).flatMap(h => ScraperParse.parseHHmm(h.text))
        } yield {
          val room    = Option(element.selectFirst("small.sala")).map(_.text.trim).filter(_.nonEmpty)
          val booking = Option(element.selectFirst("a.bilet[href]")).map(_.attr("href"))
            .filter(h => h.nonEmpty && h != "#")
          val filmUrl = Option(element.selectFirst("h3 a[href]")).map(_.attr("href")).filter(_.nonEmpty)
          RawSlot(title, LocalDateTime.of(date, time), room, booking, filmUrl)
        }
      } else None
    }
  }

  private def headingDate(element: Element): Option[LocalDate] =
    DayIdPat.findFirstMatchIn(element.id)
      .flatMap(m => Try(LocalDate.parse(m.group(1))).toOption)

  // ── Per-film detail page ────────────────────────────────────────────────────

  /** Parse a `/event/<slug>` page into its film-level metadata. The detail block
    * is a run of `Label: value` lines separated by `<br/>`; the labels are bold
    * (`<b>Label: </b>value`) on most pages and plain text on the Fellini
    * retrospective pages, so we read them line-wise off the rendered text rather
    * than off the `<b>` tags. Non-breaking spaces glue labels to values and
    * are normalised to plain spaces (denbsp). */
  private[cinemas] def parseDetail(document: Document): FilmDetail = {
    val content = Option(document.selectFirst("div.col-lg-9")).getOrElse(document.body)
    val fields  = labelledValues(content)
    // Most films carry a dedicated `Rok:` field; the Fellini-retrospective pages
    // instead fold the year into `Produkcja:` ("Włochy, Francja, 1957"), so fall
    // back to a year token there and strip it out of the country list.
    val prod = fields.get("produkcja")
    FilmDetail(
      synopsis       = parseSynopsis(content),
      cast           = splitList(fields.get("obsada")),
      director       = splitList(fields.get("reżyseria")),
      runtimeMinutes = fields.get("czas trwania").flatMap(parseRuntime),
      releaseYear    = fields.get("rok").flatMap(yearOf).orElse(prod.flatMap(yearOf)),
      originalTitle  = fields.get("tytuł oryginalny"),
      countries      = splitList(prod.map(p => YearTokenPat.replaceAllIn(p, ""))),
      genres         = splitList(fields.get("gatunek"))
    )
  }

  private def yearOf(s: String): Option[Int] =
    YearTokenPat.findFirstMatchIn(s).map(_.matched.toInt)

  // The metadata fields we read, lower-cased. `tytuł oryginalny` is an extra
  // TMDB-resolution hint the retrospective pages expose.
  private val KnownLabels = Set(
    "reżyseria", "rok", "gatunek", "produkcja", "obsada", "czas trwania", "tytuł oryginalny")
  // Label markers (with their colon) that flag a `<p>` as metadata/awards rather
  // than synopsis prose. The colon is what keeps them out of a sentence — bare
  // "Nagrody" appears in prose ("Nagrody Nobla"), "Nagrody:" does not.
  private val MetadataMarkers =
    Seq("Reżyseria:", "Scenariusz:", "Produkcja:", "Obsada:", "Czas trwania:", "Nagrody:", "Tytuł oryginalny:", "Muzyka:")

  /** `label → value` map from the `Label: value` lines, keyed by lower-cased
    * label. Reads the rendered text with `<br/>` turned into line breaks so it
    * works whether the label is bold (`<b>Label: </b>value`, most pages) or
    * plain text (the Fellini retrospective pages). First occurrence of a label
    * wins; empty values are dropped so a blank `Reżyseria:` can't shadow a real
    * one. */
  private def labelledValues(content: Element): Map[String, String] =
    textLines(content).flatMap { line =>
      val idx = line.indexOf(':')
      if (idx <= 0) None
      else {
        val label = line.substring(0, idx).trim.toLowerCase(java.util.Locale.ROOT)
        val value = line.substring(idx + 1).trim
        if (KnownLabels.contains(label) && value.nonEmpty) Some(label -> value) else None
      }
    }.reverse.toMap // reverse so the first occurrence wins on duplicate labels

  /** The content block's text split into trimmed, non-empty lines, with `<br/>`
    * and `<p>` boundaries rendered as line breaks. */
  private def textLines(content: Element): Seq[String] = {
    val work = content.clone()
    work.select("br").asScala.foreach(_.replaceWith(new TextNode("\n")))
    work.select("p").asScala.foreach(_.prependChild(new TextNode("\n")))
    denbsp(work.wholeText()).split("\n").iterator.map(_.trim).filter(_.nonEmpty).toSeq
  }

  /** Stand-alone synopsis paragraphs — `<p>` blocks of prose carrying none of the
    * metadata/awards markers. When a page folds the synopsis into the same `<p>`
    * as the metadata + awards (the retrospective pages), no `<p>` qualifies and
    * TMDB's synopsis is used instead. */
  private def parseSynopsis(content: Element): Option[String] = {
    val paras = content.select("p").asScala.toSeq
      .map(p => denbsp(p.text).trim)
      .filter(t => t.length > 80 && !MetadataMarkers.exists(t.contains))
    Some(paras.mkString("\n\n")).filter(_.nonEmpty)
  }

  /** "176 min", "70 min.", "108 min." or "1h 22min" → minutes, clamped to a
    * plausible feature length so a stray number can't masquerade as a runtime. */
  private def parseRuntime(s: String): Option[Int] = {
    val hours = """(\d+)\s*h""".r.findFirstMatchIn(s).map(_.group(1).toInt)
    val mins  = """(\d+)\s*min""".r.findFirstMatchIn(s).map(_.group(1).toInt)
    val total = hours match {
      case Some(h) => Some(h * 60 + mins.getOrElse(0))
      case None    => mins.orElse("""(\d+)""".r.findFirstMatchIn(s).map(_.group(1).toInt))
    }
    total.filter(n => n >= 30 && n <= 300)
  }

  private def denbsp(s: String): String = s.replace(' ', ' ')

  private def splitList(s: Option[String]): Seq[String] =
    s.toSeq.flatMap(_.split(",")).map(t => denbsp(t).trim).filter(_.nonEmpty)
}
