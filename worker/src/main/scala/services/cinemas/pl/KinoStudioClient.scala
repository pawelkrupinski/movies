package services.cinemas.pl

import java.util.Locale

import services.cinemas.common.ScraperParse
import models._
import tools.{HttpFetch, HttpRead}
import org.jsoup.Jsoup
import org.jsoup.nodes.Element
import services.cinemas.common.CinemaScraper

import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Studio (Opole, run by MDK Opole). Its repertoire at
 * `mdk.opole.pl/kino-studio` is a hand-edited CMS page inside the
 * `div.ckeditor.clearfix` content block. One block per film, in document order:
 *   - an `<img>` (bare, or alone inside a `<p>`) → the poster heading the block
 *   - `<h3>DD.MM (...)` → the screening date (day.month, no year — inferred
 *     from `today`; a month before today rolls to next year)
 *   - `<p>godziny seansów: <strong>HH:MM [i HH:MM ...]</strong>` → times,
 *     separated by " i "; the hour separator is `:` since the 2026-08 rebuild
 *     and was `.` before it
 *   - `<h2>` (`<h1>` before the rebuild) → the film title
 *   - `<p>gatunek / reżyseria / obsada / produkcja / czas trwania: <strong>…`
 *     → genre, director, cast, countries + release year, runtime
 *   - `<p><em>` → synopsis (first `<em>` paragraph)
 *
 * Box-office only — no booking URLs. The page carries the cinema's whole
 * season, one weekly slot per film, so blocks repeat; before the rebuild it
 * usually held a single film.
 */
class KinoStudioClient(
  http:             HttpFetch,
  override val cinema: Cinema = KinoStudio,
  today:            => LocalDate
) extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoStudioClient.BaseUrl)
  override def sourceUrl: Option[String] = Some(KinoStudioClient.RepertoireUrl)

  def fetch(): Seq[CinemaMovie] = {
    var lastFailure = Option.empty[Throwable]

    // Take the FIRST page that actually rendered the CMS content div, fetching
    // lazily so a healthy in-season page costs one request. MDK moves the cinema
    // between two slugs (`kino-studio` in season, `kino-studio-przerwa` over a
    // break) and leaves whichever one is currently dead 404ing — and the site's
    // "Strona nie znaleziona" body carries `ckeditor` elements of its own, so
    // only the content div tells a real page from it.
    val content = KinoStudioClient.PageUrls.iterator.map { url =>
      val body = Try(HttpRead.page(http, url))
      body.failed.foreach(error => lastFailure = Some(error))
      body.toOption.filter(KinoStudioClient.hasContent)
    }.collectFirst { case Some(html) => html }

    content match {
      case Some(html) => KinoStudioClient.parse(html, cinema, today)
      case None       =>
        // NEITHER slug rendered content. That is a dead source, not a venue with
        // no films — report it as a failure so it surfaces RED on /uptime rather
        // than white, where it would be indistinguishable from a genuinely
        // film-dormant venue. Same guard as MsiClient / KinoAwangarda2Client /
        // KinoPatriaClient.
        lastFailure.foreach(throw _)
        throw new IllegalStateException(
          s"No repertoire content at any of ${KinoStudioClient.PageUrls.mkString(", ")} " +
          s"— every page rendered without a `${KinoStudioClient.ContentSelector}` block (404 body?)")
    }
  }
}

object KinoStudioClient {

  val BaseUrl       = "https://mdk.opole.pl"
  val RepertoireUrl = s"$BaseUrl/kino-studio"
  /** MDK parks the cinema on this slug while it is on a seasonal break, and
   *  leaves the in-season slug dead until the season restarts. */
  val BreakUrl      = s"$BaseUrl/kino-studio-przerwa"
  /** In-season slug first, so a live repertoire always wins over the break page. */
  val PageUrls      = Seq(RepertoireUrl, BreakUrl)

  /** The CMS content block. Its ABSENCE is how a dead slug is told apart from a
   *  real page. `clearfix` is load-bearing: since the 2026-08 rebuild the site's
   *  404 body ships `ckeditor` elements too (a `title-section`, a modal pane),
   *  and only an editable content block carries both classes. */
  val ContentSelector = "div.ckeditor.clearfix"

  private def hasContent(html: String): Boolean =
    Jsoup.parse(html, BaseUrl).selectFirst(ContentSelector) != null

  /** "DD.MM" at the start of an `<h3>` heading. */
  private val DatePat = """^(\d{1,2})\.(\d{1,2})\b""".r

  /** "HH:MM" time tokens. The rebuilt page uses `:`; the older one used `.`. */
  private val TimePat = ScraperParse.ClockPartsDotted.r

  /** Trailing release year on the `produkcja:` line ("Izrael, Włochy 2024"). */
  private val YearPat = """(\d{4})\s*$""".r

  /** Runtime on the `czas trwania:` line ("108 min"). */
  private val RuntimePat = """(\d{1,3})\s*min""".r

  /** The `produkcja / gatunek / reżyseria / obsada / czas trwania` fields of
   *  one film block. Each is read once: the first line that carries it wins. */
  private final case class FilmMeta(
    genres:    Seq[String] = Seq.empty,
    director:  Seq[String] = Seq.empty,
    cast:      Seq[String] = Seq.empty,
    countries: Seq[String] = Seq.empty,
    year:      Option[Int] = None,
    runtime:   Option[Int] = None
  )

  /** One film's block as the scan has read it so far. */
  private final case class FilmBlock(
    dates:    Seq[LocalDate]  = Seq.empty,
    times:    Seq[LocalTime]  = Seq.empty,
    poster:   Option[String]  = None,
    title:    Option[String]  = None,
    meta:     FilmMeta        = FilmMeta(),
    synopsis: Option[String]  = None
  ) {
    def showtimes: Seq[LocalDateTime] =
      (for { d <- dates; t <- times } yield LocalDateTime.of(d, t)).distinct.sorted
  }

  /** The scan's state: the films committed so far and the block being read. */
  private final case class Scan(films: Vector[(String, FilmBlock)] = Vector.empty, block: FilmBlock = FilmBlock()) {
    /** Commit the block being read, if it has a title — the element at hand
     *  belongs to the next one. A titled block with no showtimes is dropped; an
     *  untitled one keeps accumulating. */
    def flushed: Scan = block.title match {
      case Some(title) =>
        Scan(if (block.showtimes.nonEmpty) films :+ (title -> block) else films, FilmBlock())
      case None => this
    }
    def edit(f: FilmBlock => FilmBlock): Scan = copy(block = f(block))
  }

  def parse(html: String, cinema: Cinema, today: LocalDate): Seq[CinemaMovie] = {
    val doc = Jsoup.parse(html, BaseUrl)
    // Only ever scan the CMS content div. The old fallback to the whole `<body>`
    // meant a 404 body (or any redesign) got the site's nav and footer fed to the
    // date/title state machine, which can only manufacture junk films — never
    // recover the real ones. `fetch` guarantees the div is present.
    val films = Option(doc.selectFirst(ContentSelector)).toSeq.flatMap(extractFilms(_, today))
    films.map { case (title, block) =>
      val meta = block.meta
      CinemaMovie(
        movie     = Movie(ScraperParse.stripFormatTags(title), genres = meta.genres,
                          releaseYear = meta.year, runtimeMinutes = meta.runtime, countries = meta.countries),
        cinema    = cinema,
        posterUrl = block.poster,
        filmUrl   = Some(RepertoireUrl),
        synopsis  = block.synopsis,
        cast      = meta.cast,
        director  = meta.director,
        showtimes = block.showtimes.map(dt => Showtime(dt, None)).sortBy(_.dateTime)
      )
    }
  }

  /** The poster that heads a film's block: a bare `<img>` child (the rebuilt
   *  page) or a `<p>` holding nothing but an `<img>` (the older one). */
  private def posterOf(el: Element): Option[String] = {
    val img =
      if (el.tagName == "img") Some(el)
      else if (el.tagName == "p" && el.text.trim.isEmpty) Option(el.selectFirst("img"))
      else None
    img.map(_.attr("abs:src")).filter(_.nonEmpty)
  }

  /**
   * Walk the children of the content div in document order. The structure is
   * free-form CMS HTML, so we use a state machine: each `<h3>` that starts with
   * "DD.MM" opens a new date context; `<h1>`/`<h2>` names the film; accumulated
   * (date, time) pairs are folded into showtimes when the NEXT film's block
   * starts or the element stream ends.
   *
   * A block starts at its poster, at its date header, or at the `<hr>` the CMS
   * puts between films — whichever comes first — so a season page of 14 blocks
   * keeps each film's dates to itself. Flushing on the title instead (as this
   * did while the page only ever held one film) pooled every date onto film #1
   * and left the rest with none, because the date header precedes the title.
   */
  private def extractFilms(container: Element, today: LocalDate): Seq[(String, FilmBlock)] =
    container.children().asScala.foldLeft(Scan())((scan, el) => step(scan, el, today)).flushed.films

  /** One content-div child read into the scan. */
  private def step(scan: Scan, el: Element, today: LocalDate): Scan =
    posterOf(el) match {
      case Some(src) => scan.flushed.edit(_.copy(poster = Some(src)))
      case None => el.tagName match {
        case "hr"        => scan.flushed
        case "h1" | "h2" => scan.edit(_.copy(title = Some(el.text.trim).filter(_.nonEmpty)))
        case "h3"        => dateHeading(scan, el, today)
        case "p"         => scan.edit(paragraph(_, el))
        case _           => scan
      }
    }

  /** Date line — "25.06 (czwartek) - ..."; only counted when the date regex
   *  matches, so non-date `<h3>`s (e.g. "REPERTUAR") are ignored. Times reset on
   *  every `<h3>` — each date block carries its own "godziny seansów" in the
   *  following `<p>`. */
  private def dateHeading(scan: Scan, el: Element, today: LocalDate): Scan = {
    val dated = DatePat.findFirstMatchIn(el.text).fold(scan) { m =>
      val date = ScraperParse.monthDay(m.group(1).toInt, m.group(2).toInt).flatMap(ScraperParse.upcomingMonthDate(_, today))
      scan.flushed.edit(block => block.copy(dates = block.dates ++ date))
    }
    dated.edit(_.copy(times = Seq.empty))
  }

  /** A `<p>`: the "godziny seansów:" times line, the metadata block, or the
   *  first synopsis paragraph (em-wrapped prose) — whichever it is first. */
  private def paragraph(block: FilmBlock, el: Element): FilmBlock = {
    val text  = el.text
    val lower = text.toLowerCase(Locale.ROOT)
    if (lower.contains("godziny") || lower.contains("seansów") || lower.contains("seansow")) {
      val strong = Option(el.selectFirst("strong")).map(_.text).getOrElse(text)
      block.copy(times = TimePat.findAllMatchIn(strong).flatMap(m => ScraperParse.clockAt(m, 1)).toSeq)
    }
    else if (MetaLabels.exists(lower.contains)) block.copy(meta = metadata(block.meta, el))
    else if (block.synopsis.isEmpty && el.select("em").size > 0)
      block.copy(synopsis = Option(el.selectFirst("em")).map(_.text.trim).filter(_.length > 20))
    else block
  }

  /** Metadata block — gatunek, reżyseria, obsada, produkcja, czas trwania, each
   *  on a `<br/>`-separated line of ONE paragraph. `wholeOwnText` has no `<br>`,
   *  so the paragraph's HTML is split on `<br>` and each line's value read from
   *  its `<strong>`. */
  private def metadata(meta: FilmMeta, el: Element): FilmMeta =
    el.html.split("(?i)<br\\s*/?>").foldLeft(meta)(metaLine)

  /** One `label:&nbsp;<strong>value</strong>` line; a comma-split of the bold
   *  text yields a list. A field already read is kept. */
  private def metaLine(meta: FilmMeta, line: String): FilmMeta = {
    val label = line.toLowerCase(Locale.ROOT)
    if (label.contains("gatunek") && meta.genres.isEmpty) meta.copy(genres = boldList(line))
    else if ((label.contains("reżyseria") || label.contains("rezyseria")) && meta.director.isEmpty)
      meta.copy(director = boldList(line))
    else if (label.contains("obsada") && meta.cast.isEmpty) meta.copy(cast = boldList(line))
    // "produkcja: Izrael, Włochy 2024" — countries then the release year, the
    // strongest TMDB-identity hint this page carries.
    else if (label.contains("produkcja") && meta.countries.isEmpty && meta.year.isEmpty) {
      val value = bold(line)
      meta.copy(
        year      = YearPat.findFirstMatchIn(value).flatMap(m => Try(m.group(1).toInt).toOption),
        countries = YearPat.replaceAllIn(value, "").split(",").map(_.trim).filter(_.nonEmpty).toSeq)
    }
    else if (label.contains("czas trwania") && meta.runtime.isEmpty)
      meta.copy(runtime = RuntimePat.findFirstMatchIn(bold(line)).flatMap(m => Try(m.group(1).toInt).toOption))
    else meta
  }

  /** A line's `<strong>` text; `&nbsp;` decodes to a non-breaking space,
   *  normalised back to a plain space before the trim. */
  private def bold(line: String): String =
    Option(Jsoup.parseBodyFragment(line).body.selectFirst("strong"))
      .map(_.text).getOrElse("").replaceAll("\\u00A0", " ").trim

  private def boldList(line: String): Seq[String] =
    bold(line).split(",").map(_.trim).filter(_.nonEmpty).toSeq

  /** Labels of the one metadata paragraph — any of them means the whole
   *  `<br>`-separated block is worth splitting apart. */
  private val MetaLabels =
    Seq("gatunek", "reżyseria", "rezyseria", "obsada", "produkcja", "czas trwania")
}
