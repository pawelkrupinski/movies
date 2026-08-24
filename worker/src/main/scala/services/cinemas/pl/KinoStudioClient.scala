package services.cinemas.pl

import models._
import tools.HttpFetch
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
  today:            LocalDate = LocalDate.now(java.time.ZoneId.of("Europe/Warsaw"))
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
      val body = Try(http.get(url))
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
  private val TimePat = """(\d{1,2})[.:](\d{2})""".r

  /** Trailing release year on the `produkcja:` line ("Izrael, Włochy 2024"). */
  private val YearPat = """(\d{4})\s*$""".r

  /** Runtime on the `czas trwania:` line ("108 min"). */
  private val RuntimePat = """(\d{1,3})\s*min""".r

  private case class RawFilm(
    title:     String,
    showtimes: Seq[LocalDateTime],
    posterUrl: Option[String],
    synopsis:  Option[String],
    genres:    Seq[String],
    director:  Seq[String],
    cast:      Seq[String],
    countries: Seq[String],
    year:      Option[Int],
    runtime:   Option[Int]
  )

  def parse(html: String, cinema: Cinema, today: LocalDate): Seq[CinemaMovie] = {
    val doc = Jsoup.parse(html, BaseUrl)
    // Only ever scan the CMS content div. The old fallback to the whole `<body>`
    // meant a 404 body (or any redesign) got the site's nav and footer fed to the
    // date/title state machine, which can only manufacture junk films — never
    // recover the real ones. `fetch` guarantees the div is present.
    val films = Option(doc.selectFirst(ContentSelector)).toSeq.flatMap(extractFilms(_, today))
    films.map { f =>
      CinemaMovie(
        movie     = Movie(ScraperParse.stripFormatTags(f.title), genres = f.genres,
                          releaseYear = f.year, runtimeMinutes = f.runtime, countries = f.countries),
        cinema    = cinema,
        posterUrl = f.posterUrl,
        filmUrl   = Some(RepertoireUrl),
        synopsis  = f.synopsis,
        cast      = f.cast,
        director  = f.director,
        showtimes = f.showtimes.map(dt => Showtime(dt, None)).sortBy(_.dateTime)
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
  private def extractFilms(container: Element, today: LocalDate): Seq[RawFilm] = {
    val children = container.children().asScala.toSeq
    val films    = scala.collection.mutable.Buffer.empty[RawFilm]

    // Mutable state as we scan
    var pendingDates  = Seq.empty[LocalDate]
    var pendingTimes  = Seq.empty[LocalTime]
    var pendingPoster = Option.empty[String]
    var currentTitle  = Option.empty[String]
    var currentGenres = Seq.empty[String]
    var currentSynopsis  = Option.empty[String]
    var currentDirector  = Seq.empty[String]
    var currentCast      = Seq.empty[String]
    var currentCountries = Seq.empty[String]
    var currentYear      = Option.empty[Int]
    var currentRuntime   = Option.empty[Int]

    def flushFilm(): Unit =
      currentTitle.filter(_.nonEmpty).foreach { title =>
        val showtimes = for { d <- pendingDates; t <- pendingTimes } yield LocalDateTime.of(d, t)
        if (showtimes.nonEmpty)
          films += RawFilm(title, showtimes.distinct.sorted, pendingPoster, currentSynopsis,
                           currentGenres, currentDirector, currentCast, currentCountries,
                           currentYear, currentRuntime)
        pendingDates     = Seq.empty
        pendingTimes     = Seq.empty
        pendingPoster    = None
        currentTitle     = None
        currentGenres    = Seq.empty
        currentSynopsis  = None
        currentDirector  = Seq.empty
        currentCast      = Seq.empty
        currentCountries = Seq.empty
        currentYear      = None
        currentRuntime   = None
      }

    /** Commit the film being read, if any — the element at hand belongs to the
     *  next one. */
    def startBlock(): Unit = if (currentTitle.nonEmpty) flushFilm()

    children.foreach { el =>
      posterOf(el) match {
        case Some(src) =>
          startBlock()
          pendingPoster = Some(src)

        case None => el.tagName match {
          case "hr" =>
            flushFilm()

          case "h1" | "h2" =>
            currentTitle = Some(el.text.trim).filter(_.nonEmpty)

          case "h3" =>
            // Date line — "25.06 (czwartek) - ..."; only count it if the
            // date regex matches so non-date <h3>s (e.g. "REPERTUAR") are ignored.
            DatePat.findFirstMatchIn(el.text).foreach { m =>
              startBlock()
              val day   = m.group(1).toInt
              val month = m.group(2).toInt
              val year  = if (month < today.getMonthValue) today.getYear + 1 else today.getYear
              Try(LocalDate.of(year, month, day)).toOption.foreach { d =>
                pendingDates = pendingDates :+ d
              }
            }
            // Reset times when a new date header appears — each date block
            // carries its own "godziny seansów" in the following <p>.
            pendingTimes = Seq.empty

          case "p" =>
            val text  = el.text
            val lower = text.toLowerCase
            // "godziny seansów:" line — extract all HH:MM tokens from the <strong>
            if (lower.contains("godziny") || lower.contains("seansów") || lower.contains("seansow")) {
              val strong = Option(el.selectFirst("strong")).map(_.text).getOrElse(text)
              pendingTimes = TimePat.findAllMatchIn(strong)
                .flatMap(m => Try(LocalTime.of(m.group(1).toInt, m.group(2).toInt)).toOption)
                .toSeq
            }
            // Metadata block — gatunek, reżyseria, obsada, produkcja, czas
            // trwania, each on a <br/>-separated line of ONE paragraph.
            else if (MetaLabels.exists(lower.contains)) {
              // wholeOwnText has no <br> → use the original HTML, split on <br>
              // and parse each fragment independently using Jsoup to extract the
              // value from the fragment's <strong>. Every line is
              // `label:&nbsp;<strong>value</strong>`, so a comma-split of the bold
              // text yields a list. `&nbsp;` decodes to a non-breaking space,
              // normalised back to a plain space before trim.
              val fragments = el.html.split("(?i)<br\\s*/?>").toSeq
              def bold(frag: String): String =
                Option(Jsoup.parseBodyFragment(frag).body.selectFirst("strong"))
                  .map(_.text).getOrElse("").replaceAll("\\u00A0", " ").trim
              def boldList(frag: String): Seq[String] =
                bold(frag).split(",").map(_.trim).filter(_.nonEmpty).toSeq
              fragments.foreach { frag =>
                val fragLower = frag.toLowerCase
                if (fragLower.contains("gatunek") && currentGenres.isEmpty)
                  currentGenres = boldList(frag)
                else if ((fragLower.contains("reżyseria") || fragLower.contains("rezyseria")) && currentDirector.isEmpty)
                  currentDirector = boldList(frag)
                else if (fragLower.contains("obsada") && currentCast.isEmpty)
                  currentCast = boldList(frag)
                // "produkcja: Izrael, Włochy 2024" — countries then the release
                // year, the strongest TMDB-identity hint this page carries.
                else if (fragLower.contains("produkcja") && currentCountries.isEmpty && currentYear.isEmpty) {
                  val value = bold(frag)
                  currentYear = YearPat.findFirstMatchIn(value).flatMap(m => Try(m.group(1).toInt).toOption)
                  currentCountries = YearPat.replaceAllIn(value, "")
                    .split(",").map(_.trim).filter(_.nonEmpty).toSeq
                }
                else if (fragLower.contains("czas trwania") && currentRuntime.isEmpty)
                  currentRuntime = RuntimePat.findFirstMatchIn(bold(frag))
                    .flatMap(m => Try(m.group(1).toInt).toOption)
              }
            }
            // First synopsis paragraph (em-wrapped prose)
            else if (currentSynopsis.isEmpty && el.select("em").size > 0) {
              val em = el.selectFirst("em")
              val prose = Option(em).map(_.text.trim).filter(_.length > 20)
              currentSynopsis = prose
            }

          case _ =>
        }
      }
    }
    flushFilm()
    films.toSeq
  }

  /** Labels of the one metadata paragraph — any of them means the whole
   *  `<br>`-separated block is worth splitting apart. */
  private val MetaLabels =
    Seq("gatunek", "reżyseria", "rezyseria", "obsada", "produkcja", "czas trwania")
}
