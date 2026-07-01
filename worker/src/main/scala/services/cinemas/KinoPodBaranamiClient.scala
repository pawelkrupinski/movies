package services.cinemas

import models._
import org.jsoup.Jsoup
import org.jsoup.nodes.{Element, TextNode}
import tools.HttpFetch

import java.time.{LocalDate, LocalDateTime, ZoneId}
import java.util.Locale
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Kino Pod Baranami (Kraków) — historic arthouse cinema on Rynek Główny.
 * The cinema publishes a self-hosted PHP repertoire at `/repertuar.php`
 * (ISO-8859-2 encoded), listing the full week's schedule on one page.
 *
 * Structure of the page:
 *   - `p.rep_date` — a day header: "Niedziela 7 czerwca // Sunday, June 7".
 *     Contains the day-of-month and a Polish genitive month name; the year is
 *     absent and must be inferred from the current date.
 *   - `ul.program_list > li` — one film entry per `<li>`. Each entry holds:
 *       - `a[href^="film.php"]` — the film title (anchor text) and cinema-relative
 *         film URL.
 *       - `span > a[href^="/rezerwacja_start.php?event_id="]` — one or more
 *         screening time links, with the time as the anchor text (`HH:MM`) and
 *         the booking URL as the href. The `onclick` on each link encodes the
 *         date and time as positional arguments but we read the time directly
 *         from the anchor text.
 *
 * A single `<li>` can carry several screening times across the same day,
 * each as a separate `<a>` inside the `<span>`. The parser emits one
 * `RawSlot` per time × date.
 */
class KinoPodBaranamiClient(
  http:             HttpFetch,
  override val cinema: Cinema,
  today:            LocalDate = LocalDate.now(ZoneId.of("Europe/Warsaw"))
) extends CinemaScraper with DetailEnricher {

  import KinoPodBaranamiClient._

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(BaseUrl)
  override def sourceUrl: Option[String] = Some(BaseUrl)

  def fetch(): Seq[CinemaMovie] = {
    val html  = http.getBytes(RepertoireUrl)
    val str   = new String(html, "ISO-8859-2")
    val slots = parseDocument(str, today)

    // Group by the LANGUAGE-STRIPPED title, not by `film.php?film_id=` URL: the
    // cinema lists a film's subtitled and dubbed editions as two separate
    // `film_id` entries differing only by a trailing "(Napisy PL)"/"(Dubbing PL)"
    // tag (already stripped into `RawSlot.format` at parse time). Keyed by URL
    // they'd be two `CinemaMovie`s that sanitize to the same slot and clobber
    // each other every scrape; keyed by title they fold into one film with the
    // UNION of screenings, each screening keeping its own language on
    // `Showtime.format` (NAP / DUB). A deterministic representative `filmUrl` is
    // picked so the merged movie is stable run-to-run.
    val byTitle = slots.groupBy(_.title)
    byTitle.toSeq.flatMap { case (title, group) =>
      val showtimes = group
        .map(s => Showtime(s.dateTime, s.bookingUrl, format = s.format))
        .distinctBy(s => (s.dateTime, s.bookingUrl))
        .sortBy(_.dateTime)
      if (showtimes.isEmpty) None
      else Some(CinemaMovie(
        movie     = Movie(title, originalTitle = group.flatMap(_.originalTitle).headOption),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = group.flatMap(_.filmUrl).distinct.sorted.headOption,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      ))
    }.sortBy(_.movie.title)
  }

  // The listing already carries an originalTitle hint (the anchor `title=` attr)
  // and the language tag per screening, so the film resolves immediately from the
  // listing; the detail page only adds display enrichment (synopsis / cast /
  // director / runtime / poster), merged in asynchronously by the EnrichDetails
  // task rather than fetched on the scrape thread.
  override val detailGroup: String = "kino-pod-baranami"
  override def defersTmdbResolution: Boolean = false

  /** Deferred per-film detail — the EnrichDetails task calls this with the slot's
   *  `filmUrl` (`film.php?film_id=…`). The page is ISO-8859-2 like the listing.
   *  None when nothing useful parsed, so the task stays stale and retries rather
   *  than recording an empty result as fresh. NB: the page also carries a `ul.when`
   *  screening list, but we read ONLY the static fields — showings stay sourced
   *  (live) from the listing, which keeps this fetch safely deferrable/cacheable. */
  override def fetchFilmDetail(ref: String): Option[FilmDetail] =
    Try(http.getBytes(ref)).toOption
      .map(bytes => parseDetail(new String(bytes, "ISO-8859-2")))
      .filter(d => d.synopsis.nonEmpty || d.director.nonEmpty || d.cast.nonEmpty ||
                   d.runtimeMinutes.nonEmpty || d.originalTitle.nonEmpty)
}

object KinoPodBaranamiClient {

  val BaseUrl       = "https://kinopodbaranami.pl"
  val RepertoireUrl = s"$BaseUrl/repertuar.php"

  // "7 czerwca" or "10 maja" — day + Polish genitive month name
  private val DayMonthPat = """(\d{1,2})\s+(\w+)""".r

  // "(SMAK)" is Kino Pod Baranami's discussion-club programme tag ("Seans z
  // dyskusją"), appended to the film title for those screenings. It is not part
  // of the film's name: left in, "Dzień objawienia (SMAK)" sanitizes to a
  // different merge key (`_id` = sanitize(title)|year) and forks the film into a
  // separate one-cinema record. Strip only this exact label — other trailing
  // parentheticals here are genuine English original titles ("(The Promised
  // Land)") that must be preserved.
  private val ProgrammeLabelSuffix = """\s*\(SMAK\)\s*$""".r

  private[cinemas] def stripProgrammeLabel(title: String): String =
    ProgrammeLabelSuffix.replaceAllIn(title, "").trim

  private[cinemas] case class RawSlot(
    title:         String,
    originalTitle: Option[String],
    filmUrl:       Option[String],
    dateTime:      LocalDateTime,
    bookingUrl:    Option[String],
    format:        List[String]
  )

  /** Infer year: if the date falls more than 60 days in the past relative to
    * `today`, it must belong to next year (handles the December→January turn). */
  private def guessYear(day: Int, month: Int, today: LocalDate): Int = {
    val candidate = Try(LocalDate.of(today.getYear, month, day)).toOption
    candidate match {
      case Some(d) if d.isBefore(today.minusDays(60)) => today.getYear + 1
      case _ => today.getYear
    }
  }

  private[cinemas] def parseDocument(html: String, today: LocalDate): Seq[RawSlot] = {
    val document = Jsoup.parse(html)
    val slots = collection.mutable.ArrayBuffer.empty[RawSlot]
    var currentDate: Option[LocalDate] = None

    document.select("p.rep_date, ul.program_list").asScala.foreach { element =>
      element.tagName match {
        case "p" if element.hasClass("rep_date") =>
          // "Niedziela 7 czerwca // Sunday, June 7"
          val text = element.text.trim
          currentDate = DayMonthPat.findFirstMatchIn(text).flatMap { m =>
            val day   = m.group(1).toInt
            val month = ScraperParse.PolishMonths.get(m.group(2).toLowerCase)
            month.flatMap { mo =>
              val yr = guessYear(day, mo, today)
              Try(LocalDate.of(yr, mo, day)).toOption
            }
          }

        case "ul" if element.hasClass("program_list") =>
          currentDate.foreach { date =>
            element.select("li").asScala.foreach { li =>
              val titleAnchor = Option(li.selectFirst("a[href^=\"film.php\"]"))
              // Peel the trailing language/format tag ("(Napisy PL)", "(Dubbing
              // PL)") off the title and surface it as `format` tokens (NAP/DUB/…),
              // so a film's subtitled and dubbed editions collapse to one title
              // and their screenings merge (each keeping its own language badge)
              // instead of clobbering one slot. `(SMAK)` is peeled first (it has no
              // format word, so `extractFormatTags` would leave it) — see
              // `stripProgrammeLabel`.
              val (title, format) = titleAnchor
                .map(a => ScraperParse.extractFormatTags(stripProgrammeLabel(a.text.trim)))
                .getOrElse(("", Nil))
              val filmUrl     = titleAnchor.map(a => s"$BaseUrl/${a.attr("href").trim}").filter(_.nonEmpty)
              // The anchor's `title=` attribute carries the film's original
              // (usually international/English) title — "Gourou" for "Guru",
              // "Fight Club" for "Podziemny krąg". The cinema exposes it for
              // every film; we keep it (when it actually differs from the
              // displayed Polish title) as a TMDB-search hint, which lets a
              // same-Polish-title film resolve to the right TMDB entry instead
              // of an arrival-order-dependent guess between same-title candidates.
              val originalTitle = titleAnchor.map(_.attr("title").trim)
                .filter(_.nonEmpty).filterNot(_.equalsIgnoreCase(title))

              if (title.nonEmpty) {
                // One or more time links inside <span>
                li.select("span a[href^=\"/rezerwacja_start.php\"]").asScala.foreach { a =>
                  val time       = ScraperParse.parseHHmm(a.text.trim)
                  val bookingUrl = Some(a.attr("href").trim).filter(_.nonEmpty)
                    .map(h => if (h.startsWith("http")) h else s"$BaseUrl$h")
                  time.foreach { t =>
                    slots += RawSlot(title, originalTitle, filmUrl, LocalDateTime.of(date, t), bookingUrl, format)
                  }
                }
              }
            }
          }

        case _ =>
      }
    }
    slots.toSeq
  }

  private val YearPat = """(?:19|20)\d{2}""".r

  /** Parse a `film.php?film_id=…` detail page into a [[FilmDetail]]. The static
   *  film facts sit in `<strong>label:</strong> value<br>` pairs inside
   *  `div.indent`; the synopsis prose is the `<p>` after the film `<table>`; the
   *  poster is the full-size image the thumbnail links to. The page's own
   *  screening list (`ul.when`) is deliberately ignored — showings come from the
   *  live listing. */
  private[cinemas] def parseDetail(html: String): FilmDetail = {
    val doc    = Jsoup.parse(html)
    val fields = Option(doc.selectFirst("div.indent")).map(labeledFields).getOrElse(Map.empty)
    def people(label: String): Seq[String] =
      fields.get(label).toSeq.flatMap(_.split(",").map(_.trim).filter(_.nonEmpty))
    val runtime = fields.get("czas trwania").flatMap(v => """(\d+)""".r.findFirstIn(v)).map(_.toInt).filter(_ > 0)
    val (countries, prodYear) = fields.get("produkcja").map(ScraperParse.productionMeta).getOrElse((Seq.empty, None))
    val premieraYear = fields.get("premiera").flatMap(v => YearPat.findFirstIn(v)).map(_.toInt)
    val synopsis = Option(doc.selectFirst("div#column_wide div.bot > p"))
      .map(p => ScraperParse.cleanSynopsis(p, "div")).filter(_.length > 20)
    val poster = Option(doc.selectFirst("td[width=130] a[href]")).map(_.attr("href").trim)
      .filter(_.nonEmpty).map(h => if (h.startsWith("http")) h else s"$BaseUrl/${h.stripPrefix("/")}")
    FilmDetail(
      synopsis       = synopsis,
      cast           = people("aktorzy"),
      director       = people("reżyseria"),
      runtimeMinutes = runtime,
      releaseYear    = prodYear.orElse(premieraYear),
      originalTitle  = fields.get("tytuł oryginalny").filter(_.nonEmpty),
      countries      = countries,
      posterUrl      = poster
    )
  }

  /** Map the `<strong>label:</strong> value<br>` metadata pairs inside `container`
   *  to `label -> value` — label lower-cased, whitespace-collapsed, trailing colon
   *  stripped; value read from the text node right after each `<strong>`. First
   *  value wins per label. */
  private def labeledFields(container: Element): Map[String, String] = {
    val out = scala.collection.mutable.LinkedHashMap.empty[String, String]
    container.select("strong").asScala.foreach { s =>
      val label = s.text.trim.toLowerCase(Locale.ROOT).replaceAll("\\s+", " ").stripSuffix(":").trim
      val value = s.nextSibling() match {
        case tn: TextNode => tn.text.replaceAll("\\s+", " ").trim
        case _            => ""
      }
      if (label.nonEmpty && value.nonEmpty && !out.contains(label)) out(label) = value
    }
    out.toMap
  }
}
