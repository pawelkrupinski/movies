package services.cinemas

import models._
import org.jsoup.Jsoup

import java.time.{LocalDateTime, YearMonth}
import scala.jdk.CollectionConverters._
import scala.util.Try

/**
 * Shared parsing logic for the MSI ticketing platform used by several Polish
 * art-house / repertoire cinemas (Cinema1 Gdańsk, Zamek Szczecin, and others).
 *
 * The month URL `<base>/MSI/mvc/pl?sort=Name&date=YYYY-MM` (Cinema1, Zamek, …)
 * renders every screening in the given calendar month on a single server-side
 * HTML page.  No JavaScript
 * execution required; all dates and times are in the document.
 *
 * Page structure per film:
 *   - `div.movies-movie__single` — one block per film variant (one film may
 *     appear in multiple variants if the same title screens in several formats
 *     or under slightly different version titles).
 *   - `.movies-movie__single__title[title]` — raw title attribute (may
 *     include a format suffix like `(2D NAPISY)` or an original-language
 *     subtitle in parens). The heading tag varies by portal theme (`h2` on
 *     most, `h3` on RCK Kołobrzeg), so it's matched by class, not tag.
 *   - `div.movies-movie__single__options.d-none ul.movies-movie__single__options__hours a[href]`
 *     — each anchor's text is `DD mmm HH:MM` (abbreviated Polish months),
 *     and the href is the booking link.
 *
 * The desktop div (class includes `d-none d-md-block`) and the mobile div
 * (class includes `d-block d-lg-flex`) carry the same events.  We read only
 * the desktop div so each event is counted exactly once.
 *
 * Showtimes for the same cleaned title across all fetched months are merged
 * before returning.
 */
private[cinemas] object MsiScraper {

  /** Abbreviated Polish month names as used by the MSI platform in event-time
   *  anchors ("07 cze 17:00", "14 lis 20:00"). These are abbreviations, distinct
   *  from the full genitive forms (`ScraperParse.PolishMonths`) used by other
   *  scrapers. */
  val PolishMonthsAbbrev: Map[String, Int] = Map(
    "sty" -> 1, "lut" -> 2, "mar" -> 3, "kwi" -> 4, "maj" -> 5, "cze" -> 6,
    "lip" -> 7, "sie" -> 8, "wrz" -> 9, "paź" -> 10, "lis" -> 11, "gru" -> 12
  )

  private val EventTimePat = """(\d{1,2})\s+(\w+)\s+(\d{2}):(\d{2})""".r

  // The MSI page also embeds full film metadata in a `var RepertoireEvents = [
  // {…} ]` JS array (one flat object per screening) that Jsoup doesn't surface.
  // Each object carries a `Name` (identical to the rendered block's `title`
  // attribute) and a free-text `Description`. There is no dedicated year /
  // original-title field — that metadata, like the director, lives inside the
  // Description prose. We mine all of it out of there and join it back to the
  // rendered blocks by name.
  private val JsObjectPat = """(?s)\{[^{}]*\}""".r
  private val JsNamePat   = """'Name'\s*:\s*'((?:[^'\\]|\\.)*)'""".r
  private val JsDescPat   = """'Description'\s*:\s*'((?:[^'\\]|\\.)*)'""".r
  // Director line of a Description, anchored at the start of a `<br>`-delimited
  // segment so a mid-sentence mention can't match. Accepts the label variants
  // the portals actually emit: `REŻYSERIA:`, `REŻYSERIA ` (no colon) and the
  // `Reżysera` genitive typo. The Polish genitive/instrumental "reżyserii"/
  // "reżyserią" end past `(?:ia|a)` so prose like "w reżyserii X" won't match.
  private val DirectorLinePat = """(?iu)(?:^|<br>)\s*re[żz]yser(?:ia|a)\s*:?\s*([^<]+)""".r

  // The "production line" parenthetical the art-house portals (Zamek/…) emit in
  // a Description: `( [OriginalTitle, ] Country[/Country…] YEAR [, runtime'] )`.
  //   foreign:    `(Casablanca, USA 1942, 102’)`   → orig "Casablanca", year 1942
  //   polish:     `(Polska 1976, 153’)`            → no orig title,     year 1976
  //   multi-país: `(Kanada/USA/Wielka Brytania 2001, 117’)` → no orig,  year 2001
  // The year is required to be preceded by a country WORD (letters) so a bare
  // biographical paren like `(1869-1939)` or a lone `(2017)` can't false-match.
  // Group 1 = the text before the country/year group (possibly empty or a comma-
  // terminated original-title prefix); group 2 = the 4-digit year.
  private val ProductionLinePat =
    """(?u)\(([^()]*?)[\p{L}/. ]+\s((?:19|20)\d{2})(?=\s*[,)])""".r

  private[cinemas] case class FilmMeta(director: Seq[String] = Seq.empty,
                                       releaseYear: Option[Int] = None,
                                       originalTitle: Option[String] = None) {
    def nonEmpty: Boolean = director.nonEmpty || releaseYear.isDefined || originalTitle.isDefined
  }

  private[cinemas] case class RawSlot(title: String, rawTitle: String, dateTime: LocalDateTime,
                                      booking: Option[String], format: List[String],
                                      meta: FilmMeta = FilmMeta())

  /** Map every `RepertoireEvents` entry's name → the metadata mined from its
   *  `Description` (director, release year, original title). Entries whose
   *  Description yields nothing are omitted. The name is matched against the
   *  rendered block's (Jsoup-decoded) title attr. */
  private[cinemas] def metaByName(html: String): Map[String, FilmMeta] =
    JsObjectPat.findAllMatchIn(html).flatMap { obj =>
      val block = obj.matched
      for {
        name <- JsNamePat.findFirstMatchIn(block).map(m => unescapeJs(m.group(1)))
        desc <- JsDescPat.findFirstMatchIn(block).map(m => unescapeJs(m.group(1)))
        meta = parseDescriptionMeta(desc) if meta.nonEmpty
      } yield name -> meta
    }.toMap

  /** All structured metadata mined from a Description: director, release year,
   *  and original title (when the production line carries one). */
  private[cinemas] def parseDescriptionMeta(description: String): FilmMeta = {
    val (year, originalTitle) = parseDescriptionProduction(description)
    FilmMeta(parseDescriptionDirector(description), year, originalTitle)
  }

  /** Release year + original title from a Description's production-line
   *  parenthetical. The year is the 4-digit number that follows the country
   *  word(s); the original title is the comma-terminated prefix before them,
   *  present only for foreign films (Polish films open the paren with the
   *  country, so there's no leading title). `(None, None)` when there's no
   *  production line (kids-film synopses and other Descriptions carry none). */
  private[cinemas] def parseDescriptionProduction(description: String): (Option[Int], Option[String]) =
    ProductionLinePat.findFirstMatchIn(description).map { m =>
      val year = Try(m.group(2).toInt).toOption
      // The original title is everything up to (and including) the last comma in
      // the pre-year prefix — `Casablanca, ` → "Casablanca". No comma means the
      // paren opened on the country (Polish / multi-country film) → no title.
      val prefix = m.group(1)
      val originalTitle = prefix.lastIndexOf(',') match {
        case -1 => None
        case i  => Some(prefix.substring(0, i).trim).filter(_.nonEmpty)
      }
      (year, originalTitle)
    }.getOrElse((None, None))

  /** Director names from a Description's `REŻYSERIA …` line — comma-split, with
   *  trailing sentence punctuation dropped. Empty when there's no director line
   *  (kids-film synopses and other Descriptions carry none). */
  private[cinemas] def parseDescriptionDirector(description: String): Seq[String] =
    DirectorLinePat.findFirstMatchIn(description).map(_.group(1)).toSeq
      .flatMap(_.split(","))
      .map(_.trim.stripSuffix(".").trim)
      .filter(_.nonEmpty)

  /** Undo the JS single-quoted-string escapes that matter for our fields. */
  private def unescapeJs(s: String): String =
    s.replace("\\'", "'").replace("\\\"", "\"").replace("\\/", "/").replace("\\\\", "\\")

  /**
   * Parse one MSI month page with full year context.  Returns one `RawSlot`
   * per unique event anchor found in the desktop showtime list.
   *
   * @parameter html       Raw HTML of the month page.
   * @parameter yearMonth  The calendar month this page was fetched for — used to
   *                   supply the year component missing from event-anchor text.
   * @parameter baseUrl    Base URL of the MSI host (used only with `attr("abs:href")`
   *                   expansion by Jsoup to produce absolute booking URLs).
   * @parameter cleanTitle Per-cinema title normalisation: returns the cleaned title
   *                   plus the screening-format display tokens (2D/NAP/DUB/…)
   *                   the MSI title buried in its text.
   */
  def parseMonthWithYear(html: String, yearMonth: YearMonth, baseUrl: String,
                         cleanTitle: String => (String, List[String])): Seq[RawSlot] = {
    val document = Jsoup.parse(html, baseUrl)
    val metaByName0 = metaByName(html)
    document.select("div.movies-movie__single").asScala.toSeq.flatMap { movieDiv =>
      val rawTitle = Option(movieDiv.selectFirst(".movies-movie__single__title"))
        .map(_.attr("title").trim)
        .filter(_.nonEmpty)
        .getOrElse("")
      val (title, format) = cleanTitle(rawTitle)
      val meta = metaByName0.getOrElse(rawTitle, FilmMeta())
      if (title.isEmpty) Seq.empty
      else {
        // Only the desktop showtime list — the mobile duplicate has no extra `d-none`
        // class, so the selector `div.movies-movie__single__options.d-none` matches
        // only the desktop div, giving each event exactly once.
        val desktopList = movieDiv.select(
          "div.movies-movie__single__options.d-none ul.movies-movie__single__options__hours"
        )
        desktopList.asScala.toSeq.flatMap { ul =>
          ul.select("a[href]").asScala.toSeq.flatMap { a =>
            val text    = a.text.trim
            val href    = a.attr("abs:href")
            val booking = if (href.nonEmpty) Some(href) else None
            parseEventTime(text, yearMonth).map { dt => RawSlot(title, rawTitle, dt, booking, format, meta) }
          }
        }
      }
    }
  }

  private def parseEventTime(text: String, ym: YearMonth): Option[LocalDateTime] =
    EventTimePat.findFirstMatchIn(text).flatMap { m =>
      val day   = m.group(1).toInt
      val month = PolishMonthsAbbrev.get(m.group(2).toLowerCase)
      val hour  = m.group(3).toInt
      val min   = m.group(4).toInt
      month.flatMap { mo =>
        // The event text supplies day + month but not the year.  Use the year
        // from the YearMonth the page was fetched for.
        Try(LocalDateTime.of(ym.getYear, mo, day, hour, min)).toOption
      }
    }

  /**
   * Group raw slots by title into [[CinemaMovie]] entries, deduplicating
   * showtimes by `(dateTime, bookingUrl)`.
   */
  def toMovies(slots: Seq[RawSlot], cinema: Cinema): Seq[CinemaMovie] =
    SlotsToMovies.fold(slots, _.title, s => Showtime(s.dateTime, s.booking, format = s.format)) {
      (title, group, showtimes) =>
        val metas = group.map(_.meta)
        CinemaMovie(
          movie     = Movie(
            title,
            releaseYear   = metas.flatMap(_.releaseYear).headOption,
            originalTitle = metas.flatMap(_.originalTitle).headOption,
            rawTitle      = group.map(_.rawTitle).headOption
          ),
          cinema    = cinema,
          posterUrl = None,
          filmUrl   = None,
          synopsis  = None,
          cast      = Seq.empty,
          director  = metas.map(_.director).find(_.nonEmpty).getOrElse(Seq.empty),
          showtimes = showtimes
        )
    }
}
