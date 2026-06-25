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
 * A few MSI installs serve an older "list-group" skin instead (e.g. Max Kino
 * Świebodzin at repertuar.maxkino.eu): each film is a
 * `div.list-group-item.event-background` (rendered `visible-md`/`visible-xs`
 * twice), the title lives in the poster anchor's `title` attr, and showtimes
 * are `a.badge-purple` anchors in `ul.repo-event-dates`. The event-time text
 * format (`DD mmm HH:MM`) is identical, so only the block/title/anchor
 * selectors differ. `parseMonthWithYear` tries the standard skin first and
 * falls back to this one when it yields nothing.
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
  // a Description: `( [OriginalTitle, ] Country[, /Country…] YEAR [, runtime'] )`.
  //   foreign:    `(Casablanca, USA 1942, 102’)`            → orig "Casablanca", year 1942
  //   polish:     `(Polska 1976, 153’)`                     → no orig,           year 1976
  //   multi-país: `(Kanada/USA/Wielka Brytania 2001, 117’)` → no orig,           year 2001
  //   two-país:   `(USA, Japonia 1989, 110 min.)`           → no orig,           year 1989
  // The year is required to be preceded by a country WORD (letters) so a bare
  // biographical paren like `(1869-1939)` or a lone `(2017)` can't false-match.
  // Group 1 = the text before the last country (an optional original-title prefix,
  // possibly with leading countries); group 2 = the 4-digit year.
  private val ProductionLinePat =
    """(?u)\(([^()]*?)[\p{L}/. ]+\s((?:19|20)\d{2})(?=\s*[,)])""".r

  // The runtime that trails the year in the same production line — `1942, 102’`
  // or `1989, 110 min.`. Anchored on the year so a stray number elsewhere in the
  // Description prose can't match.
  private val ProductionRuntimePat =
    """(?u)(?:19|20)\d{2}\s*,\s*(\d{1,3})\s*(?:['’]|min)""".r

  // Country names (and the abbreviations) that show up in MSI production lines,
  // lower-cased. Used to strip a leading country out of the original-title prefix:
  // `(USA, Japonia 1989)` has no original title — "USA" is the first of two
  // countries, not a title — whereas `(Casablanca, USA 1942)` does. Telling them
  // apart structurally is impossible (both are "X, Y YEAR"), so we test against
  // this set. Not exhaustive — an unlisted country merely risks re-surfacing the
  // leak for that one film, never a wrong YEAR.
  private val Countries: Set[String] = Set(
    "usa", "polska", "niemcy", "rfn", "nrd", "francja", "włochy", "hiszpania",
    "wielka brytania", "anglia", "kanada", "japonia", "zsrr", "rosja", "czechy",
    "czechosłowacja", "słowacja", "austria", "szwajcaria", "szwecja", "norwegia",
    "dania", "finlandia", "holandia", "niderlandy", "belgia", "irlandia",
    "australia", "nowa zelandia", "chiny", "korea", "korea południowa", "indie",
    "brazylia", "argentyna", "meksyk", "węgry", "rumunia", "bułgaria", "grecja",
    "turcja", "portugalia", "islandia", "izrael", "iran", "ukraina", "jugosławia",
    "chorwacja", "serbia", "luksemburg", "estonia", "litwa", "łotwa", "rpa",
    "egipt", "tajwan", "hongkong", "wietnam", "tajlandia", "kuba", "chile",
    "kolumbia", "gruzja", "armenia", "kazachstan", "katar")

  private def isCountry(s: String): Boolean = Countries.contains(s.trim.toLowerCase(java.util.Locale.ROOT))

  private[cinemas] case class FilmMeta(director: Seq[String] = Seq.empty,
                                       releaseYear: Option[Int] = None,
                                       originalTitle: Option[String] = None,
                                       runtimeMinutes: Option[Int] = None) {
    def nonEmpty: Boolean =
      director.nonEmpty || releaseYear.isDefined || originalTitle.isDefined || runtimeMinutes.isDefined
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
    FilmMeta(parseDescriptionDirector(description), year, originalTitle, parseDescriptionRuntime(description))
  }

  /** Runtime (minutes) from the production line's `YEAR, NNN’` / `YEAR, NNN min.`
   *  tail. None when the line carries no runtime. */
  private[cinemas] def parseDescriptionRuntime(description: String): Option[Int] =
    ProductionRuntimePat.findFirstMatchIn(description).map(_.group(1).toInt)

  /** Release year + original title from a Description's production-line
   *  parenthetical. The year is the 4-digit number that follows the country
   *  word(s); the original title is the comma-terminated prefix before them.
   *
   *  That prefix is ambiguous: `(Casablanca, USA 1942)` carries a title, but
   *  `(USA, Japonia 1989)` opens on the first of two countries — both look like
   *  "X, Y YEAR". So we strip any trailing country tokens off the prefix (the
   *  ones sitting between a real title and the year-bearing country): what's left
   *  is the original title, or nothing when the prefix was countries all the way
   *  down. `(None, None)` when there's no production line at all. */
  private[cinemas] def parseDescriptionProduction(description: String): (Option[Int], Option[String]) =
    ProductionLinePat.findFirstMatchIn(description).map { m =>
      val year = Try(m.group(2).toInt).toOption
      // The candidate title is everything up to the last comma in the pre-year
      // prefix (`Casablanca, ` → "Casablanca"; no comma → the paren opened on the
      // country, so no title).
      val prefix = m.group(1)
      val originalTitle = prefix.lastIndexOf(',') match {
        case -1 => None
        case i  =>
          // Drop trailing country tokens so "USA, Japonia" → nothing and
          // "Mystery Train, USA" → "Mystery Train", while "Casablanca" stays.
          val parts = prefix.substring(0, i).split(',').map(_.trim).filter(_.nonEmpty)
          val titleParts = parts.reverse.dropWhile(isCountry).reverse
          Some(titleParts.mkString(", ")).filter(_.nonEmpty)
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

    /** One pass over a skin's film blocks: for each block, read its raw title and
     *  desktop showtime anchors, clean the title, and emit a `RawSlot` per anchor.
     *  The two skins differ only in these three selectors + how the title is read,
     *  so they share everything below. */
    def parseBlocks(blockSelector: String, rawTitleOf: org.jsoup.nodes.Element => String,
                    showtimeAnchorSelector: String): Seq[RawSlot] =
      document.select(blockSelector).asScala.toSeq.flatMap { movieDiv =>
        val rawTitle        = rawTitleOf(movieDiv)
        val (title, format) = cleanTitle(rawTitle)
        val meta            = metaByName0.getOrElse(rawTitle, FilmMeta())
        if (title.isEmpty) Seq.empty
        else movieDiv.select(showtimeAnchorSelector).asScala.toSeq.flatMap { a =>
          val href    = a.attr("abs:href")
          val booking = if (href.nonEmpty) Some(href) else None
          parseEventTime(a.text.trim, yearMonth).map { dt =>
            RawSlot(title, rawTitle, dt, booking, format, meta)
          }
        }
      }

    // Standard MSI skin: one `div.movies-movie__single` per film variant; the raw
    // title is the `title` attr of `.movies-movie__single__title`; showtimes are in
    // the desktop-only `div.movies-movie__single__options.d-none` list (the mobile
    // duplicate lacks the extra `d-none` class, so each event is counted once).
    val standard = parseBlocks(
      "div.movies-movie__single",
      m => Option(m.selectFirst(".movies-movie__single__title")).map(_.attr("title").trim).filter(_.nonEmpty).getOrElse(""),
      "div.movies-movie__single__options.d-none ul.movies-movie__single__options__hours a[href]"
    )
    if (standard.nonEmpty) standard
    else
      // Alternate "list-group" skin (e.g. Max Kino Świebodzin at
      // repertuar.maxkino.eu): each film is a `div.list-group-item.event-background`,
      // rendered twice — `visible-md visible-lg` (desktop) + `visible-xs visible-sm`
      // (mobile). We read only the desktop copy so each event is counted once. The
      // clean raw title lives in the poster anchor's `title` attr (the `.event-title`
      // text is polluted with a `Premiera!` badge span); showtimes are the
      // `a.badge-purple` anchors in `ul.repo-event-dates`, same `DD mmm HH:MM` text.
      parseBlocks(
        "div.list-group-item.event-background.visible-md",
        m => Option(m.selectFirst("a[title]")).map(_.attr("title").trim).filter(_.nonEmpty).getOrElse(""),
        "ul.repo-event-dates a.badge-purple[href]"
      )
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
            runtimeMinutes = metas.flatMap(_.runtimeMinutes).headOption,
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
