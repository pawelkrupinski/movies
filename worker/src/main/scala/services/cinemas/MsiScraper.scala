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
  // attribute) and a `Description` whose first line is the director. We mine the
  // director out of there and join it back to the rendered blocks by name.
  private val JsObjectPat = """(?s)\{[^{}]*\}""".r
  private val JsNamePat   = """'Name'\s*:\s*'((?:[^'\\]|\\.)*)'""".r
  private val JsDescPat   = """'Description'\s*:\s*'((?:[^'\\]|\\.)*)'""".r
  // Director line of a Description, anchored at the start of a `<br>`-delimited
  // segment so a mid-sentence mention can't match. Accepts the label variants
  // the portals actually emit: `REŻYSERIA:`, `REŻYSERIA ` (no colon) and the
  // `Reżysera` genitive typo. The Polish genitive/instrumental "reżyserii"/
  // "reżyserią" end past `(?:ia|a)` so prose like "w reżyserii X" won't match.
  private val DirectorLinePat = """(?iu)(?:^|<br>)\s*re[żz]yser(?:ia|a)\s*:?\s*([^<]+)""".r

  private[cinemas] case class RawSlot(title: String, rawTitle: String, dateTime: LocalDateTime,
                                      booking: Option[String], format: List[String],
                                      director: Seq[String] = Seq.empty)

  /** Map every `RepertoireEvents` entry's name → its director list, mined from
   *  the entry's `Description`. Names with no director line are omitted. The
   *  name is matched against the rendered block's (Jsoup-decoded) title attr. */
  private[cinemas] def directorsByName(html: String): Map[String, Seq[String]] =
    JsObjectPat.findAllMatchIn(html).flatMap { obj =>
      val block = obj.matched
      for {
        name <- JsNamePat.findFirstMatchIn(block).map(m => unescapeJs(m.group(1)))
        desc <- JsDescPat.findFirstMatchIn(block).map(m => unescapeJs(m.group(1)))
        dirs = parseDescriptionDirector(desc) if dirs.nonEmpty
      } yield name -> dirs
    }.toMap

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
    val directors = directorsByName(html)
    document.select("div.movies-movie__single").asScala.toSeq.flatMap { movieDiv =>
      val rawTitle = Option(movieDiv.selectFirst(".movies-movie__single__title"))
        .map(_.attr("title").trim)
        .filter(_.nonEmpty)
        .getOrElse("")
      val (title, format) = cleanTitle(rawTitle)
      val director = directors.getOrElse(rawTitle, Seq.empty)
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
            parseEventTime(text, yearMonth).map { dt => RawSlot(title, rawTitle, dt, booking, format, director) }
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
    slots
      .groupBy(_.title)
      .toSeq
      .flatMap { case (title, group) =>
        val showtimes = group
          .map(s => Showtime(s.dateTime, s.booking, format = s.format))
          .distinctBy(s => (s.dateTime, s.bookingUrl))
          .sortBy(_.dateTime)
        if (showtimes.isEmpty) None
        else Some(CinemaMovie(
          movie     = Movie(title, rawTitle = group.map(_.rawTitle).headOption),
          cinema    = cinema,
          posterUrl = None,
          filmUrl   = None,
          synopsis  = None,
          cast      = Seq.empty,
          director  = group.map(_.director).find(_.nonEmpty).getOrElse(Seq.empty),
          showtimes = showtimes
        ))
      }
      .sortBy(_.movie.title)
}
