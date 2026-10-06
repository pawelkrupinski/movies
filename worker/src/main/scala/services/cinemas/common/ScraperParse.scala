package services.cinemas.common

import java.util.Locale

import org.jsoup.nodes.{Document, Element, TextNode}
import services.movies.FormatTags

import java.time.{LocalDate, LocalDateTime, LocalTime, MonthDay, Period}
import java.time.temporal.TemporalAmount
import scala.jdk.CollectionConverters._
import scala.util.Try
import scala.util.matching.Regex

/** Parsing snippets shared by the hand-rolled cinema scrapers. Each of these
  * shapes was copy-pasted across several `*Client`s before — keeping one copy
  * here means a fix (or a new quoting/format quirk) lands in every scraper at
  * once. */
private[cinemas] object ScraperParse {

  /** A clock time as the pages print it, for a client's own pattern to compose with
    * `raw"""…${ScraperParse.ClockParts}…"""` instead of spelling the digits again
    * (`NoHandRolledClockPatternSpec`). The `Parts` forms capture the hour and the minute,
    * read back by [[clockAt]]; the `Text` forms capture nothing, for a pattern that captures
    * the whole time and reads it with [[parseHHmm]]. `Dotted` also takes the "19.30" a
    * Polish page often prints. */
  val ClockParts: String       = """(\d{1,2}):(\d{2})"""
  val ClockPartsDotted: String = """(\d{1,2})[.:](\d{2})"""
  val ClockText: String        = """\d{1,2}:\d{2}"""
  val ClockTextDotted: String  = """\d{1,2}[.:]\d{2}"""
  /** The `HH:mm` of a machine-written stamp ("2026-09-06 19:30"), always two-digit, read
    * with `LocalDateTime.parse` — where a one-digit hour is not this format at all. */
  val IsoClockText: String     = """\d{2}:\d{2}"""

  private val HourMinute = ClockParts.r
  private val CssUrl     = """url\((?:'|"|&quot;)?(.+?)(?:'|"|&quot;)?\)""".r

  /** Polish genitive month names as the cinema pages spell dates ("5 maja",
    * "12 grudnia"). Shared so the hand-rolled scrapers don't each carry their
    * own copy of the same 12-entry map. */
  val PolishMonths: Map[String, Int] = Map(
    "stycznia" -> 1, "lutego" -> 2, "marca" -> 3, "kwietnia" -> 4, "maja" -> 5, "czerwca" -> 6,
    "lipca" -> 7, "sierpnia" -> 8, "września" -> 9, "października" -> 10, "listopada" -> 11, "grudnia" -> 12
  )

  /** Polish month names in the NOMINATIVE ("Czerwiec", "Styczeń"), as several
    * calendar pages spell their day headers, vs the genitive [[PolishMonths]]
    * ("czerwca"). */
  private val PolishMonthsNominative: Map[String, Int] = Map(
    "styczeń" -> 1, "luty" -> 2, "marzec" -> 3, "kwiecień" -> 4, "maj" -> 5, "czerwiec" -> 6,
    "lipiec" -> 7, "sierpień" -> 8, "wrzesień" -> 9, "październik" -> 10, "listopad" -> 11, "grudzień" -> 12
  )

  /** Genitive and nominative month names folded into one lookup — accepts
    * either spelling (the dok.pl / Iluzjon calendars use the nominative in their
    * day headers, most other pages the genitive). Keyed lower-case; [[polishMonth]]
    * folds the token's case. */
  private val PolishMonthsAnyCase: Map[String, Int] = PolishMonths ++ PolishMonthsNominative

  /** A day header's "<day> <Polish month name>" opening, with [[DayMonthYearPat]]
    * the same shape closed by an explicit 4-digit year ("4 września 2026"). The
    * month group feeds [[polishMonth]] — via [[parseDayMonth]] / [[parseDayMonthYear]]
    * for the common shapes, or directly for a scraper with a composite pattern of
    * its own (a "5-7 września" day range, say).
    *
    * The month is matched with `\p{L}`, NOT `\w`, and that is the whole point of
    * sharing these two: Java's `\w` is ASCII-only unless the pattern is compiled
    * with UNICODE_CHARACTER_CLASS, so it stops dead at the first diacritic. Of the
    * twelve genitive month names exactly two carry one — "września" and
    * "października" — so a `\w`-spelled copy reads every date for ten months of
    * the year and none at all in September and October. Four hand-rolled scrapers
    * had drifted to that spelling and lost their whole autumn programme on
    * 1 September 2026 (see `clients.cinemas.DiacriticMonthNameSpec`); one shared
    * pattern is how the fifth copy doesn't repeat it. */
  val DayMonthPat     = """(\d{1,2})\s+(\p{L}+)""".r
  val DayMonthYearPat = """(\d{1,2})\s+(\p{L}+)\s+(\d{4})""".r

  /** Polish three-letter month abbreviations as several cinema pages spell them
    * ("10 Cze 2026", "5 paź"). Keyed lower-case; [[polishMonthAbbrev]] and
    * [[polishMonth]] fold case so a page can capitalise them ("Cze") or not
    * ("cze"). Shared so the hand-rolled scrapers (the MSI portals, Praha, Art
    * Kino Krosno, and every [[parseDayMonth]] caller) don't each carry their own
    * copy of the same 12-entry map. */
  private val PolishMonthAbbrevs: Map[String, Int] = Map(
    "sty" -> 1, "lut" -> 2, "mar" -> 3, "kwi" -> 4, "maj" -> 5, "cze" -> 6,
    "lip" -> 7, "sie" -> 8, "wrz" -> 9, "paź" -> 10, "lis" -> 11, "gru" -> 12
  )

  /** The month number for a Polish three-letter abbreviation, case-insensitively
    * ("Cze"/"cze" → 6); `None` for anything not a known abbreviation. */
  def polishMonthAbbrev(token: String): Option[Int] =
    PolishMonthAbbrevs.get(token.trim.toLowerCase(Locale.ROOT))

  /** The month number for a Polish month token in any spelling the cinema
    * pages use — genitive ("września"), nominative ("wrzesień") or the
    * three-letter abbreviation ("wrz") — case-insensitively. */
  def polishMonth(token: String): Option[Int] = {
    val key = token.trim.toLowerCase(Locale.ROOT)
    PolishMonthsAnyCase.get(key).orElse(PolishMonthAbbrevs.get(key))
  }

  /** The first `HH:mm` in `s` as a `LocalTime`, or `None` when there's no
    * match or the captured hour/minute is out of range. */
  def parseHHmm(s: String): Option[LocalTime] =
    HourMinute.findFirstMatchIn(s).flatMap(clockAt(_, 1))

  /** An ISO `yyyy-MM-dd` day at the first clock time in `hour`; `None` when either is unreadable. */
  def isoDateAtClock(day: String, hour: String): Option[LocalDateTime] =
    Try(LocalDate.parse(day)).toOption.flatMap(date => parseHHmm(hour).map(date.atTime))

  /** The time a [[ClockParts]] / [[ClockPartsDotted]] match captured, its hour in group
    * `hourGroup` and its minute in the next; `None` for an hour or minute no clock has
    * ("25:00"), never a throw that would take the page's other screenings with it. */
  def clockAt(m: Regex.Match, hourGroup: Int): Option[LocalTime] =
    Option(m.group(hourGroup)).zip(Option(m.group(hourGroup + 1))).flatMap { case (hour, minute) => clock(hour, minute) }

  /** An hour and a minute already split out of a page (an extractor's bindings, a `split(":")`);
    * `None` for one that is no number or no clock ("24", "60"). */
  def clock(hour: String, minute: String): Option[LocalTime] =
    Try(LocalTime.of(hour.trim.toInt, minute.trim.toInt)).toOption

  /** A [[ClockParts]] match whose group `markerGroup` holds an optional "am" / "p.m." marker: a
    * marked hour 1–12 is a 12-hour clock ("12:10 am" → 00:10, "7:05 pm" → 19:05); a marker on an
    * hour no 12-hour clock has ("19:30 pm", "0:15 am") is ignored rather than read twice. */
  def meridiemClockAt(m: Regex.Match, hourGroup: Int, markerGroup: Int): Option[LocalTime] =
    clockAt(m, hourGroup).map { time =>
      val marker = Option(m.group(markerGroup)).map(_.trim.toLowerCase(Locale.ROOT))
      if (time.getHour == 0 || time.getHour > 12) time
      else if (marker.exists(_.startsWith("p"))) time.withHour(time.getHour % 12 + 12)
      else if (marker.exists(_.startsWith("a"))) time.withHour(time.getHour % 12)
      else time
    }

  /** ISO `2026-09-06` or the day-first `6.09.2026` / `06-09-2026` / `06/09/2026`
    * the Polish cinema pages spell their dates in. Groups 1-3 are the ISO
    * year/month/day, groups 4-6 the day-first day/month/year. */
  private val NumericDate = """(\d{4})-(\d{2})-(\d{2})|(\d{1,2})[./-](\d{1,2})[./-](\d{4})""".r
  private val DayDotMonth = """(\d{1,2})\.(\d{1,2})""".r

  /** The first calendar date in `s` in any [[NumericDate]] spelling; `None`
    * for no match or an impossible date. Finds rather than requires the whole
    * string to be the date — like [[parseHHmm]] — so a label such as
    * "Data: 06.09.2026 18:00" reads without a per-client extraction regex, and
    * one helper replaces the `DateTimeFormatter.ofPattern` copy each scraper
    * used to carry for its own separator. */
  def parseDate(s: String): Option[LocalDate] =
    NumericDate.findFirstMatchIn(s).flatMap { m =>
      val (year, month, day) =
        if (m.group(1) != null) (m.group(1), m.group(2), m.group(3))
        else (m.group(6), m.group(5), m.group(4))
      Try(LocalDate.of(year.toInt, month.toInt, day.toInt)).toOption
    }

  /** [[parseDate]] paired with [[parseHHmm]] on the same string — "06.09.2026
    * 18:00", "2026-09-06 18:00:00", "6.09.2026, 18:00", in either order.
    * `None` when either half is missing. */
  def parseDateTime(s: String): Option[LocalDateTime] =
    for { date <- parseDate(s); time <- parseHHmm(s) } yield date.atTime(time)

  /** The first "<day> <Polish month name>" in `s` ([[DayMonthPat]], the month
    * in any spelling [[polishMonth]] knows) as a `MonthDay` — the yearless day
    * header most calendar pages carry; [[upcomingDate]] places it in a year.
    * `None` when that first "<digits> <word>" is not a date. */
  def parseDayMonth(s: String): Option[MonthDay] =
    DayMonthPat.findFirstMatchIn(s).flatMap { m =>
      polishMonth(m.group(2)).flatMap(month => Try(MonthDay.of(month, m.group(1).toInt)).toOption)
    }

  /** The numeric day header some pages use instead ("5.09") as a `MonthDay`. */
  def parseNumericDayMonth(s: String): Option[MonthDay] =
    DayDotMonth.findFirstMatchIn(s)
      .flatMap(m => Try(MonthDay.of(m.group(2).toInt, m.group(1).toInt)).toOption)

  /** The first "<day> <Polish month name> <yyyy>" in `s` ([[DayMonthYearPat]])
    * as a date; `None` without a year or for an impossible date. */
  def parseDayMonthYear(s: String): Option[LocalDate] =
    DayMonthYearPat.findFirstMatchIn(s).flatMap { m =>
      polishMonth(m.group(2))
        .flatMap(month => Try(LocalDate.of(m.group(3).toInt, month, m.group(1).toInt)).toOption)
    }

  /** The year a yearless page date belongs to: this year's, unless that lies
    * more than `grace` before `today`, in which case next year's — a
    * late-December page listing "13 stycznia" means January of the coming year,
    * while a page still showing last week's screenings means this one. Each
    * scraper picks the grace its page's staleness warrants. The one roll BACK
    * is a leftover row: in early January a "30 grudnia" still on the page is
    * last December's when that lies within [[LeftoverRow]] (and the grace) of
    * `today`, not one 11 months ahead. Nothing else is pulled back, so a date
    * up to a year out keeps its place however long the grace: the horizon is
    * never capped. The year is chosen with a 29 lutego this year lacks placed on
    * the 28th, so one listed in December lands on next year's leap day; `None`
    * when the day doesn't exist in the year chosen. */
  def upcomingDate(dayMonth: MonthDay, today: LocalDate, grace: TemporalAmount = Period.ofDays(60)): Option[LocalDate] = {
    val leftoverFrom = Seq(today.minus(grace), today.minus(LeftoverRow)).maxBy(_.toEpochDay)
    val candidate    = dayMonth.atYear(today.getYear)
    val year =
      if (candidate.isBefore(today.minus(grace))) today.getYear + 1
      else if (candidate.isAfter(today) && candidate.minusYears(1).isBefore(today) && !candidate.minusYears(1).isBefore(leftoverFrom))
        today.getYear - 1
      else today.getYear
    Option.when(dayMonth.isValidYear(year))(dayMonth.atYear(year))
  }

  /** How long a past screening can linger on a page: the only span a yearless
    * date is read as last year's in. */
  private val LeftoverRow: Period = Period.ofDays(31)

  /** [[upcomingDate]] at month granularity: this year for the current month
    * and any later one, next year for an earlier month — for a calendar that
    * only ever lists the current month onwards, where "5 sierpnia" seen in
    * September can only mean next August. */
  def upcomingMonthDate(dayMonth: MonthDay, today: LocalDate): Option[LocalDate] =
    upcomingDate(dayMonth, today, grace = Period.ofDays(today.getDayOfMonth - 1))

  /** A page's numeric day and month as a `MonthDay` for [[upcomingDate]] / [[upcomingMonthDate]],
    * `None` when no year has that day (a "31.04"). Every yearless page date goes through those
    * two — never a year rule of a client's own (`NoHandRolledYearInferenceSpec`). */
  def monthDay(day: Int, month: Int): Option[MonthDay] = Try(MonthDay.of(month, day)).toOption

  /** Every calendar day from `from` to `to` inclusive, at `time` — the
    * "screens daily HH:MM from DD.MM.YYYY to DD.MM.YYYY" shape several small
    * venues (Kino Narew, Kino Parczew) spell a multi-day run in, instead of
    * listing each date separately. `to` before `from` yields empty rather
    * than throwing, so a malformed pair drops the run instead of looping
    * backwards. */
  def dailyRange(from: LocalDate, to: LocalDate, time: LocalTime): Seq[LocalDateTime] =
    if (to.isBefore(from)) Seq.empty
    else Iterator.iterate(from)(_.plusDays(1)).takeWhile(!_.isAfter(to)).map(_.atTime(time)).toSeq

  /** The `<br>`-separated lines of `el`'s text, trimmed and with blanks
    * dropped — a small venue often packs a multi-line "Label: value" block
    * or a run-per-line schedule into ONE `<br>`-joined paragraph, and jsoup's
    * `.text()` alone fuses them with no separator. Inserts a sentinel text
    * node after each `<br>` (on a clone, so the live DOM is untouched), then
    * splits on it. */
  def linesOf(el: Element): Seq[String] = {
    val clone = el.clone()
    clone.select("br").asScala.foreach(_.after("\u0001"))
    clone.text().split('\u0001').iterator.map(_.trim).filter(_.nonEmpty).toSeq
  }

  /** The URL inside a CSS `url(...)` value, unwrapping `'`, `"` or `&quot;`
    * quoting. `None` when `s` holds no `url(...)`. */
  def cssUrl(s: String): Option[String] =
    CssUrl.findFirstMatchIn(s).map(_.group(1))

  /** Text of the `<dd>` immediately after the `<dt>` whose text contains
    * `label` (case-insensitive), searched within `dtSelector`. Trimmed; empty
    * → `None`. The Drupal-style cinema sites render film metadata as such
    * definition lists. An empty `<dt>` between the label and its value is
    * passed over: NoveKino never closes its label (`<dt>Reżyseria<dt><dd>…`),
    * which parses as a second, empty `<dt>`. */
  def ddField(document: Document, label: String, dtSelector: String = "dt"): Option[String] =
    document.select(dtSelector).asScala
      .find(_.text.toLowerCase(Locale.ROOT).contains(label))
      .flatMap(dt => Iterator.iterate(dt.nextElementSibling)(_.nextElementSibling).takeWhile(_ != null)
        .find(sibling => !(sibling.tagName == "dt" && sibling.text.trim.isEmpty)))
      .filter(_.tagName == "dd")
      .map(_.text.trim)
      .filter(_.nonEmpty)

  /** The page's `og:image`, unless it has the shape of a SITE-WIDE default rather than this film's
    * poster. Many venues set one og:image on every page — Kinoteka's `kinoteka-opengraph.png`,
    * BOK's `logo-bok_…jpg`, the bilety24 venue sites' `PAN-BILET_…svg`, Kino Muranów's
    * `kino_share.png` — and a logo taken as a film's poster is worse than none: it feeds the
    * identity's poster vote and veto. Judged by the file NAME only (a path segment such as
    * bilety24's `dealer-default/` says nothing), as whole words, so "plakat-bez-logotypow" stays. */
  def ogImage(document: Document): Option[String] =
    Option(document.selectFirst("meta[property=og:image]")).map(_.attr("content").trim).filter(_.nonEmpty)
      .filterNot(isSiteDefaultImage)

  private val SiteDefaultImageWords =
    Seq("logo", "opengraph", "favicon", "placeholder", "zaslepka", "share", "og-image", "no-photo", "nophoto", "page-thumbnail")

  private def isSiteDefaultImage(url: String): Boolean = {
    val file = url.takeWhile(c => c != '?' && c != '#').split('/').lastOption.getOrElse("").toLowerCase(Locale.ROOT)
    val words = "-" + file.replaceAll("[^a-z]+", "-") + "-"
    file.endsWith(".svg") || SiteDefaultImageWords.exists(word => words.contains(s"-$word-"))
  }

  /**
   * Convert an ALL-CAPS title to sentence case for the MSI scrapers (Cinema1,
   * Kino Zamek, Kino Kijów, …) whose portals serve ALL-CAPS titles. The casing
   * itself is the shared `tools.TextNormalization.sentenceCase` (also used by
   * the central scrape-time `TitleNormalizer.recase`); this wrapper just runs
   * the MSI-specific de-glue first.
   *
   * A sequel number glued to the next word by a missing space ("3.ALE KOSMOS"
   * on RCK Kołobrzeg) is a source typo — restore the space so the word
   * capitalises and the cleaned title converges with the cinemas that spell it
   * "3. Ale kosmos". A digit-dot-digit decimal ("2.0") is untouched.
   */
  def sentenceCase(title: String): String =
    tools.TextNormalization.sentenceCase(title.replaceAll("""(\d)\.(\p{L})""", "$1. $2"))

  // Format/version tag extraction now lives in the shared `services.movies.FormatTags`
  // (common), so the ingest choke point (`MovieCache.recordCinemaScrape`) and the
  // cinema clients share ONE implementation. These delegate — every existing call
  // site (`ScraperParse.extractFormatTags` / `.stripFormatTags` / `.formatTokensIn`
  // / `.FormatToken`) is unchanged.
  val FormatToken: Map[String, String] = FormatTags.FormatToken
  def stripFormatTags(raw: String): String = FormatTags.stripFormatTags(raw)
  def extractFormatTags(raw: String): (String, List[String]) = FormatTags.extractFormatTags(raw)
  def formatTokensIn(text: String): List[String] = FormatTags.formatTokensIn(text)

  private val RuntimeHours   = """(?i)(\d+)\s*(?:godz|h\b)""".r
  private val RuntimeMinutes = """(?i)(\d+)\s*m(?:in|\b)""".r

  /** A runtime spelled in hours and minutes — "1 godz. 53 min", "3 godz 03 min",
   *  "95 min", "ok. 85 MIN", "1h 50m" — in minutes; `None` when neither part is present. */
  def hoursMinutesRuntime(s: String): Option[Int] = {
    val hours   = RuntimeHours.findFirstMatchIn(s).map(_.group(1).toInt).getOrElse(0)
    val minutes = RuntimeMinutes.findFirstMatchIn(s).map(_.group(1).toInt).getOrElse(0)
    Some(hours * 60 + minutes).filter(_ > 0)
  }

  private val FourDigitYear = """(?:19|20)\d{2}""".r

  /** A cinema "production" line — "USA 2026", "Polska, Kanada, Hiszpania, 2026" —
   *  split into (production countries, release year). The year is the 4-digit
   *  part; the remaining comma/slash-separated parts are the countries (verbatim,
   *  canonicalised later in `recordCinemaScrape`). */
  def productionMeta(s: String): (List[String], Option[Int]) = {
    val year = FourDigitYear.findFirstIn(s).map(_.toInt)
    val countries = year.foldLeft(s)((acc, y) => acc.replace(y.toString, ""))
      .split("[,/]").iterator.map(_.trim).filter(_.nonEmpty).toList
    (countries, year)
  }

  /** The first four-digit year in `s` — a premiere date's ("24 marca 1980"), a credits line's ("…, USA, 2018"). */
  def yearIn(s: String): Option[Int] = FourDigitYear.findFirstIn(s).map(_.toInt)

  /** A venue's `<strong>label:</strong> value<br>` film facts, as `label -> value`: the label lower-cased,
   *  whitespace-collapsed, its colon stripped; the value the text node right after the `<strong>`. The first
   *  value wins per label; a label with no text after it is left out. */
  def strongLabeledFields(container: Element): Map[String, String] = {
    val out = scala.collection.mutable.LinkedHashMap.empty[String, String]
    container.select("strong").asScala.foreach { strong =>
      val label = strong.text.trim.toLowerCase(Locale.ROOT).replaceAll("\\s+", " ").stripSuffix(":").trim
      val value = strong.nextSibling() match {
        case text: TextNode => text.text.replaceAll("\\s+", " ").trim
        case _              => ""
      }
      if (label.nonEmpty && value.nonEmpty && !out.contains(label)) out(label) = value
    }
    out.toMap
  }

  /** The facts a page published from the Kino za Rogiem network's shared film catalogue (kinozarogiem.pl —
   *  GOK Siedlec's and Chorzów's Kino Grajfka's both are) links as that catalogue's WordPress terms, within
   *  `root`: `re_yseria` (and its later twin `re_zyseria`, which some directors are filed under) → the
   *  directors, `produkcja` → the production countries (less the catalogue's "Koprodukcja", which names
   *  none), `rok_produkcji_filmu` → the production year. */
  def kinoZaRogiemTerms(root: Element): FilmDetail = {
    def terms(taxonomies: String*) =
      root.select(taxonomies.map(taxonomy => s"a[href*=/$taxonomy/]").mkString(", ")).asScala.toSeq
        .map(_.text.trim).filter(_.nonEmpty).distinct
    FilmDetail(
      director    = terms("re_yseria", "re_zyseria"),
      countries   = terms("produkcja").filterNot(_.equalsIgnoreCase("Koprodukcja")),
      releaseYear = terms("rok_produkcji_filmu").flatMap(yearIn).headOption)
  }

  private val FilmwebSlugYear ="""filmweb\.pl/film/.+-((?:19|20)\d{2})-\d+/?$""".r

  /** The production year a Filmweb film link's slug carries —
   *  `filmweb.pl/film/Mistyczka-2026-10125135` → 2026. Venues that link a title
   *  to its Filmweb page publish no year of their own, so the slug is their
   *  only year signal; `None` for any other link. */
  def filmwebSlugYear(url: String): Option[Int] =
    FilmwebSlugYear.findFirstMatchIn(url).map(_.group(1).toInt)

  /** Canonical `https://www.youtube.com/watch?v=<id>` form for a YouTube
    * embed / watch / `youtu.be` URL; Vimeo URLs pass through unchanged for the
    * view layer's `TrailerEmbed` to reshape, and anything else is dropped. Each
    * scraper grabs a trailer URL off its own page shape, then funnels it
    * through this single canonicaliser. */
  def canonicalTrailer(url: String): Option[String] =
    services.movies.TrailerEmbed.youTubeId(url).map(id => s"https://www.youtube.com/watch?v=$id")
      .orElse(services.movies.TrailerEmbed.vimeoId(url).map(_ => url))

  private val BareUrl = """(?i)\b(?:https?://|www\.)\S+""".r

  /** Drop bare URL tokens that leaked into prose (a plain-text link, an
   *  Instagram/Facebook handle, a "Więcej: www…" footer) and collapse the
   *  whitespace they leave behind. Anchored URLs are better removed at the
   *  DOM level (see [[cleanSynopsis]]); this catches the plain-text ones a
   *  `.text` extraction flattens in. */
  def stripUrls(text: String): String =
    BareUrl.replaceAllIn(text, "").replaceAll("[ \\t]{2,}", " ").replaceAll(" +([.,;:])", "$1").trim

  // Block- and inline-boundary sentinels: private-use code points that never
  // occur in cinema prose AND that jsoup's `.text` (which collapses only
  // *whitespace*) leaves untouched. We mark `<p>`/`<li>`/`<br>` boundaries and
  // `<b>`/`<i>` emphasis with them before flattening, then restore them as
  // newlines / markdown markers — `.text` alone fuses every paragraph into one
  // wall of text and drops all emphasis. U+E000 = paragraph break,
  // U+E001 = line, U+E002 = bold edge (→ `**`), U+E003 = italic edge (→ `*`).
  private val ParaMark = "\uE000"
  private val LineMark = "\uE001"
  private val BoldMark = "\uE002"
  private val ItalMark = "\uE003"

  /** Plain text of an element with its block structure preserved as newlines
   *  and its inline emphasis preserved as lightweight markdown: `<p>`/`<li>`
   *  separated by a blank line, `<br>` as a single line break, `<b>`/`<strong>`
   *  as `**bold**`, `<i>`/`<em>` as `*italic*`. jsoup's `.text` flattens all of
   *  that; the web detail page, the iOS `AttributedString(markdown:)` view and
   *  the Android markdown→AnnotatedString view render `\n`/`\n\n` + the bold/
   *  italic markers, so preserving them here restores formatting end-to-end.
   *  Operates on a clone, so the live DOM is left intact. */
  def blockText(el: Element): String = {
    val clone = el.clone()
    clone.select("br").asScala.foreach(_.after(LineMark))
    clone.select("p, li").asScala.foreach(_.append(ParaMark))
    // Wrap non-empty INLINE emphasis runs with a marker on both edges. Empty
    // tags (`<b></b>`) are skipped so they can't emit a bare `****`; emphasis
    // that wraps a block (`<b><p>…</p><p>…</p></b>`) is skipped too — markdown
    // can't bold across a paragraph break, so it would only produce a broken
    // `**\n\n**`; the prose still renders, just without the (unrepresentable)
    // emphasis.
    def inlineEmphasis(sel: String) =
      clone.select(sel).asScala.filter { e =>
        e.text.trim.nonEmpty &&
          e.select("p, li").isEmpty &&                          // not block-spanning
          !e.select("b, strong, i, em").asScala.exists(_ ne e)  // no NESTED emphasis (avoids broken ***…* **)
      }
    inlineEmphasis("b, strong").foreach { e => e.before(BoldMark); e.after(BoldMark) }
    inlineEmphasis("i, em").foreach { e => e.before(ItalMark); e.after(ItalMark) }
    // Produce best-effort inline markdown; the read boundary
    // (`MovieRecord.synopsis` -> `SynopsisMarkdown.sanitize`) is the single
    // place that GUARANTEES well-formed markdown across every source, so
    // blockText doesn't re-validate here.
    val tidied = tidyMarker(tidyMarker(clone.text, BoldMark), ItalMark)
    tidied
      .replace(ParaMark, "\n\n")
      .replace(LineMark, "\n")
      .replace(BoldMark, "**")
      .replace(ItalMark, "*")
  }


  // A char is "whitespace-like" for emphasis tidying if it's real whitespace or
  // one of our block-break sentinels — emphasis must not straddle either.
  private def isWsLike(c: Char): Boolean =
    c.isWhitespace || c == ParaMark.head || c == LineMark.head

  /** Markers of one type are balanced toggle pairs (one before + one after each
   *  element), so split on the marker and treat the odd segments as emphasised.
   *  Move any whitespace / block-break sentinel touching a marker OUTSIDE the
   *  pair — CommonMark won't emphasise `** x **`, so iOS would show the literal
   *  markers — and drop a pair whose content is blank (`<b> </b>`). */
  private def tidyMarker(s: String, mark: String): String = {
    val parts = s.split(java.util.regex.Pattern.quote(mark), -1)
    // Even parts ⇒ ODD markers ⇒ unbalanced (malformed source HTML — unclosed or
    // adoption-agency-split tags). Drop every marker of this type rather than
    // ship a broken half-pair; the prose still renders, just unemphasised.
    if (parts.length % 2 == 0) return s.replace(mark, "")
    val sb = new StringBuilder
    parts.iterator.zipWithIndex.foreach { case (part, i) =>
      if (i % 2 == 0) sb.append(part)
      else {
        val lead = part.takeWhile(isWsLike)
        if (lead.length == part.length) sb.append(part)  // all whitespace-like → drop markers, keep spacing
        else {
          val trail = part.reverse.takeWhile(isWsLike).reverse
          val core  = part.substring(lead.length, part.length - trail.length)
          sb.append(lead).append(mark).append(core).append(mark).append(trail)
        }
      }
    }
    sb.toString
  }

  /** Extract clean synopsis prose from a container element that also wraps
   *  junk sub-trees — booking CTAs, showtime tables, event agendas, "read
   *  more" links, trailer anchors. Pass the CSS selectors of those sub-trees
   *  to drop them; any residual bare URL surviving as plain text is stripped
   *  too. Paragraph / line structure is preserved (see [[blockText]]). The
   *  container is cloned, so the live DOM is left intact for other fields
   *  parsed from the same page. */
  def cleanSynopsis(container: Element, dropSelectors: String*): String = {
    val el = container.clone()
    dropSelectors.foreach(sel => el.select(sel).remove())
    normalizeBlocks(stripUrls(blockText(el)))
  }
  /** [[cleanSynopsis]] minus the paragraphs `drop` picks — a venue's boilerplate
   *  lines that no selector names. The container is cloned, as there. */
  def cleanSynopsisWithout(container: Element)(drop: Element => Boolean): String = {
    val kept = container.clone()
    kept.select("p").asScala.filter(drop).foreach(_.remove())
    cleanSynopsis(kept)
  }

  /** A paragraph set entirely in `<strong>` — a header line or a box-office notice,
   *  never the film's prose. */
  def isAllBold(paragraph: Element): Boolean = paragraph.select("strong").text.trim == paragraph.text.trim


  /** Tidy block-text after URL stripping: drop spaces hugging a newline and
   *  cap blank-line runs at one, so an empty (URL-only) paragraph collapses
   *  instead of leaving a gap. */
  private def normalizeBlocks(s: String): String =
    s.replaceAll("[ \\t]*\n[ \\t]*", "\n")
      .replaceAll("\n{3,}", "\n\n")
      .trim
}
