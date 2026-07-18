package services.cinemas.pl

import org.jsoup.nodes.{Document, Element}
import services.movies.FormatTags

import java.time.LocalTime
import scala.jdk.CollectionConverters._
import scala.util.Try

/** Parsing snippets shared by the hand-rolled cinema scrapers. Each of these
  * shapes was copy-pasted across several `*Client`s before — keeping one copy
  * here means a fix (or a new quoting/format quirk) lands in every scraper at
  * once. */
private[cinemas] object ScraperParse {
  private val HourMinute = """(\d{1,2}):(\d{2})""".r
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
  val PolishMonthsNominative: Map[String, Int] = Map(
    "styczeń" -> 1, "luty" -> 2, "marzec" -> 3, "kwiecień" -> 4, "maj" -> 5, "czerwiec" -> 6,
    "lipiec" -> 7, "sierpień" -> 8, "wrzesień" -> 9, "październik" -> 10, "listopad" -> 11, "grudzień" -> 12
  )

  /** Genitive and nominative month names folded into one lookup — accepts
    * either spelling (the dok.pl / Iluzjon calendars use the nominative in their
    * day headers, most other pages the genitive). Keyed lower-case; callers
    * lower-case the token before lookup. */
  val PolishMonthsAnyCase: Map[String, Int] = PolishMonths ++ PolishMonthsNominative

  /** Polish three-letter month abbreviations as several cinema pages spell them
    * ("10 Cze 2026", "5 paź"). Keyed lower-case; [[polishMonthAbbrev]] folds case
    * so a page can capitalise them ("Cze") or not ("cze"). Shared so the
    * hand-rolled scrapers (Kino Żak, the MSI portals, Kijów, Kinomuzeum, Praha)
    * don't each carry their own copy of the same 12-entry map. */
  private val PolishMonthAbbrevs: Map[String, Int] = Map(
    "sty" -> 1, "lut" -> 2, "mar" -> 3, "kwi" -> 4, "maj" -> 5, "cze" -> 6,
    "lip" -> 7, "sie" -> 8, "wrz" -> 9, "paź" -> 10, "lis" -> 11, "gru" -> 12
  )

  /** The month number for a Polish three-letter abbreviation, case-insensitively
    * ("Cze"/"cze" → 6); `None` for anything not a known abbreviation. */
  def polishMonthAbbrev(token: String): Option[Int] =
    PolishMonthAbbrevs.get(token.trim.toLowerCase)

  /** The first `HH:mm` in `s` as a `LocalTime`, or `None` when there's no
    * match or the captured hour/minute is out of range. */
  def parseHHmm(s: String): Option[LocalTime] =
    HourMinute.findFirstMatchIn(s)
      .flatMap(m => Try(LocalTime.of(m.group(1).toInt, m.group(2).toInt)).toOption)

  /** The URL inside a CSS `url(...)` value, unwrapping `'`, `"` or `&quot;`
    * quoting. `None` when `s` holds no `url(...)`. */
  def cssUrl(s: String): Option[String] =
    CssUrl.findFirstMatchIn(s).map(_.group(1))

  /** Text of the `<dd>` immediately after the `<dt>` whose text contains
    * `label` (case-insensitive), searched within `dtSelector`. Trimmed; empty
    * → `None`. The Drupal-style cinema sites render film metadata as such
    * definition lists. */
  def ddField(document: Document, label: String, dtSelector: String = "dt"): Option[String] =
    document.select(dtSelector).asScala
      .find(_.text.toLowerCase.contains(label))
      .flatMap(dt => Option(dt.nextElementSibling))
      .filter(_.tagName == "dd")
      .map(_.text.trim)
      .filter(_.nonEmpty)

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

  /** Tidy block-text after URL stripping: drop spaces hugging a newline and
   *  cap blank-line runs at one, so an empty (URL-only) paragraph collapses
   *  instead of leaving a gap. */
  private def normalizeBlocks(s: String): String =
    s.replaceAll("[ \\t]*\n[ \\t]*", "\n")
      .replaceAll("\n{3,}", "\n\n")
      .trim
}
