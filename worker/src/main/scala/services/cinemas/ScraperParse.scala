package services.cinemas

import org.jsoup.nodes.{Document, Element}

import java.time.LocalTime
import java.util.Locale
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

  /** Lower-case format/version word → the display token it maps to, shared with
   *  the cinema clients that surface a screening's version as a `Showtime.format`
   *  badge. The token vocabulary is fixed across the app — 2D, 3D, IMAX, 4DX,
   *  DUB (dubbing), NAP (subtitles/napisy), LEK (lektor) — so all sources agree.
   *  Words droppable from a title but with no version meaning (premiera, dolby,
   *  atmos, vod, …) are NOT here; they're stripped without yielding a token. */
  val FormatToken: Map[String, String] = Map(
    "napisy" -> "NAP", "nap" -> "NAP", "dubbing" -> "DUB", "dub" -> "DUB", "dubb" -> "DUB",
    "lektor" -> "LEK", "2d" -> "2D", "3d" -> "3D", "imax" -> "IMAX", "4dx" -> "4DX"
  )

  // Words that, alone, mark a trailing screening-format/version tag.
  private val FormatVersionWords = Set(
    "2d", "3d", "imax", "dolby", "atmos", "4dx", "dubbing", "napisy", "lektor",
    "dubb", "dub", "nap", "premiera", "krajowa", "pokaz", "jednorazowy",
    "specjalny", "przedpremierowy", "vood", "vod", "hd")
  private val FormatSeparators = Set("-", "–", "—", "|", "/", ":")
  private val FormatBracketTag = """\s*\[[^\]]*\]\s*$""".r
  private val FormatParenTag   =
    """(?i)\s*\((?:[^)]*\b(?:2D|3D|IMAX|DOLBY|4DX|dubbing|napisy|lektor|pokaz)\b[^)]*)\)\s*$""".r
  // Underscore-glued format/version tag — some bilety24 portals (Forum Bolesławiec)
  // join the version word straight to the title with an underscore:
  // "Supergirl_dubbing", "Spider-Man. Całkiem nowy dzień_3D". Un-glue ONLY before a
  // known format/version word so a legitimate underscore ("Seans w ciemno_7.26",
  // the "_DKF"/"_FKS" programme tags) is left intact and those titles aren't re-split.
  private val GluedFormatUnderscore =
    ("(?i)_(?=(?:" + FormatVersionWords.toSeq.sortBy(-_.length).mkString("|") + """)\b)""").r

  /** The bare lower-cased word of a title token (paren/bracket/punctuation
   *  peeled), or `""` for an empty token. */
  private def bareWord(tok: String): String =
    tok.toLowerCase(Locale.ROOT).replaceAll("""[\[\]().,]""", "")

  private def isDroppableTag(tok: String): Boolean = {
    val w = bareWord(tok)
    w.isEmpty || FormatSeparators.contains(w) || FormatVersionWords.contains(w)
  }

  /** Strip the trailing format/version tags a ticketing-portal title carries —
   *  `[2D napisy]`, `(2D dubbing)`, ` - napisy`, ` / 2D dubbing`, ` NAPISY 2D`,
   *  ` dubb` — so the same film's screening variants collapse to one title and
   *  merge. Removes trailing tokens that are pure separators or format/version
   *  words after peeling any bracket/paren tag, repeating until stable. A
   *  leading programme tag (e.g. "DKF -") is never trailing, so it survives. */
  def stripFormatTags(raw: String): String = extractFormatTags(raw)._1

  /** Like [[stripFormatTags]], but also returns the display [[FormatToken]]s
   *  (2D, NAP, DUB, …) recognised among the stripped trailing tags, so a
   *  scraper can surface the screening's version as a `Showtime.format` badge
   *  while keeping the cleaned title byte-identical to `stripFormatTags`.
   *  Tokens are de-duplicated, and ordered as they appeared left-to-right in
   *  the original title (e.g. "(2D NAPISY)" → `List("2D", "NAP")`). Stripped
   *  words with no version meaning (dolby, atmos, premiera, …) yield no token. */
  def extractFormatTags(raw: String): (String, List[String]) = {
    var t    = GluedFormatUnderscore.replaceAllIn(raw.replaceAll("\\s+", " ").trim, " ")
    var previous = ""
    val dropped = scala.collection.mutable.Set.empty[String]
    while (t != previous) {
      previous = t
      // Tokens peeled inside a [..]/(..) tag also carry version meaning, so
      // capture them before the regex deletes the whole tag.
      FormatBracketTag.findFirstIn(t).foreach(captureTagWords(_, dropped))
      t = FormatBracketTag.replaceFirstIn(t, "").trim
      FormatParenTag.findFirstIn(t).foreach(captureTagWords(_, dropped))
      t = FormatParenTag.replaceFirstIn(t, "").trim
      var toks = t.split(" ").filter(_.nonEmpty).toVector
      while (toks.length > 1 && isDroppableTag(toks.last)) {
        captureTagWords(toks.last, dropped)
        toks = toks.dropRight(1)
      }
      // Some portals glue the format tag to the title with a separator and a
      // space ("Straszny Film- 2D dubbing"): dropping "2D"/"dubbing" leaves a
      // dangling "Film-". Trim a trailing separator so the variants collapse to
      // the canonical title and merge across cinemas. Re-runs via the outer loop.
      t = toks.mkString(" ").replaceAll("""\s*[-–—|/:]+\s*$""", "").trim
    }
    // Order the display tokens by where their word first appears in the raw
    // title (left-to-right), so "(2D NAPISY)" → List("2D","NAP") regardless of
    // the right-to-left order the strip loop peeled them off.
    val tokens = raw.toLowerCase(Locale.ROOT)
      .split("""[\s\[\]().,/|:_-]+""")
      .iterator
      .filter(dropped.contains)
      .flatMap(FormatToken.get)
      .distinct
      .toList
    (t, tokens)
  }

  /** Record every [[FormatToken]]-bearing word found in `chunk` (a single token
   *  or a whole `(...)`/`[...]` tag) into `out`. */
  private def captureTagWords(chunk: String, out: scala.collection.mutable.Set[String]): Unit =
    chunk.split("""[\s\[\]()]+""").iterator
      .map(w => bareWord(w))
      .filter(FormatToken.contains)
      .foreach(out.add)

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
