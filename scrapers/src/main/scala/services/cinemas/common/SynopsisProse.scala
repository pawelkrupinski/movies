package services.cinemas.common

import scala.jdk.CollectionConverters.*

import org.jsoup.nodes.Element

/**
 * A venue's synopsis as prose: the text of its container with paragraph, line and emphasis structure kept as
 * newlines and lightweight markdown ([[blockText]]), the junk sub-trees a venue wraps around it (booking buttons,
 * agendas, "read more" links) and bare URLs dropped ([[cleanSynopsis]]).
 */
object SynopsisProse {
  private val BareUrl = """(?i)\b(?:https?://|www\.)\S+""".r
  private val CutShort = """(?:\.\.\.|…)\s*$""".r

  /** Whether a venue cut this text short with a trailing ellipsis ("..." / "…"): a listing's teaser of the
   *  synopsis, never the whole of it, so never what a page shows over a complete one. */
  def isCutShort(text: String): Boolean = CutShort.findFirstIn(text).isDefined

  /** The text without the trailing ellipsis a venue cut it short with. */
  def withoutCut(text: String): String = CutShort.replaceFirstIn(text, "").trim

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
  def normalizeBlocks(s: String): String =
    s.replaceAll("[ \\t]*\n[ \\t]*", "\n")
      .replaceAll("\n{3,}", "\n\n")
      .trim
}
