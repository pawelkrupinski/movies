package tools

/**
 * The film synopsis carries a tiny, fixed markdown subset — `**bold**` and
 * `*italic*` (plus `***bold+italic***`) — emitted by `ScraperParse.blockText`
 * from the cinema pages' `<b>`/`<strong>`/`<i>`/`<em>` tags, alongside the
 * `\n`/`\n\n` paragraph breaks. This is the single place that understands that
 * subset, so the web renderer and the plain-text consumers can't drift:
 *
 *  - [[toInlineHtml]] for the web detail page: HTML-escape the prose first
 *    (so it can't inject markup), THEN turn the markers into `<strong>`/`<em>`.
 *    Newlines are left as-is — the `.synopsis { white-space: pre-wrap }` rule
 *    renders them.
 *  - [[strip]] for consumers that need plain text — the OG share-card PNG and
 *    the `og:description` meta tag — so they show `bold`, never `**bold**`.
 *
 * The mobile apps receive the raw markdown over `/api/details` and render it
 * themselves (iOS `AttributedString(markdown:)`, Android markdown→AnnotatedString).
 */
object SynopsisMarkdown {

  // Triple first (bold+italic), then double, then single, each non-greedy and
  // DOTALL so a run can span the paragraph newlines. Unpaired markers are left
  // untouched (treated as literal text).
  private val Triple = "(?s)\\*\\*\\*(.+?)\\*\\*\\*".r
  private val Bold   = "(?s)\\*\\*(.+?)\\*\\*".r
  private val Italic = "(?s)\\*(.+?)\\*".r

  private def quote(g: String) = java.util.regex.Matcher.quoteReplacement(g)

  /** Normalise a synopsis to clean, well-formed markdown — applied once at the
   *  read boundary ([[models.MovieRecord.synopsis]]) so EVERY source path is
   *  guaranteed valid, whether the markers came from `ScraperParse.blockText`,
   *  a cinema JSON `description` that already held `**`, or anything else.
   *
   *  Keeps only emphasis that hugs non-whitespace on both edges and contains no
   *  line break; strips every other stray `*`/`**` (a missing bold is fine, a
   *  dangling marker is not). Bold is hidden behind placeholders first so the
   *  italic pass can't split a `**…**`. Finally caps blank-line runs and trims. */
  def sanitize(s: String): String = {
    val boldValid = "\\*\\*(?=\\S)((?:(?!\\*\\*)[^\\n])+?)(?<=\\S)\\*\\*".r
    val italValid = "\\*(?=\\S)([^*\\n]+?)(?<=\\S)\\*".r
    val (bO, bC, iO, iC) = ("\u0011", "\u0012", "\u0013", "\u0014")
    val withBold = boldValid.replaceAllIn(s, m => bO + quote(m.group(1)) + bC).replace("**", "")
    val withItal = italValid.replaceAllIn(withBold, m => iO + quote(m.group(1)) + iC).replace("*", "")
    withItal.replace(bO, "**").replace(bC, "**").replace(iO, "*").replace(iC, "*")
      .replaceAll("[ \\t]*\n[ \\t]*", "\n")  // drop spaces hugging a newline
      .replaceAll("\n{3,}", "\n\n")          // cap blank-line runs at one
      .trim
  }

  /** Inline HTML for the web detail page: escaped prose with the markdown
   *  emphasis turned into `<strong>`/`<em>`. The result is trusted markup
   *  (`@Html(...)`); every character that came from the synopsis itself is
   *  escaped before any tag is introduced, so this can't inject HTML. */
  def toInlineHtml(s: String): String = {
    val escaped = s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
    val triple  = Triple.replaceAllIn(escaped, m => "<strong><em>" + java.util.regex.Matcher.quoteReplacement(m.group(1)) + "</em></strong>")
    val bold    = Bold.replaceAllIn(triple, m => "<strong>" + java.util.regex.Matcher.quoteReplacement(m.group(1)) + "</strong>")
    Italic.replaceAllIn(bold, m => "<em>" + java.util.regex.Matcher.quoteReplacement(m.group(1)) + "</em>")
  }

  /** Plain text: the same markdown with its emphasis markers removed. Newlines
   *  are preserved. */
  def strip(s: String): String = {
    val triple = Triple.replaceAllIn(s, m => java.util.regex.Matcher.quoteReplacement(m.group(1)))
    val bold   = Bold.replaceAllIn(triple, m => java.util.regex.Matcher.quoteReplacement(m.group(1)))
    Italic.replaceAllIn(bold, m => java.util.regex.Matcher.quoteReplacement(m.group(1)))
  }
}
