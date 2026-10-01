package services.enrichment.scraping

/**
 * A page's `<script …>…</script>` elements, each as its attribute text and its raw body, read without
 * building the page's DOM: a script's text is raw up to its first `</script` closing (HTML's
 * script-data state), so the few data islands a rating page is read for — its JSON-LD blocks, Rotten
 * Tomatoes' `media-scorecard-json` — need no parser.
 *
 * Exactly what `(?is)<script\b([^>]*)>(.*?)</script\s*>` matches, in its order, but found with
 * `indexOf` rather than that regex's lazy `.*?`, which tried the closing at every character of every
 * script body: rating pages inline hundreds of kilobytes of script, and that scan was the largest
 * single cost of a rating refresh (~11% of a US convergence leg's CPU, JFR). `HtmlScriptsSpec` holds
 * the two to the same answer; `JsonLdScanSpec` holds the readers to Jsoup's on every recorded page.
 */
object HtmlScripts {

  /** One script element: the text between `<script` and its `>`, and its raw body. */
  final case class Script(attributes: String, body: String)

  private val Open  = "script"
  private val Close = "/script"

  /** Every script element of `html`, in page order. */
  def all(html: String): Iterator[Script] = new Iterator[Script] {
    private var from = 0
    private var upcoming: Option[Script] = advance()
    def hasNext: Boolean = upcoming.isDefined
    def next(): Script = { val script = upcoming.get; upcoming = advance(); script }

    private def advance(): Option[Script] = {
      var at = html.indexOf('<', from)
      var found: Option[Script] = None
      while (found.isEmpty && at >= 0) {
        val name = at + 1 + Open.length
        if (asciiIgnoringCase(html, at + 1, Open) && !wordAt(html, name)) {
          val end = html.indexOf('>', name)
          // No `>` after this opening means none after any later one, and no closing after the body
          // means none after any later body: nothing further can match.
          if (end < 0) at = -1
          else closing(html, end + 1) match {
            case Some((bodyEnd, matchEnd)) =>
              found = Some(Script(html.substring(name, end), html.substring(end + 1, bodyEnd)))
              from = matchEnd
            case None => at = -1
          }
        } else at = html.indexOf('<', at + 1)
      }
      if (found.isEmpty) from = html.length
      found
    }
  }

  /** The first `</script\s*>` at or after `from`: where the body ends, and where the match does. */
  private def closing(html: String, from: Int): Option[(Int, Int)] = {
    var at = html.indexOf('<', from)
    while (at >= 0) {
      if (asciiIgnoringCase(html, at + 1, Close)) {
        var i = at + 1 + Close.length
        while (i < html.length && isSpace(html.charAt(i))) i += 1
        if (i < html.length && html.charAt(i) == '>') return Some((at, i + 1))
      }
      at = html.indexOf('<', at + 1)
    }
    None
  }

  /** `html` holds `lower` at `at`, ASCII letters compared without case — the regex's `(?i)`, which
   *  folds ASCII only. */
  private def asciiIgnoringCase(html: String, at: Int, lower: String): Boolean =
    at + lower.length <= html.length && {
      var i = 0
      while (i < lower.length && { val c = html.charAt(at + i); (if (c >= 'A' && c <= 'Z') (c + 32).toChar else c) == lower.charAt(i) }) i += 1
      i == lower.length
    }

  /** A word character at `at` — the regex's `\b` after `script` fails on one. */
  private def wordAt(html: String, at: Int): Boolean =
    at < html.length && { val c = html.charAt(at); (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_' }

  /** The regex's `\s`: ASCII whitespace. */
  private def isSpace(c: Char): Boolean = c == ' ' || c == '\t' || c == '\n' || c == '\u000B' || c == '\f' || c == '\r'
}
