package tools

import java.util.concurrent.ConcurrentHashMap

/**
 * Hand-rolled JS + CSS minifier for the inline `<script>` / `<style>`
 * blocks the Twirl templates emit on every page. Conservative on purpose
 * — no variable renaming, no AST work — but cuts ~40-50 % off the source
 * size of `_sharedJs` (~34 KB → ~18 KB) and `_sharedStyles` (~16 KB →
 * ~9 KB) by stripping comments and collapsing whitespace.
 *
 * Why hand-roll instead of pulling in Closure Compiler / Terser:
 *   - No npm in this project; staying Scala-only keeps the build simple.
 *   - A real obfuscator (variable renaming) would mangle the global
 *     handlers our HTML references by name (`hideFilm`, `toggleFavMovie`,
 *     etc.). Closure-SIMPLE preserves globals but adds a ~5 MB JAR and
 *     a parse step. Not worth it for the marginal extra bytes.
 *   - Comment + whitespace stripping is the bulk of the win in raw
 *     bytes AND in browser parse time. After gzip the wire saving is
 *     modest (~5 KB) but the browser still parses fewer source bytes,
 *     which matters on mobile CPUs.
 *
 * Safety properties:
 *   - JS line-comments and block-comments are recognised via a
 *     string-aware regex that won't strip a `//` appearing inside a
 *     string literal or template literal.
 *   - Newlines inside JS are preserved (collapsed runs to one),
 *     which keeps automatic-semicolon-insertion (ASI) safe — naive
 *     newline-stripping would break `return\n{…}` and equivalents.
 *   - CSS is whitespace-aggressive (CSS has no ASI) but still
 *     preserves space-separated values inside declarations (`1px
 *     solid #fff`).
 *
 * Memoised per-input via a ConcurrentHashMap keyed on input hash so
 * per-request render cost is O(1) string lookup after the first hit.
 * The cache is unbounded but the input set is bounded — every distinct
 * template-rendered block is one entry, ~dozens total even with
 * interpolated values.
 */
object Minify {

  /** Top-level: process an HTML fragment, minifying every `<style>` and
   *  `<script>` block found inside while leaving everything else
   *  untouched. The block boundaries are preserved so the surrounding
   *  markup keeps working. */
  def process(html: String): String = blockCache.computeIfAbsent(html, doProcess)

  private val blockCache = new ConcurrentHashMap[String, String]
  private val jsCache    = new ConcurrentHashMap[String, String]
  private val cssCache   = new ConcurrentHashMap[String, String]

  private def doProcess(html: String): String = {
    val sb = new java.lang.StringBuilder(html.length)
    var i = 0
    while (i < html.length) {
      // Find the next opening tag we care about; if none, copy the rest.
      val nextScript = html.indexOf("<script", i)
      val nextStyle  = html.indexOf("<style",  i)
      val (tagStart, tagName, minifier) = (nextScript, nextStyle) match {
        case (-1, -1)              => (-1,             "",       identity[String] _)
        case (s, -1)               => (s,              "script", minifyJs _)
        case (-1, t)               => (t,              "style",  minifyCss _)
        case (s, t)  if s < t      => (s,              "script", minifyJs _)
        case (_, t)                => (t,              "style",  minifyCss _)
      }
      if (tagStart < 0) {
        sb.append(html, i, html.length)
        i = html.length
      } else {
        // Copy verbatim up to the opening tag.
        sb.append(html, i, tagStart)
        // Walk through the opening tag's attributes to find the `>`.
        val openEnd = html.indexOf('>', tagStart)
        if (openEnd < 0) { sb.append(html, tagStart, html.length); i = html.length }
        else {
          // Self-closing or void tag — no body to minify.
          val isSelfClosing = html.charAt(openEnd - 1) == '/'
          sb.append(html, tagStart, openEnd + 1)
          if (isSelfClosing) i = openEnd + 1
          else {
            val closeTag = s"</$tagName>"
            val closeStart = html.indexOf(closeTag, openEnd + 1)
            if (closeStart < 0) { sb.append(html, openEnd + 1, html.length); i = html.length }
            else {
              val body = html.substring(openEnd + 1, closeStart)
              sb.append(minifier(body))
              sb.append(closeTag)
              i = closeStart + closeTag.length
            }
          }
        }
      }
    }
    sb.toString
  }

  // ── JS ────────────────────────────────────────────────────────────────────
  //
  // Strip comments via a regex that matches a string literal OR a
  // comment in one alternation, so comments INSIDE strings are kept
  // verbatim. The regex covers single-, double-, and backtick strings
  // with backslash escapes.
  //
  // After comment stripping, collapse runs of horizontal whitespace
  // (spaces + tabs) to a single space, and runs of 2+ newlines to one,
  // and trim trailing whitespace per line. Newlines themselves are
  // preserved so ASI keeps working — `return` followed by a newline
  // still terminates a statement.
  // Split the close-block-comment sequence across two raw-string
  // segments. Scala 3's lexer treats it as a multi-line-comment
  // terminator even when the surrounding open sequence is inside the
  // raw string. Joining the regex from parts dodges the ambiguity
  // without affecting the pattern semantics.
  private val JsCommentOrString = (
    """("(?:\\.|[^"\\\n])*"|""" +
    """'(?:\\.|[^'\\\n])*'|""" +
    """`(?:\\.|[^`\\])*`|""" +
    """//[^\n]*|""" +
    """/\*[\s\S]*?\*""" + "/)"
  ).r

  def minifyJs(src: String): String = jsCache.computeIfAbsent(src, doMinifyJs)

  private def doMinifyJs(src: String): String = {
    val noComments = JsCommentOrString.replaceAllIn(src, m => {
      val s = m.matched
      if (s.startsWith("//") || s.startsWith("/*")) ""
      else java.util.regex.Matcher.quoteReplacement(s)
    })
    val sb = new java.lang.StringBuilder(noComments.length)
    var i = 0
    val n = noComments.length
    while (i < n) {
      val c = noComments.charAt(i)
      if (c == ' ' || c == '\t') {
        sb.append(' ')
        while (i < n && (noComments.charAt(i) == ' ' || noComments.charAt(i) == '\t')) i += 1
      } else if (c == '\n' || c == '\r') {
        // Trim trailing horizontal whitespace already in sb, then one '\n'.
        while (sb.length > 0 && (sb.charAt(sb.length - 1) == ' ' || sb.charAt(sb.length - 1) == '\t'))
          sb.setLength(sb.length - 1)
        if (sb.length == 0 || sb.charAt(sb.length - 1) != '\n') sb.append('\n')
        i += 1
        while (i < n && (noComments.charAt(i) == '\n' || noComments.charAt(i) == '\r' ||
                         noComments.charAt(i) == ' ' || noComments.charAt(i) == '\t')) i += 1
      } else {
        sb.append(c); i += 1
      }
    }
    sb.toString.trim
  }

  // ── CSS ───────────────────────────────────────────────────────────────────
  //
  // Strip block comments (CSS has no line-comment form).
  // Collapse all whitespace runs (including newlines) to single space.
  // Strip whitespace around `{ } ; : ,` (and after `:` inside selectors
  // is fine too, browsers accept `prop:value` AND `:hover`). Drop the
  // final `;` before `}` since it's optional.
  private val CssBlockComment = ("""/\*[\s\S]*?\*""" + "/").r

  def minifyCss(src: String): String = cssCache.computeIfAbsent(src, doMinifyCss)

  private def doMinifyCss(src: String): String = {
    val noComments = CssBlockComment.replaceAllIn(src, "")
    val collapsed  = noComments.replaceAll("\\s+", " ").trim
    val tightened  = collapsed
      .replace(" {", "{").replace("{ ", "{")
      .replace(" }", "}").replace("} ", "}")
      .replace(" ;", ";").replace("; ", ";")
      .replace(" :", ":").replace(": ", ":")
      .replace(" ,", ",").replace(", ", ",")
      .replace(";}", "}")
    tightened
  }
}
