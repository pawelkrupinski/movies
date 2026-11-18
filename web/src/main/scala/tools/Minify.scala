package tools

import tools.Whitespace.collapsedSpaces


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
 * Stateless: every call does the full work. The per-request path goes
 * through a [[MemoisingMinifier]] the web composition root owns, which
 * wraps these functions in its own caches.
 */
object Minify extends Minifier {

  /** Top-level: process an HTML fragment, minifying every `<style>` and
   *  `<script>` block found inside while leaving everything else
   *  untouched. The block boundaries are preserved so the surrounding
   *  markup keeps working. */
  def process(html: String): String = process(html, minifyJs, minifyCss)

  /** [[process]] with the per-block minifiers supplied by the caller — the
   *  seam [[MemoisingMinifier]] uses to route each block through its caches. */
  def process(html: String, minifyJs: String => String, minifyCss: String => String): String = {
    val sb = new java.lang.StringBuilder(html.length)
    var i = 0
    while (i < html.length) i = copyThroughNextBlock(html, i, sb, minifyJs, minifyCss)
    sb.toString
  }

  /** Copies `html` from `from` up to and through the next `<script>` / `<style>` block (its body minified), or
   *  the rest when there is none; answers where to go on from. */
  private def copyThroughNextBlock(html: String, from: Int, sb: java.lang.StringBuilder,
                                   minifyJs: String => String, minifyCss: String => String): Int = {
    val nextScript = html.indexOf("<script", from)
    val nextStyle  = html.indexOf("<style",  from)
    if (nextScript < 0 && nextStyle < 0) { sb.append(html, from, html.length); html.length }
    else if (nextStyle < 0 || (nextScript >= 0 && nextScript < nextStyle)) copyBlock(html, from, nextScript, "script", minifyJs, sb)
    else copyBlock(html, from, nextStyle, "style", minifyCss, sb)
  }

  /** Copies verbatim up to the opening tag at `tagStart`, the tag itself, and its body through `minifier`;
   *  answers the index after the block. */
  private def copyBlock(html: String, from: Int, tagStart: Int, tagName: String, minifier: String => String,
                        sb: java.lang.StringBuilder): Int = {
    sb.append(html, from, tagStart)
    // Walk through the opening tag's attributes to find the `>`.
    val openEnd = html.indexOf('>', tagStart)
    if (openEnd < 0) { sb.append(html, tagStart, html.length); return html.length }
    sb.append(html, tagStart, openEnd + 1)
    // Self-closing or void tag — no body to minify.
    if (html.charAt(openEnd - 1) == '/') return openEnd + 1
    val closeTag   = s"</$tagName>"
    val closeStart = html.indexOf(closeTag, openEnd + 1)
    if (closeStart < 0) { sb.append(html, openEnd + 1, html.length); html.length }
    else {
      sb.append(minifier(html.substring(openEnd + 1, closeStart)))
      sb.append(closeTag)
      closeStart + closeTag.length
    }
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

  def minifyJs(src: String): String = {
    val noComments = JsCommentOrString.replaceAllIn(src, m => {
      val s = m.matched
      if (s.startsWith("//") || s.startsWith("/*")) ""
      else java.util.regex.Matcher.quoteReplacement(s)
    })
    collapseJsWhitespace(noComments).trim
  }

  private def collapseJsWhitespace(src: String): String = {
    val sb = new java.lang.StringBuilder(src.length)
    var i = 0
    while (i < src.length) {
      val c = src.charAt(i)
      if (isHorizontalSpace(c)) { sb.append(' '); i = skipHorizontalSpace(src, i) }
      else if (c == '\n' || c == '\r') { endLine(sb); i = skipWhitespace(src, i + 1) }
      else { sb.append(c); i += 1 }
    }
    sb.toString
  }

  private def isHorizontalSpace(c: Char): Boolean = c == ' ' || c == '\t'

  private def skipHorizontalSpace(src: String, from: Int): Int = {
    var i = from
    while (i < src.length && isHorizontalSpace(src.charAt(i))) i += 1
    i
  }

  private def skipWhitespace(src: String, from: Int): Int = {
    var i = from
    while (i < src.length && (isHorizontalSpace(src.charAt(i)) || src.charAt(i) == '\n' || src.charAt(i) == '\r')) i += 1
    i
  }

  /** Trim trailing horizontal whitespace already in `sb`, then one '\n'. */
  private def endLine(sb: java.lang.StringBuilder): Unit = {
    while (sb.length > 0 && isHorizontalSpace(sb.charAt(sb.length - 1))) sb.setLength(sb.length - 1)
    if (sb.length == 0 || sb.charAt(sb.length - 1) != '\n') sb.append('\n')
  }

  // ── CSS ───────────────────────────────────────────────────────────────────
  //
  // Strip block comments (CSS has no line-comment form).
  // Collapse all whitespace runs (including newlines) to single space.
  // Strip whitespace around `{ } ; : ,` (and after `:` inside selectors
  // is fine too, browsers accept `prop:value` AND `:hover`). Drop the
  // final `;` before `}` since it's optional.
  private val CssBlockComment = ("""/\*[\s\S]*?\*""" + "/").r

  def minifyCss(src: String): String = {
    val noComments = CssBlockComment.replaceAllIn(src, "")
    val collapsed  = noComments.collapsedSpaces
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
