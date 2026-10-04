package kinowo.build

/**
 * The digest behind the identity rules version and the venue slot version (`IdentityRulesSources`, `build.sbt`): every
 * digested path, then its content — a Scala source as its CODE (`code`), so an edit to a comment or to whitespace no
 * compiler reads re-resolves no worker's corpus and rebuilds no venue slot; any other file byte for byte.
 *
 * Compiled twice: into the build (Scala 2.12) and into common's tests (Scala 3), which check it — so it is written in
 * the syntax both accept.
 */
object SourceDigest {

  /** SHA-256 over each path and its digested content (`code` for a `.scala` source), in the order given. */
  def of(paths: Seq[String], read: String => Array[Byte]): String = {
    val digest = java.security.MessageDigest.getInstance("SHA-256")
    paths.foreach { path =>
      digest.update(path.getBytes("UTF-8"))
      val bytes = read(path)
      digest.update(if (path.endsWith(".scala")) code(new String(bytes, "UTF-8")).getBytes("UTF-8") else bytes)
    }
    digest.digest.map("%02x".format(_)).mkString
  }

  private final class Unlexable extends RuntimeException(null, null, false, false)

  /**
   * `source` without what cannot change what it compiles to: its comments (line, block, nested block, Scaladoc),
   * whitespace at the end of a line, the lines only a comment held, and every blank line after the first of a run.
   * What can is kept: every literal byte for byte (a line or block comment marker inside a string, a triple-quoted string, an
   * interpolation and its `${...}` splices, a character literal or a backquoted name is no comment), the indentation
   * (Scala 3 reads it), the column a token starts at after a comment on its line, a line end where a comment held one
   * (it separates statements), and whether two lines have a blank line between them (it can end a statement that a
   * single line end would continue). A source this cannot lex — an unterminated literal or comment — is returned as is.
   */
  def code(source: String): String = lexed(source).getOrElse(source)

  /** `code(source)`, or none if `source` cannot be lexed. */
  def lexed(source: String): Option[String] =
    try Some(new CodeLexer(source).run()) catch { case _: Unlexable => None }

  private final class CodeLexer(s: String) {
    private val n          = s.length
    private val out        = new java.lang.StringBuilder
    private val pending    = new java.lang.StringBuilder // whitespace not yet known to precede a token on its line
    private var hasContent = false             // the current output line holds a token
    private var hadComment = false             // ... or held a comment
    private var lastBlank  = true              // the last line written is blank (or none is): no blank line follows it

    private def at(i: Int): Char = if (i < n) s.charAt(i) else '\u0000'
    private def fail(): Nothing = throw new Unlexable

    private def token(from: Int, until: Int): Unit = {
      out.append(pending); pending.setLength(0)
      out.append(s, from, until)
      hasContent = true
    }

    private def endLine(): Unit = {
      if (hasContent) { out.append('\n'); lastBlank = false }
      else if (!hadComment && !lastBlank) { out.append('\n'); lastBlank = true }
      pending.setLength(0); hasContent = false; hadComment = false
    }

    def run(): String = {
      var i = 0
      while (i < n) {
        val c = s.charAt(i)
        if (c == '\n') { endLine(); i += 1 }
        else if (c == ' ' || c == '\t' || c == '\r' || c == '\f') { pending.append(c); i += 1 }
        else if (c == '/' && at(i + 1) == '/') {
          while (i < n && s.charAt(i) != '\n') i += 1
          hadComment = true
        }
        else if (c == '/' && at(i + 1) == '*') {
          val end = skipBlockComment(i)
          val newline = s.lastIndexOf('\n', end - 1)
          if (newline >= i) {
            // A comment over several lines ends the line it starts on and leaves the next token at its own column.
            hadComment = true
            endLine()
            hadComment = true
            pending.append(columns(newline + 1, end))
          } else {
            // On one line it separates the tokens beside it; before a line's first token, it keeps that token's column.
            if (hasContent) pending.append(' ') else pending.append(columns(i, end))
            hadComment = true
          }
          i = end
        }
        else {
          val end = skipToken(i)
          token(i, end)
          i = end
        }
      }
      if (hasContent) out.append('\n')
      out.toString
    }

    /** Blanks as wide as `s(from until until)`, tabs kept, so what follows starts at the same column. */
    private def columns(from: Int, until: Int): String = {
      val b = new java.lang.StringBuilder
      var i = from
      while (i < until) { b.append(if (s.charAt(i) == '\t') '\t' else ' '); i += 1 }
      b.toString
    }

    /** The end of the token (or literal) at `i`, which is no whitespace and starts no comment. */
    private def skipToken(i: Int): Int = s.charAt(i) match {
      case '"'  => skipString(i)
      case '\'' => skipQuote(i)
      case '`'  => skipBackquoted(i)
      case _    => i + 1
    }

    private def isIdentifierPart(c: Char): Boolean = c == '_' || Character.isUnicodeIdentifierPart(c)

    /** `/* ... */` at `i`, nested ones included: the index past its end. */
    private def skipBlockComment(start: Int): Int = {
      var i = start + 2; var depth = 1
      while (depth > 0) {
        if (i >= n) fail()
        if (s.charAt(i) == '/' && at(i + 1) == '*') { depth += 1; i += 2 }
        else if (s.charAt(i) == '*' && at(i + 1) == '/') { depth -= 1; i += 2 }
        else i += 1
      }
      i
    }

    /** A string literal at `i` (a `"`), plain or interpolated (an identifier right before it), single-line or
     *  triple-quoted: the index past its closing quote. */
    private def skipString(start: Int): Int = {
      val interpolated = start > 0 && isIdentifierPart(s.charAt(start - 1))
      if (at(start + 1) == '"' && at(start + 2) == '"') {
        var i = start + 3
        while (true) {
          if (i >= n) fail()
          val c = s.charAt(i)
          if (c == '"' && at(i + 1) == '"' && at(i + 2) == '"') {
            i += 3
            while (at(i) == '"') i += 1 // a run of quotes closes on its last three
            return i
          }
          else if (interpolated && c == '$') i = skipDollar(i)
          else i += 1
        }
        fail()
      } else {
        var i = start + 1
        while (true) {
          if (i >= n) fail()
          val c = s.charAt(i)
          if (c == '"') return i + 1
          else if (c == '\n') fail()
          else if (c == '\\') i += 2
          else if (interpolated && c == '$' && at(i + 1) == '"') i += 2
          else if (interpolated && c == '$') i = skipDollar(i)
          else i += 1
        }
        fail()
      }
    }

    /** `$` inside an interpolation: `$$`, a `${...}` splice, or a `$name` (whose name is plain text). */
    private def skipDollar(i: Int): Int =
      if (at(i + 1) == '$') i + 2
      else if (at(i + 1) == '{') skipSplice(i + 1)
      else i + 1

    /** The code of a `${...}` splice, its `{` at `start`: the index past its matching `}`, with every literal and
     *  comment inside it skipped as a whole. Kept verbatim. */
    private def skipSplice(start: Int): Int = {
      var i = start + 1; var depth = 1
      while (depth > 0) {
        if (i >= n) fail()
        val c = s.charAt(i)
        if (c == '/' && at(i + 1) == '/') { while (i < n && s.charAt(i) != '\n') i += 1 }
        else if (c == '/' && at(i + 1) == '*') i = skipBlockComment(i)
        else if (c == '{') { depth += 1; i += 1 }
        else if (c == '}') { depth -= 1; i += 1 }
        else i = skipToken(i)
      }
      i
    }

    /** A `'` at `i`: a character literal (`'x'`, `'\n'`, `'A'`), else a lone quote (a Scala 3 quote `'{`, `'[`). */
    private def skipQuote(i: Int): Int =
      if (at(i + 1) == '\\') {
        var j = i + 3
        while (at(j) != '\'') { if (j >= n || s.charAt(j) == '\n') fail(); j += 1 }
        j + 1
      }
      else if (at(i + 1) != '\n' && at(i + 2) == '\'') i + 3
      else if (Character.isHighSurrogate(at(i + 1)) && at(i + 3) == '\'') i + 4
      else i + 1

    /** A backquoted name at `i`: the index past its closing backquote. */
    private def skipBackquoted(i: Int): Int = {
      val end = s.indexOf('`', i + 1)
      val newline = s.indexOf('\n', i + 1)
      if (end < 0 || (newline >= 0 && newline < end)) fail()
      end + 1
    }
  }
}
