package services.titlerules

/** Expands `{{NAME}}` placeholder tokens in a rule pattern from a `name → regex
 *  snippet` map (see [[TitleRulePlaceholders]]), so a shared regex fragment lives
 *  in one place and many rules reference it by name.
 *
 *  Plain literal substitution (`String.replace`), NOT a regex replace: the
 *  expansion text is full of regex metacharacters (`\`, `[`, `$`, …) a
 *  regex-replacement would mis-handle (a `$1` in the snippet would be read as a
 *  group reference). Iterated to a fixpoint so a placeholder may reference
 *  another, with a depth cap that turns a cycle (or a too-deep nest) into a
 *  left-as-is token rather than a hang.
 *
 *  An unknown token is left VERBATIM — `{{FOO}}` is then an invalid regex, so the
 *  rule surfaces as invalid in the editor instead of silently matching nothing. */
object PlaceholderExpander {
  val MaxDepth = 10

  private val TokenRegex = """\{\{[A-Za-z][A-Za-z0-9_]*\}\}""".r

  /** True iff `s` still carries a `{{NAME}}` token — i.e. a reference that wasn't
   *  resolved (an unknown placeholder, or a cycle the depth cap stopped). Such a
   *  rule must be treated as invalid, NOT compiled as a literal `{{NAME}}`: don't
   *  rely on the regex engine rejecting the braces. */
  def containsToken(s: String): Boolean = TokenRegex.findFirstIn(s).isDefined

  private def token(name: String): String = "{{" + name + "}}"

  /** True iff `s` still contains a `{{NAME}}` token for one of the KNOWN
   *  placeholders — the loop condition (an unknown token never resolves, so it
   *  must not keep the loop spinning). */
  private def hasKnownToken(s: String, placeholders: Map[String, String]): Boolean =
    placeholders.keysIterator.exists(name => s.contains(token(name)))

  def expand(pattern: String, placeholders: Map[String, String] = TitleRulePlaceholders.all,
             maxDepth: Int = MaxDepth): String = {
    if (placeholders.isEmpty || !hasKnownToken(pattern, placeholders)) return pattern
    var current = pattern
    var depth   = 0
    while (depth < maxDepth && hasKnownToken(current, placeholders)) {
      current = placeholders.foldLeft(current) { case (acc, (name, expansion)) =>
        acc.replace(token(name), expansion)
      }
      depth += 1
    }
    current
  }
}
