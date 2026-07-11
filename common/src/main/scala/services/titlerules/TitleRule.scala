package services.titlerules

import scala.util.matching.Regex

/** One editable regex-replace step in the title-normalisation pipeline.
 *
 *  The full set of these (stored in the `titleRules` Mongo collection, edited
 *  via the admin page, broadcast to web + worker over a change stream) describes
 *  every prefix/suffix/canonicalisation the app strips. The seed set in
 *  [[TitleRules]] is transcribed verbatim from the formerly-hardcoded
 *  regexes, so behaviour is unchanged until someone edits a rule.
 *
 *  `replacement` follows `scala.util.matching.Regex` replacement semantics: `""`
 *  is a pure strip, and `$1`/`$2` reference capture groups. A literal `$` or `\`
 *  in the replacement must be escaped — surfaced as guidance in the editor. */
case class TitleRule(
  id:          String,
  scope:       RuleScope,
  // Some(cinemaId) iff scope == PerCinema; None for the global tiers.
  cinemaId:    Option[String],
  pattern:     String,
  replacement: String,
  // replaceAllIn (true) vs replaceFirstIn (false) — matches whether the legacy
  // code used `.replaceAll` / String.replace (all) or `.replaceFirstIn` (first).
  applyAll:    Boolean,
  // Application order within a scope. Lower runs first; ties broken by id.
  order:       Int,
  // When true, this rule runs AFTER every non-last rule of the same scope/cinema,
  // regardless of `order` — `TitleRuleSet` sorts each tier by (last, order, id).
  // Surfaced in the editor as a "last" checkbox / a separate ordered list per
  // record. Lets a catch-all cleanup sit after the specific strips.
  last:        Boolean        = false,
  enabled:     Boolean        = true,
  // Optional semantic label. The programme-prefix rule carries
  // tag = Some("programmePrefix") so `TitleNormalizer.programmePrefix` can find
  // it to EXTRACT (not strip) the matched banner for separate-row casing.
  tag:         Option[String] = None,
  note:        Option[String] = None
) {
  // Compiled once per rule. An invalid pattern (e.g. a half-typed regex from the
  // editor) is caught here and the rule becomes a no-op rather than throwing
  // inside the hot normalisation path and taking down every scrape.
  val compiled: Option[Regex] =
    try Some(new Regex(pattern)) catch { case _: Throwable => None }

  val patternValid: Boolean = compiled.isDefined

  /** True when the pattern is anchored to the START of the title (a `^`, after an
   *  optional leading inline-flag group like `(?i)`) — i.e. a prefix/banner rule
   *  such as the programme prefixes or the Cykl banner, as opposed to a `$`-anchored
   *  suffix strip. Drives `TitleRuleSet.leadingBannerBoundary` (display casing). */
  val isPrefixAnchored: Boolean =
    pattern.replaceFirst("""^\(\?[a-zA-Z]+\)""", "").startsWith("^")

  /** Apply this rule to `in`. No-op when disabled or the pattern didn't compile.
   *
   *  The replace itself is also guarded: a `replacement` carrying a bare `$` or a
   *  trailing `\` is, to Java's `Matcher`, an illegal group reference and throws
   *  `IllegalArgumentException` (or `IndexOutOfBoundsException` for a `$N` past the
   *  group count). A malformed editor-entered replacement must degrade to a no-op,
   *  same as a malformed pattern — never throw inside the hot normalisation path
   *  or the admin "affected" preview (Sentry KINOWO-Y). */
  def apply(in: String): String = compiled match {
    case Some(re) if enabled =>
      try if (applyAll) re.replaceAllIn(in, replacement) else re.replaceFirstIn(in, replacement)
      catch { case _: IllegalArgumentException | _: IndexOutOfBoundsException => in }
    case _ => in
  }
}
