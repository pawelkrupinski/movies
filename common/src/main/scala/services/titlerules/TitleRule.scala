package services.titlerules

import models.Country

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
  note:        Option[String] = None,
  // Countries whose corpus this rule applies to. `None` — the default — means
  // every country, which is right for language-neutral rules (format tags,
  // 4K-restored suffixes, punctuation). A rule that rewrites words MUST name its
  // languages: the canonical " & " → " i " unification is Polish, and applying it
  // everywhere spelled a German film "Minions i Monster" and keyed it
  // `minionsimonster` — a key no German cinema slot can ever produce, so every
  // settle re-canonicalised the row.
  countries:   Option[Set[Country]] = None
) {
  /** True when this rule is in force for the country a process serves. A rule
   *  with no declared countries applies everywhere. */
  def appliesTo(country: Country): Boolean = countries.forall(_.contains(country))

  // Compiled once per rule. An invalid pattern (e.g. a half-typed regex from the
  // editor) is caught here and the rule becomes a no-op rather than throwing
  // inside the hot normalisation path and taking down every scrape.
  val compiled: Option[Regex] =
    try Some(new Regex(pattern)) catch { case _: Throwable => None }

  val patternValid: Boolean = compiled.isDefined

  /** True when this rule WRITES words rather than only deleting text. A strip
   *  (empty replacement) or a pure capture-group reshuffle ("$1") is
   *  language-neutral: it can't invent a word that belongs to one language. A
   *  replacement carrying a letter can, and is therefore language-specific. */
  val rewritesWords: Boolean = replacement.exists(_.isLetter)

  /** True unless this is a CANONICAL word rewrite that forgot to name its
   *  countries — the shape that reaches every country's corpus and rewrites
   *  titles in a language they don't speak.
   *
   *  Twice now an unscoped Polish rewrite has leaked: `" & " → " i "` spelled a
   *  German film "Minions i Monster", and `^The Mandalorian and Grogu$` →
   *  "Mandalorian i Grogu" pinned the Berlin row to the Polish key
   *  `mandalorianigrogu`. Both were Canonical, and that is not a coincidence: the
   *  Canonical tier writes the `sanitize` MERGE KEY, so a foreign-language
   *  rewrite pins the row to a key no local cinema slot can ever produce — and
   *  since every re-key path runs through the same `sanitize`, nothing can
   *  rescue it. The damage is structural and self-perpetuating.
   *
   *  Deliberately NOT extended to the other tiers:
   *    - `GlobalStructural` shapes the TMDB SEARCH string. A bad rewrite there
   *      degrades one lookup and is retried on the next resolve — it never pins
   *      an identity. It also legitimately carries letters in language-neutral
   *      format rewrites (`(\d+)D` → `"$1 D"`, "Avatar 3D" → "Avatar 3 D"), so
   *      guarding it would disable real rules for no safety gain.
   *    - `PerCinema` is already bound by `cinemaId` to one venue in one country.
   *
   *  `countries = None` meaning "everywhere" stays the right default for the
   *  format-tag and bracketed-year strips that are most of the ~227 rules; only
   *  the one genuinely dangerous shape has to declare. */
  val countryScopeValid: Boolean =
    !rewritesWords || countries.nonEmpty || scope != RuleScope.Canonical

  /** True when the pattern is anchored to the START of the title (a `^`, after an
   *  optional leading inline-flag group like `(?i)`) — i.e. a prefix/banner rule
   *  such as the programme prefixes or the Cykl banner, as opposed to a `$`-anchored
   *  suffix strip. Drives `TitleRuleSet.leadingBannerBoundary` (display casing). */
  val isPrefixAnchored: Boolean =
    pattern.replaceFirst("""^\(\?[a-zA-Z]+\)""", "").startsWith("^")

  /** Apply this rule to `in`. No-op when disabled, when the pattern didn't
   *  compile, or when [[countryScopeValid]] is false — an unscoped Canonical word
   *  rewrite is inert EVERYWHERE rather than wrong everywhere, which is the
   *  fail-safe direction and the same degradation a malformed pattern gets. Code
   *  rules can't reach this (a spec fails the build); it exists for a rule typed
   *  into the admin editor, which no compiler sees.
   *
   *  The replace itself is also guarded: a `replacement` carrying a bare `$` or a
   *  trailing `\` is, to Java's `Matcher`, an illegal group reference and throws
   *  `IllegalArgumentException` (or `IndexOutOfBoundsException` for a `$N` past the
   *  group count). A malformed editor-entered replacement must degrade to a no-op,
   *  same as a malformed pattern — never throw inside the hot normalisation path
   *  or the admin "affected" preview (Sentry KINOWO-Y). */
  def apply(in: String): String = compiled match {
    case Some(re) if enabled && countryScopeValid =>
      try if (applyAll) re.replaceAllIn(in, replacement) else re.replaceFirstIn(in, replacement)
      catch { case _: IllegalArgumentException | _: IndexOutOfBoundsException => in }
    case _ => in
  }
}
