package services.titlerules

/** Hardcoded, reusable regex snippets that rule patterns reference as
 *  `{{NAME}}`. A rule writes `{{SEP}}` instead of spelling out a separator
 *  class, so the separator language is defined in ONE place and generalising it
 *  reshapes every rule that uses it. Expanded into each rule's pattern by
 *  [[PlaceholderExpander]] before the regex compiles (see
 *  [[TitleRuleSet.effectiveRules]]).
 *
 *  These are part of the rule language, not editable data — change them here and
 *  redeploy. The admin editor shows them as a read-only reference and expands
 *  them when validating a pattern, but can't edit them. */
object TitleRulePlaceholders {

  /** The banner-separator characters: colon, pipe, slash, underscore, backslash,
   *  en-dash, em-dash, hyphen. (Hyphen last so it's literal in the class.) */
  private val SepChars = """:|/_\\–—-"""

  /** A title-banner separator with optional surrounding whitespace — the
   *  consolidated form of the many hand-spelled `\s*[:|-]\s*` / `[|_]` / `[-–—]`
   *  variants the rules used to carry. One token now matches any of them. */
  val Separator = s"""\\s*[$SepChars]\\s*"""

  /** Like [[Separator]] but also accepting a period — for the few suffixes that
   *  historically allowed a `.` delimiter ("Movie. 40th anniversary",
   *  "Film. wersja polska", "przedpremiera. Film"). */
  val SeparatorOrDot = s"""\\s*[.$SepChars]\\s*"""

  /** One character that is NOT a separator — the building block for a
   *  banner-name guard like `^DKF\s+{{NSEP}}+{{SEP}}` ("DKF <name>: <film>"),
   *  where the guard must stop at the first separator instead of backtracking
   *  across one into the film title. Spaces are allowed (a name has spaces).
   *
   *  Narrower than [[Separator]] on purpose: it excludes only the HARD
   *  separators (`: | / \ – — -`), NOT `_`, because a cycle name can be
   *  underscore-glued ("„GAG"_SENIOR") and the guard must eat that, not stop on
   *  it — even though `_` is a strip separator elsewhere ("_DKF"). */
  private val GuardSepChars = """:|/\\–—-"""
  val NonSeparator = s"""[^$GuardSepChars]"""

  val all: Map[String, String] = Map(
    "SEP"  -> Separator,
    "SEPD" -> SeparatorOrDot,
    "NSEP" -> NonSeparator
  )
}
