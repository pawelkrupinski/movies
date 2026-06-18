package services.titlerules

/** Hardcoded, reusable regex snippets that rule patterns reference as
 *  `{{NAME}}`. A rule writes `{{SEP}}` instead of spelling out a separator
 *  class, so the separator language is defined in ONE place and generalising it
 *  reshapes every rule that uses it. Expanded into each rule's pattern by
 *  [[PlaceholderExpander]] before the regex compiles (see
 *  [[TitleRuleSet.effectiveRules]]).
 *
 *  These are part of the rule language, not editable data — change them here and
 *  redeploy. The admin editor shows [[defined]] as a read-only, documented
 *  reference and expands the tokens when validating a pattern, but can't edit
 *  them. */
object TitleRulePlaceholders {

  /** The banner-separator characters: colon, pipe, slash, underscore, backslash,
   *  en-dash, em-dash, hyphen. (Hyphen last so it's literal in the class.) */
  private val SepChars = """:|/_\\–—-"""

  /** Like [[SepChars]] but without `_` — the guard set. A banner-name guard must
   *  eat an underscore-glued name ("„GAG"_SENIOR") yet stop at a hard separator. */
  private val GuardSepChars = """:|/\\–—-"""

  /** A placeholder: the `name` referenced as `{{name}}`, its regex `expansion`,
   *  and a human `doc` line shown in the admin editor's reference panel. */
  final case class Placeholder(name: String, expansion: String, doc: String)

  /** Every placeholder, in display order. [[all]] is derived from this. */
  val defined: Seq[Placeholder] = Seq(
    Placeholder("SEP", s"""\\s*[$SepChars]\\s*""",
      "A banner separator — colon, pipe, hyphen, en/em dash, slash, underscore or backslash, " +
      "with optional surrounding whitespace. Use it wherever a rule separates a banner from the " +
      "film title; one token matches every separator the old hand-spelled classes did, so a rule " +
      "written once works for ': ', ' | ', ' - ', '–', '/', '_' alike."),
    Placeholder("SEPD", s"""\\s*[.$SepChars]\\s*""",
      "Like SEP, but also matches a period. For the few suffixes that historically allowed a '.' " +
      "delimiter — anniversary / restored / 'wersja' / the przedpremiera prefix " +
      "('przedpremiera. Film')."),
    Placeholder("NSEP", s"""[^$GuardSepChars]""",
      "One character that is NOT a separator — the building block for a banner-name guard like " +
      "^DKF\\s+{{NSEP}}+{{SEP}} (\"DKF <name>: <film>\"), so the guard stops at the first separator " +
      "instead of backtracking across one into the film title. Narrower than SEP: it excludes the " +
      "hard separators but keeps '_', so an underscore-glued cycle name is still consumed.")
  )

  /** name → expansion, consumed by [[PlaceholderExpander]] / [[TitleRuleSet]]. */
  val all: Map[String, String] = defined.map(p => p.name -> p.expansion).toMap
}
