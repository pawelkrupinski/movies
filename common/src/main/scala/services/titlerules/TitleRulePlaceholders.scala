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

  /** A title-banner separator: colon, pipe, hyphen, en/em dash, slash or
   *  underscore, with optional surrounding whitespace. The consolidated form of
   *  the many hand-spelled `\s*[:|-]\s*` / `[|_]` / `[-–—]` variants the rules
   *  used to carry — one token now matches any of them. */
  val Separator = """\s*[:|/_–—-]\s*"""

  val all: Map[String, String] = Map(
    "SEP" -> Separator
  )
}
