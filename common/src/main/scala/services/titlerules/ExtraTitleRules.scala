package services.titlerules

/** Every title rule added after the baseline ([[TitleRules]]): the title-rule table's rows not flagged `seed` —
 *  programme banners, query strips, merge-key unifications and venue cleanups (see [[TitleRuleTable]] for the kinds).
 *  Regression-tested by `ExtraTitleRulesSpec` and, on the whole corpus, `TitleRulesGoldenSpec`. */
object ExtraTitleRules {

  /** One decoration the weekly discovery bot (`scripts.DecorationDiscovery`, `.github/workflows/decoration-discovery.yml`)
   *  proposed: a search strip measured on the unmatched clusters' fixture — at least one listing taken right, none wrong,
   *  no existing take switched, and no known film title starting (prefix) or ending (suffix) with what it strips — and the
   *  venue titles it was measured on, each with the search title it now gives. `DiscoveredDecorationsSpec` holds every
   *  rule to its examples and checks none of them comes out so without it. */
  final case class Discovered(rule: TitleRule, examples: Seq[(String, String)])

  /** The discovery bot's accepted proposals: the table's rows carrying the examples they were measured on. */
  val discovered: Seq[Discovered] = TitleRuleTable.rows.collect { case row if row.examples.nonEmpty => Discovered(row.rule, row.examples) }

  val all: Seq[TitleRule] = TitleRuleTable.rows.filterNot(_.seed).map(_.rule)
}
