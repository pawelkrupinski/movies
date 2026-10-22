package services.titlerules

/** The baseline rule set — every formerly-hardcoded `TitleNormalizer` regex, transcribed verbatim: the rows of the
 *  title-rule table ([[TitleRuleTable]]) flagged `seed`.
 *
 *  `ProdTitlesNormalizationSpec` pins this set byte-for-byte against the pre-rules code on a prod-title snapshot. New
 *  normalisation behaviour goes in new rows, never in a seed row. */
object TitleRules {

  val all: Seq[TitleRule] = TitleRuleTable.rows.filter(_.seed).map(_.rule)

  val ruleSet: TitleRuleSet = TitleRuleSet(all)
}
