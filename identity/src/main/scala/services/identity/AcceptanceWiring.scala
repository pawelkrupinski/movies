package services.identity

/** The acceptance's RULE GRAPH, each piece made once over the evidence `weights`: the questions of every kind — the
 *  title's, the bill's, the vetoes', the facts', the director's, the calibrated rules', the stage relays' —, the steps
 *  the rows share, the rows of each kind, the table that orders them, and what a node no rule took falls back or leans
 *  to. The composition root of [[Acceptance]]: it wires, and decides nothing. */
private[identity] final class AcceptanceWiring(weights: EvidenceWeights) {
  val titleRules: AcceptanceTitleRules = new AcceptanceTitleRules
  val billRules: AcceptanceBillRules   = new AcceptanceBillRules
  val vetoRules: AcceptanceVetoRules   = new AcceptanceVetoRules
  private val factRules       = new AcceptanceFactRules
  private val directorRules   = new AcceptanceDirectorRules(factRules)
  private val calibratedRules = new AcceptanceCalibratedRules(weights)
  private val steps           = new AcceptanceRuleSteps(factRules)
  val stageRows: AcceptanceStageRows = new AcceptanceStageRows(new AcceptanceStageRules(factRules), factRules, steps, weights)
  val table: AcceptanceRuleTable = new AcceptanceRuleTable(new AcceptanceTitleRows(titleRules, steps),
    new AcceptanceFactRows(factRules, directorRules, calibratedRules, steps), stageRows, vetoRules, steps)
  val fallbacks: AcceptanceFallbacks = new AcceptanceFallbacks(weights, billRules, factRules, calibratedRules)
}
