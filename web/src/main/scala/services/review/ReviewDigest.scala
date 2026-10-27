package services.review

/** Who judged a row of `identity-judgements.tsv`, read from its evidence: the user, or an agent the user has not answered. */
object ReviewDigest {

  /** The marker an agent's judgement carries in its evidence: `AGENT-JUDGED` (the skill's spelling), and the spellings
   *  agents used before it — "agent judged 2026-10-22 (…)", "judged 10-22 (agent, …)". */
  private val AgentMarker = """(?i)\bagent[- ]judged\b|\(agent[,)]""".r

  /** The user's own word on a row: the review answers' export, or a hand label. */
  def userJudged(evidence: String): Boolean = evidence.startsWith("review page:") || evidence.startsWith("hand label:")

  /** An agent's judgement the user has not confirmed or overruled yet. */
  def agentJudged(evidence: String): Boolean = AgentMarker.findFirstIn(evidence).isDefined && !userJudged(evidence)
}
