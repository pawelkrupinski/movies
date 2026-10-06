package services.review

import services.identity.{ListingTrace, ResolverDecision}

/** What a decision stored beside its explanation: the facts its members contradicted each other on, the catalogue it
 *  fell back to, each film-database family's agreement-stage answer (family → the id it named) and how many of the
 *  families it asked left the question unanswered. */
final case class ReviewReasons(contradictions: Seq[String] = Nil, fallback: Option[ResolverDecision.Fallback] = None,
                               agreed: Map[String, String] = Map.empty, unanswered: Int = 0)

object ReviewReasons {
  def of(decision: ResolverDecision): ReviewReasons =
    ReviewReasons(decision.contradictions, decision.fallback, decision.agreed, decision.unanswered)
}

/** One weighed measure of a listing against its film, as the trace writes it — `director=same_person +4.22`: the
 *  feature and its bucket, and the log-odds it added (for) or took away (against). */
final case class Contribution(feature: String, weight: Double, text: String) {
  def isFor: Boolean = weight > 0
}

object Contribution {
  private val Shape = """(.*\S)\s+([+-]\d+(?:\.\d+)?)""".r

  /** A trace's evidence line; one of another shape is kept whole, as a contribution of no weight. */
  def parse(line: String): Contribution = line.trim match {
    case Shape(feature, weight) => Contribution(feature, weight.toDouble, line.trim)
    case other                  => Contribution(other, 0.0, other)
  }
}

/** One member listing's trace as the "Why" fold-out shows it: every contribution, split for and against (strongest
 *  first), and the rest of the trace whole. */
final case class ListingReasons(trace: ListingTrace) {
  private val contributions = trace.evidence.map(Contribution.parse)
  def forIt: Seq[Contribution]   = contributions.filter(_.weight > 0).sortBy(-_.weight)
  def against: Seq[Contribution] = contributions.filter(_.weight < 0).sortBy(_.weight)
  def neutral: Seq[Contribution] = contributions.filter(_.weight == 0)
  /** A refusal that names a denial or a veto — a guard that blocked the film, not a rule that merely didn't fire. */
  def isVeto(refusal: services.identity.DecisionTrace.Refusal): Boolean =
    refusal.detail.contains("denied") || refusal.why.contains("veto") || refusal.rule.contains("veto")
  /** The rule ids that took, joined, set apart or vetoed — the refusals are listed whole on their own. */
  def rulesApplied: Seq[String] = trace.rules.filterNot(_.startsWith("refused:"))
}
