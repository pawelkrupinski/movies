package services.identity

import services.movies.ListingKey

/**
 * One cluster the identity resolver decided: which listings are one film, which film, how sure
 * it is, and why (docs/design/identity-resolver.md, "Pure two-stage resolution").
 *
 * RECONCILE ON MERGE: the resolver branch (`identity-resolver`) owns this type. This is the
 * minimal shape the curation side reads (the admin view and the rating gate), defined here only
 * because the resolver's had not landed when curation was built. When it lands, its Decision
 * replaces this trait (or extends it) and [[ShadowDecisions]] reads the resolver's shadow output.
 */
trait Decision {
  /** The cluster: every listing the resolver put in this film. */
  def listings: Set[ListingKey]
  /** The film the cluster resolved to, if any. */
  def tmdbId: Option[Int]
  /** How sure the resolver is, in [0, 1]. Only its ORDER matters to curation: the rating gate's
   *  threshold is calibrated from data (see [[ConfidenceCalibration]]), never read as a
   *  probability. */
  def confidence: Double
  /** Why, in the resolver's own words: the edges and lookups that decided the cluster. */
  def explanation: Seq[String]
  /** Constraint pressure on the cluster: each must-link the solver refused because a
   *  cannot-link held the two sides apart, or an ambiguous node it left alone. Empty for an
   *  uncontested cluster. */
  def contradictions: Seq[String]
}

/** Where curation reads the resolver's latest decisions and its labelled shadow diff. The
 *  resolver's shadow run writes both; until it lands, [[ShadowDecisions.none]] is wired and
 *  every consumer degrades to "nothing decided" (the admin view lists nothing, the rating gate
 *  withholds nothing). */
trait ShadowDecisions {
  /** Every decision of the latest shadow resolve. */
  def latest(): Seq[Decision]
  /** The latest shadow diff's decisions whose correctness is known — the pipeline agreeing, or
   *  a reviewed known-issues verdict — as the calibration input of the rating gate. */
  def verdicts(): Seq[ConfidenceCalibration.Sample]
}

object ShadowDecisions {
  val none: ShadowDecisions = new ShadowDecisions {
    def latest(): Seq[Decision]                         = Nil
    def verdicts(): Seq[ConfidenceCalibration.Sample] = Nil
  }
}
