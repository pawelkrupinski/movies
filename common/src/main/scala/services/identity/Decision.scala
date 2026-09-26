package services.identity

import services.movies.ListingKey

/**
 * One cluster the identity resolver decided: which listings are one film, which film, how sure
 * it is, and why (docs/design/identity-resolver.md, "Pure two-stage resolution").
 *
 * The shape curation reads (the admin view and the rating gate). The resolver's own verdict,
 * [[ResolverDecision]], implements it; [[ShadowDecisions]] is still `none` in both wirings until a
 * shadow run's decisions are persisted for it to read.
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

/**
 * The resolver's verdict on one CLUSTER (`IdentityResolver`): which listings are one film, which
 * film that is (if any), how sure it is, and why.
 *
 *  - `film` is the TMDB id the cluster was matched to, `None` when no candidate was accepted —
 *    which is a verdict too ("a film the database does not know, or one the evidence cannot
 *    pick"), not a failure.
 *  - `confidence` is the probability, under the calibrated [[IdentityWeights]], that the verdict
 *    is right: for a match, that `film` is the cluster's film and no rival is; for no match, that
 *    none of the candidates is.
 *  - `explanation` is the evidence it rests on, in order: each member's own best match, what
 *    joined the members, what was kept apart, and the lookups that could not be answered;
 *    `contradictions` the cannot-links that held a neighbour apart.
 */
final case class ResolverDecision(members: Seq[ListingKey], film: Option[Int], confidence: Double,
                                  basis: ResolverDecision.Basis, explanation: Seq[String],
                                  contradictions: Seq[String] = Nil) extends Decision {
  lazy val listings: Set[ListingKey] = members.toSet
  def tmdbId: Option[Int]            = film
  def render: String =
    s"${film.fold("no film")(id => s"tmdb $id")} (${ResolverDecision.percent(confidence)}, $basis) — " +
      s"${members.size} listing(s)\n    " + explanation.mkString("\n    ")
}

object ResolverDecision {

  /** How the verdict was reached. */
  enum Basis {
    /** A curation pin named the film (confidence 1). */
    case Pinned
    /** At least one member's own evidence accepted the film. */
    case OwnMatch
    /** No member accepted a film alone; the cluster's POOLED evidence did (group-level voting). */
    case PooledMatch
    /** No candidate reached the threshold. */
    case NoMatch
    /** Every lookup the cluster needed was unanswerable: the verdict has nothing to rest on. */
    case NoEvidence
  }

  def percent(p: Double): String = f"${p * 100}%.1f%%"
}

/** A resolve's result over a listing set. `violations` counts cannot-linked pairs inside one
 *  cluster (P3; zero by construction — a non-zero count is a solver bug). A resolve with an edge
 *  between two families never returns one: it throws `IdentityResolver.FamilyCrossing`. `queries`
 *  are the candidate queries in the order they were issued, `filmLookups` the films looked up. */
final case class Resolution(decisions: Seq[ResolverDecision], nodes: Int, familyOf: Map[ListingKey, Int],
                            edges: Seq[ResolverEdge], queries: Seq[CandidateQuery], filmLookups: Int,
                            unknownQueries: Int, unknownDetails: Int, violations: Int) {
  def families: Int = familyOf.values.toSet.size
  /** The partition over listing keys — what the order-independence properties compare. */
  lazy val partition: Set[Set[ListingKey]] = decisions.map(_.listings).toSet
  lazy val decisionOf: Map[ListingKey, ResolverDecision] = decisions.flatMap(d => d.members.map(_ -> d)).toMap
}

/** One constraint edge between two nodes (named by their smallest listing's sort key). */
final case class ResolverEdge(a: String, b: String, must: Boolean, tier: Int, reason: String)

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
