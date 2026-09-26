package services.identity

import services.movies.ListingKey

/**
 * One cluster the identity resolver decided: which listings are one film, which film, how sure
 * it is, and why (docs/design/identity-resolver.md, "Pure two-stage resolution").
 *
 * The shape curation reads (the admin view). The resolver's own verdict,
 * [[ResolverDecision]], implements it; the shadow run persists them ([[ShadowRunStore]]), and the
 * admin view reads them back through [[ShadowDecisions]].
 */
trait Decision {
  /** The cluster: every listing the resolver put in this film. */
  def listings: Set[ListingKey]
  /** The film the cluster resolved to, if any. */
  def tmdbId: Option[Int]
  /** How sure the resolver is, in [0, 1]. Only its ORDER matters to curation: the admin view's
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
 *  - `confidence` is the probability, under the calibrated [[IdentityCalibration]], that the verdict
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

  /** How the verdict was reached. The unmatched bases say WHY no film was accepted. */
  enum Basis {
    /** A curation pin named the film (confidence 1). */
    case Pinned
    /** At least one member's own evidence accepted the film. */
    case OwnMatch
    /** No member accepted a film alone; the cluster's POOLED evidence did (group-level voting). */
    case PooledMatch
    /** Unmatched: no candidate at all — every query answered, none named a film this evidence reaches. */
    case NoCandidate
    /** Unmatched: no candidate, and some of the cluster's queries could not be answered. */
    case NoEvidence
    /** Unmatched: the most likely candidate is denied by a member's own evidence (a cannot-link). */
    case Vetoed
    /** Unmatched: candidates were scored, and none reached the acceptance cut. */
    case BelowThreshold

    def matched: Boolean = this == Pinned || this == OwnMatch || this == PooledMatch
  }

  def percent(p: Double): String = f"${p * 100}%.1f%%"
}

/** A resolve's result over a listing set. `violations` counts cannot-linked pairs inside one
 *  cluster (P3; zero by construction — a non-zero count is a solver bug). A resolve with an edge
 *  between two families never returns one: it throws `IdentityResolver.FamilyCrossing`. `queries`
 *  are the candidate queries in the order they were issued, `filmLookups` the films looked up,
 *  `films` what the decided films are. */
final case class Resolution(decisions: Seq[ResolverDecision], nodes: Int, familyOf: Map[ListingKey, Int],
                            edges: Seq[ResolverEdge], queries: Seq[CandidateQuery], filmLookups: Int,
                            unknownQueries: Int, unknownDetails: Int, unknownFilms: Int, violations: Int,
                            films: Map[Int, IdentityMeasures.Film]) {
  def families: Int = familyOf.values.toSet.size
  /** The partition over listing keys — what the order-independence properties compare. */
  lazy val partition: Set[Set[ListingKey]] = decisions.map(_.listings).toSet
  lazy val decisionOf: Map[ListingKey, ResolverDecision] = decisions.flatMap(d => d.members.map(_ -> d)).toMap
}

/** One constraint edge between two nodes (named by their smallest listing's sort key). */
final case class ResolverEdge(a: String, b: String, must: Boolean, tier: Int, reason: String)
