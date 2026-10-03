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
                                  contradictions: Seq[String] = Nil)(val trace: DecisionTrace = DecisionTrace.Empty) extends Decision {
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
                            films: Map[Int, IdentityMeasures.Film],
                            /** How many listings — nodes alone, clusters pooled — the families scored
                             *  against their pools: the resolve's dominant cost. 0 where no family scored. */
                            scorings: Int = 0,
                            /** How many listing-film title relations the families read: once per pool film and
                             *  distinct titles a family bills, however many of its nodes and clusters bill them. */
                            titleRelations: Int = 0) {
  def families: Int = familyOf.values.toSet.size
  /** The partition over listing keys — what the order-independence properties compare. */
  lazy val partition: Set[Set[ListingKey]] = decisions.map(_.listings).toSet
  lazy val decisionOf: Map[ListingKey, ResolverDecision] = decisions.flatMap(d => d.members.map(_ -> d)).toMap
}

/** One constraint edge between two nodes (named by their smallest listing's sort key). */
final case class ResolverEdge(a: String, b: String, must: Boolean, tier: Int, reason: String)

/** Which rules decided a [[ResolverDecision]], as rule ids — the structured twin of its explanation, for the
 *  identity trace (`identity_traces`): every listing's rules, and every rule's listings. Outside the decision's
 *  equality (its second parameter list): two decisions alike are alike however they were traced.
 *  @param pooled  the rule the cluster's POOLED scoring accepted its film by, if that decided it
 *  @param vetoed  why the cluster's best candidate was denied, and the member listing whose own evidence denied it
 *  @param nodes   each member listing's own rules: the rule its node accepted a film by alone, the kinds of
 *                 must-links joining it to the cluster, and the cannot-links holding a neighbour apart */
final case class DecisionTrace(pooled: Option[String], vetoed: Option[DecisionTrace.Veto], nodes: Map[ListingKey, DecisionTrace.Node]) {
  /** `key`'s rule ids, by kind: `accept:`, `pooled:`, `veto:`, `join:`, `apart:`, and for a member no rule took alone,
   *  `refused:<rule>:<the first condition that stopped it>` for each rule. */
  def rulesOf(key: ListingKey): Seq[String] = {
    val node = nodes.get(key)
    (node.flatMap(_.accepted).map("accept:" + _) ++ pooled.map("pooled:" + _) ++ vetoed.map(v => "veto:" + DecisionTrace.id(v.reason)) ++
      node.toSeq.flatMap(_.joins.map("join:" + _)) ++ node.toSeq.flatMap(_.apart.map(reason => "apart:" + DecisionTrace.id(reason))) ++
      node.toSeq.flatMap(_.refusals.map(_.ruleId))).toSeq.distinct
  }
}

object DecisionTrace {
  /** `measures`: the node's own measures against the film its decision took (or its best candidate when it took
   *  none) — the same map its scoring holds, rendered into weights only when a trace is written. */
  final case class Node(accepted: Option[String], joins: Seq[String], apart: Seq[String],
                        measures: Map[String, IdentityMeasures.Measure] = Map.empty, candidate: Option[Int] = None,
                        refusals: Seq[Refusal] = Nil, searched: Seq[String] = Nil, candidates: Seq[String] = Nil,
                        blocker: Option[String] = None)
  /** Why `rule` refused a node no rule took alone: the condition that stopped it (`why`, a fixed phrase), the
   *  candidate it was weighing then, and what that candidate's evidence said ("p 26.6% < 40.0%", the facts against
   *  it, the rival it lost to). `ruleId` is the indexed `refused:<rule>:<why>`. */
  final case class Refusal(rule: String, why: String, film: Option[Int] = None, detail: String = "") {
    def ruleId: String = s"refused:$rule:${DecisionTrace.id(why)}"
  }
  final case class Veto(reason: String, by: Option[String])
  val Empty: DecisionTrace = DecisionTrace(None, None, Map.empty)

  /** A query as a trace shows it: `title "Manon (Ballet Live)"`, `director Polly Findlay`, `imdb "Manon"`. */
  def renderQuery(query: CandidateQuery): String = query match {
    case CandidateQuery.Title(text)    => s"title \"$text\""
    case CandidateQuery.Director(name) => s"director $name"
    case CandidateQuery.Imdb(title)    => s"imdb \"$title\""
  }
  /** A scored candidate as a trace shows it: `471328 2.4% rank 1 DENIED (…) 'BALLET LIVE. MANON. ROYAL ÓPERA HOUSE'`. */
  private[identity] def renderCandidate(scored: Scored): String =
    s"${scored.candidate.tmdbId} ${ResolverDecision.percent(scored.probability)} rank ${scored.rank.fold("-")(_.toString)}" +
      s"${scored.denial.fold("")(why => s" DENIED ($why)")} '${scored.candidate.film.title}'${scored.candidate.film.year.fold("")(year => s" ($year)")}"

  /** What stopped a node no rule took alone, as one id the next investigation can be ranked by: its searches found
   *  no candidate (`search:found-nothing`, or `search:unanswered` when a lookup went unanswered), every candidate
   *  was vetoed (`veto:<the best one's veto>`), or one stood and no rule took it (`rule:<why the calibrated rule
   *  refused it>` — below the cut, a runner-up its facts do not beat, a closer record). */
  private[identity] def blockerOf(own: Seq[Scored], unanswered: Boolean, refusals: Seq[Refusal]): String =
    if (own.isEmpty) if (unanswered) "search:unanswered" else "search:found-nothing"
    else if (own.forall(scored => scored.denied || scored.suggestedOnly))
      s"veto:${id(own.flatMap(_.denial).headOption.getOrElse("imdb-suggested only"))}"
    else refusals.find(_.rule == "favoured-calibrated").orElse(refusals.headOption).fold("rule:none")(refusal => s"rule:${id(refusal.why)}")
  /** A reason as a rule id: "Learned(runtime.delta >= 11 AND title in {none,overlap})" → "learned-runtime-delta-11-and-title-in-none-overlap". */
  def id(reason: String): String = reason.toLowerCase(java.util.Locale.ROOT).replaceAll("[^\\p{L}\\p{N}]+", "-").stripPrefix("-").stripSuffix("-")
}
