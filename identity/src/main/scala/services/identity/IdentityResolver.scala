package services.identity

import models.ListingKey
import services.identity.measures.{EvidenceNode, Film, Listing, Measure, Resolution, ResolverDecision}
import services.identity.vocabulary.{CandidateQuery, PinConstraints, TitleDecorations}
import services.titles.TitleForms

/**
 * `resolve(E)`: film identity as a pure, deterministic function of a SET of listings and the
 * answers of a lookup source (docs/design/identity-resolver.md, phase 2). The worker's model resolves
 * through it region by region ([[IncrementalResolver]] over [[resolveRegion]]); the recording sweep and curation read it whole.
 *
 * Two stages, every step a function of the set:
 *
 *   A. CANDIDATE GENERATION ([[CandidateGeneration]]). Each listing's own detail page merges into
 *      its [[Evidence]]; listings with identical evidence are one NODE. Every node's
 *      [[CandidateQueries]] are asked — all of them, up front, in sorted order, none conditional on
 *      another's answer (A1) — and every film any answer names is looked up once. The nodes are
 *      grouped into FAMILIES ([[Families]]), the block closure of `FamilyClosure` over their title
 *      keys and the films they match.
 *
 *   B. GLOBAL ASSIGNMENT. Every node scores every candidate it has an EVIDENCE PATH to (its own
 *      query named it, or a film title relates to its title) with the calibrated model
 *      ([[IdentityCalibration]] over [[IdentityMeasures]] — the same measurements the weights were
 *      fitted on, including `venues.corroborating`, the family's venue co-occurrence; [[FamilyScope]]).
 *      A candidate a node's evidence DENIES (`LearnedCannotLinks.learnedListingFilm`: a learned
 *      rule, or its OWN facts' probability below the certified cut, and only when the node
 *      publishes a fact the film can be compared on — a title relation alone is scored, never a
 *      veto; or a film of the node's credited director its title does not name, when its title
 *      names another of that director's) is not eligible. A node ACCEPTS a candidate ALONE by the
 *      rules of [[Acceptance]]; otherwise it follows its cluster. Then:
 *        1. constraint edges between nodes sharing a block key ([[ConstraintEdges]]), solved by
 *           [[ConstraintSolver]] (cannot wins, and no component holds two films however the
 *           must-links chain; an ambiguous node stays alone, A2);
 *        2. GROUP-LEVEL VOTING ([[ClusterVoting]]): a cluster no member of which accepted a film
 *           scores its members' evidence POOLED into one listing (the heaviest title, the modal
 *           year, every director), and the winner, if accepted (`Acceptance.pooled`), becomes
 *           every member's film. A winner some members' own evidence denies is not a veto of the
 *           whole cluster: those members split off and the rest take it when their own pooled
 *           facts carry it. A winner no member's title names — only a credited director's
 *           filmography reached it — must also be the one film the pooled facts and the
 *           calibration both rank first;
 *        3. the constraints are re-solved with those films, and each final cluster is a
 *           [[ResolverDecision]] with its confidence, basis and explanation ([[ResolverDecisions]]).
 *
 * Every family is resolved on its own. That is sound only while no edge crosses a family, which
 * holds by construction — every edge joins two nodes sharing a block key — and is CHECKED: a
 * crossing edge fails the resolve rather than scoping it.
 *
 * THE RUN'S STAGES, each behind its own class, each owning its steps; the orchestrators read the stages, never a stage's
 * steps:
 *  - CANDIDATES ([[ResolveStages]], stage A): candidate generation, scoring and families, wired once;
 *  - CLUSTERING ([[Clustering]]): a family's constraint edges and the solver's clusters -- round A over the films its
 *    nodes took alone, the final round over those and the votes';
 *  - VOTING ([[ClusterVoting.votes]]): the films the clusters no member took a film alone in vote for;
 *  - DECIDING ([[Deciding]]): each final cluster's decision, its bills, pieces and uncorroborated members apart.
 * [[ResolverRun]] sequences them per family (clustering, voting, clustering, deciding) and builds the [[Resolution]];
 * this object runs stage A then the run ([[resolve]], [[resolveRegion]] for the incremental model). The reports read the
 * stages through their own types' companions ([[ListingExplanation.of]], [[FamilyAnatomy.largest]], [[NodeCandidates.of]]).
 *
 * `Mutation` is for the teeth tests only: each variant breaks exactly one of the properties the
 * specs prove, and the specs must catch it.
 */
object IdentityResolver {

  private[identity] enum Mutation {
    case None
    /** A2 broken: must-links applied in arrival order, first wins. */
    case FirstWins
    /** A1 broken: listings walked in arrival order, a title query whose sanitised form an
     *  earlier listing already asked is skipped — the old "sister-row shortcut". */
    case LazyLookups
    /** Group-level voting off: a cluster with no own match stays unmatched. */
    case NoVoting
    /** Families narrowed to the sanitised title: an edge through an original title, a search
     *  form or a shared film then crosses a family and the resolve must refuse. */
    case NarrowFamilies
  }

  /** Thrown when `count` edges cross a family: a rule was added without its block key. */
  final class FamilyCrossing(val count: Int, message: String) extends IllegalStateException(message)

  /** The whole resolve of `listings` over `inputs` ([[ResolveInputs]], no default: every caller says whose pins it
   *  reads). Its pins are the curation's hard constraints (`PinConstraints`): a pinned film replaces a listing's own
   *  match, a denied one is never eligible, a pinned group is must-linked above every derived tier, and a derived edge
   *  the pins contradict is dropped. */
  def resolve(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleForms, inputs: ResolveInputs): Resolution =
    resolveWith(listings, lookups, normalizer, inputs.calibration, Mutation.None, inputs.pins, inputs.decorations)

  /** One candidate as a node's family scored it: the calibrated probability, the search rank, whether the node's own
   *  evidence denies it, whether the title names it, and whether its record is the listing's season or house production
   *  — the model's side of the unified evidence model's signals ([[UnifiedEvidence]]) — the measures it was scored
   *  on, which a refit of the calibration counts (`scripts.IdentityRefit`), and, a namesake the release veto took out
   *  ([[ReleaseVeto]]), the namesake it stood beside that TMDB dates a release of in the venue's country. */
  final case class CandidateEvidence(tmdbId: Int, film: Film, probability: Double, rank: Option[Int], denied: Boolean,
                                     titleNamesIt: Boolean, seasonProduction: Boolean, houseProduction: Boolean,
                                     measures: Map[String, Measure] = Map.empty, releasedBeside: Option[Int] = None)
  /** A node's listings and its candidates as its family scored them (best first). */
  final case class NodeEvidence(keys: Seq[ListingKey], candidates: Seq[CandidateEvidence])

  /** Every node holding a listing `focused` selects, its candidates scored as the resolve scores them. */
  def evidenceOf(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleForms,
                 calibration: IdentityCalibration = IdentityCalibration.resolver,
                 decorations: TitleDecorations = TitleDecorations.resolver)(focused: Listing => Boolean): Seq[NodeEvidence] =
    evidenceIn(new ResolveStages(listings, lookups, normalizer, calibration, Mutation.None, PinConstraints(Nil), decorations))(focused)

  private def evidenceIn(stages: ResolveStages)(focused: Listing => Boolean): Seq[NodeEvidence] =
    focusedScored(stages)(focused).map { case (node, scored) =>
      NodeEvidence(node.listings.map(_.key), scored.map(s => CandidateEvidence(s.candidate.tmdbId, s.candidate.film, s.probability, s.rank, s.denied,
        s.titleNamesIt, s.seasonProduction, s.houseProduction, s.measures, s.denial.flatMap(_.releasedBeside))))
    }

  /** Every node holding a listing `focused` selects, with its candidates as its family scores them (best first). */
  private[identity] def focusedScored(stages: ResolveStages)(focused: Listing => Boolean): Seq[(EvidenceNode, Seq[services.identity.measures.Scored])] =
    stages.generation.nodes.filter(_.listings.exists(focused)).map(node => node -> stages.families.scoredOf(node))

  /** A [[resolve]], and every node's candidates as that same resolve scored them ([[evidenceOf]], read only when asked):
   *  one pass for a caller wanting both — building the stages a second time was a whole second resolve. A family's verdict
   *  (`agreement.Agreement.verdict`, its only caller) takes a film one piece of the billing names ([[NoFilmMembers]]): what
   *  the families agree on is the agreement stage's to read against the billing (`AgreementStage.pieceOf`), which also keeps
   *  a fill off it. */
  final class Scored private[IdentityResolver] (val resolution: Resolution, stages: ResolveStages) {
    lazy val evidence: Seq[NodeEvidence] = evidenceIn(stages)(_ => true)
  }
  def resolveScored(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleForms,
                    calibration: IdentityCalibration = IdentityCalibration.resolver): Scored = {
    val stages = new ResolveStages(listings, lookups, normalizer, calibration, Mutation.None, PinConstraints(Nil), TitleDecorations.resolver)
    new Scored(run(stages, Mutation.None, pieces = false), stages)
  }

  /** The corpus-wide facts a resolve of `listings` reads ([[CorpusContext]]): what one family,
   *  resolved alone with `corpus = Some(...)`, needs to decide as the whole resolve does. */
  private[identity] def contextOf(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleForms,
                                  pins: PinConstraints = PinConstraints(Nil),
                                  decorations: TitleDecorations = TitleDecorations.None): CorpusContext =
    new CandidateGeneration(listings.toSeq.sorted.distinctBy(_.key), lookups, normalizer, pins, decorations, lazyLookups = false).context

  /** [[contextOf]] under `inputs`' pins and decorations. */
  private[identity] def contextOf(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleForms,
                                  inputs: ResolveInputs): CorpusContext =
    contextOf(listings, lookups, normalizer, inputs.pins, inputs.decorations)

  private[identity] def resolveWith(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleForms,
                                    calibration: IdentityCalibration, mutation: Mutation,
                                    pins: PinConstraints = PinConstraints(Nil),
                                    decorations: TitleDecorations = TitleDecorations.None,
                                    corpus: Option[CorpusContext] = None): Resolution = {
    run(new ResolveStages(listings, lookups, normalizer, calibration, mutation, pins, decorations, corpus), mutation)
  }

  /** One family of a region's resolve ([[resolveRegion]]): its listings, its decisions, the keys it
   *  blocks under (its title keys and the films its members matched — what another family must share
   *  to merge with it), the questions and records its answers came from, and the corpus-wide facts
   *  it could read ([[CorpusContext.Reads]]). */
  private[identity] final case class RegionFamily(listings: Set[ListingKey], decisions: Seq[ResolverDecision], blockKeys: Set[String],
                                                  queries: Set[CandidateQuery], films: Set[Int], reads: CorpusContext.Reads,
                                                  nodeKeys: Map[ListingKey, String]) {
    /** This family naming each listing by `keyOf`'s object for its key and each node by `textOf`'s for its text — equal
     *  values, so it decides and stores exactly as before, and is built anew only where some object differs. A family
     *  decoded from the store names its listings by keys of its own; kept so, worker-us held two more key objects per
     *  listing (its family's and its decision's) and a node text per node beside the corpus's, for the model's life. */
    def sharing(keyOf: ListingKey => ListingKey, textOf: (ListingKey, String) => String): RegionFamily = {
      val keysShared  = listings.forall(key => keyOf(key) eq key)
      val decided     = decisions.forall(_.members.forall(key => keyOf(key) eq key))
      val nodesShared = nodeKeys.forall { case (key, text) => (keyOf(key) eq key) && (textOf(key, text) eq text) }
      if (keysShared && decided && nodesShared) this
      else copy(
        listings  = RegionFamily.keptOr(keysShared, listings)(listings.map(keyOf)),
        decisions = RegionFamily.keptOr(decided, decisions)(decisions.map(decision => decision.copy(members = decision.members.map(keyOf))(decision.trace))),
        nodeKeys  = RegionFamily.keptOr(nodesShared, nodeKeys)(nodeKeys.map { case (key, text) => keyOf(key) -> textOf(key, text) }))
    }
  }
  private[identity] object RegionFamily {
    /** `kept` where it already shares the corpus's objects, else `rebuilt`. */
    private def keptOr[A](shared: Boolean, kept: A)(rebuilt: => A): A = if (shared) kept else rebuilt
  }

  /** Resolve `listings` — a union of whole families — against the corpus's `corpus` context, family
   *  by family: A3 with the corpus's facts, so each decides as the whole resolve would. */
  private[identity] def resolveRegion(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleForms,
                                      calibration: IdentityCalibration, pins: PinConstraints, decorations: TitleDecorations,
                                      corpus: CorpusContext): Seq[RegionFamily] = {
    val stages     = new ResolveStages(listings, lookups, normalizer, calibration, Mutation.None, pins, decorations, Some(corpus))
    val resolution = run(stages, Mutation.None)
    import stages.{families, generation}
    // Each decision is one family's: filed by its first member's, not filtered out of every decision per family.
    val decisionsOf = resolution.decisions.groupBy(decision => resolution.familyOf(decision.members.head))
    generation.nodes.groupBy(node => families.familyOf(node.id)).toSeq.sortBy(_._1).map { case (family, members) =>
      val keys  = members.flatMap(_.listings.map(_.key)).toSet
      val pool  = families.scopes(family).pool
      val queries = members.flatMap(node => generation.queriesOf(node.id)).toSet
      val films = members.flatMap(node => generation.hitsOf(node.id).map(_.tmdbId)).toSet
      val decisions = decisionsOf.getOrElse(family, Nil)
      RegionFamily(keys, decisions, members.flatMap(node => families.blockKeysOf(node.id)).toSet, queries, films,
        CorpusContext.Reads.of(members, pool.map(_.film), films ++ pool.map(_.tmdbId) ++ decisions.flatMap(_.film), queries, normalizer.sanitize),
        members.flatMap(node => node.listings.map(listing =>
          listing.key -> CandidateGeneration.nodeKeyText(CandidateGeneration.nodeKey(listing, node.evidence, pins)))).toMap)
    }
  }

  /** [[resolveRegion]] over `inputs`' calibration, pins and decorations. */
  private[identity] def resolveRegion(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleForms,
                                      inputs: ResolveInputs, corpus: CorpusContext): Seq[RegionFamily] =
    resolveRegion(listings, lookups, normalizer, inputs.calibration, inputs.pins, inputs.decorations, corpus)

  /** `pieces`: whether a member billing its cluster's film as one piece beside another thing is decided apart, as no film
   *  ([[billedAsPiece]]) — the model's own takes; not a family's verdict ([[resolveScored]]). */
  private[identity] def run(stages: ResolveStages, mutation: Mutation, pieces: Boolean = true): Resolution =
    new ResolverRun(stages, mutation, pieces).resolution
}
