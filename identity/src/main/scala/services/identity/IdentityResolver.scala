package services.identity

import models.ListingKey

import services.titles.TitleNormalizer

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
  def resolve(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer, inputs: ResolveInputs): Resolution =
    resolveWith(listings, lookups, normalizer, inputs.calibration, Mutation.None, inputs.pins, inputs.decorations)

  /** Why a listing resolved as it did and sits where it does — the resolve's own stages, told:
   *  its node; every family key it has and why, and every title piece it does not block under and
   *  why not; the chain of shared keys linking it to its family's heaviest member (why THIS
   *  family); its decision with the decision's own explanation; and its candidates as its family
   *  scored them. */
  final case class ListingExplanation(node: String, family: Int, familyListings: Int, keys: Seq[(String, String)],
                                      dropped: Seq[(String, String)], chain: Seq[(String, String)], decision: ResolverDecision,
                                      candidates: Seq[ResolveReports.CandidateScore]) {
    def render: Seq[String] =
      Seq(s"listing in node $node, family $family ($familyListings listing(s))",
          s"  decision: ${decision.render}") ++
        keys.map { case (key, why) => s"  key $key — $why" } ++
        dropped.map { case (piece, why) => s"  not a key: $piece — $why" } ++
        (if (chain.isEmpty) Seq("  the family's heaviest node itself") else s"  in this family by:" +: chain.map { case (key, next) => s"    —$key→ $next" }) ++
        candidates.take(8).map(candidate => s"  candidate ${candidate.render}")
  }

  def explain(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
              calibration: IdentityCalibration = IdentityCalibration.resolver,
              pins: PinConstraints = PinConstraints(Nil),
              decorations: TitleDecorations = TitleDecorations.resolver)(wanted: Set[ListingKey]): Seq[ListingExplanation] = {
    val stages = new ResolveStages(listings, lookups, normalizer, calibration, Mutation.None, pins, decorations)
    import stages.families.{blockKeysOf, familyOf, scopeOf}
    lazy val resolution = run(stages, Mutation.None)
    // One explanation per node holding a wanted listing: a node's listings share everything told.
    stages.generation.nodes.filter(_.listings.exists(listing => wanted(listing.key))).map { node =>
      val listing  = node.listings.find(listing => wanted(listing.key)).get.key
      val context  = stages.generation.context
      val keyed    = TitleLinks.keyed(node, normalizer, pins, context.wholeTitle, context.bannerSegment)
      val titled   = keyed.keys
      val accepted = blockKeysOf(node.id).toSeq.sorted.filterNot(titled).map(key => key -> "a film it, or a node sharing its keys, accepted")
      val members  = stages.generation.nodes.filter(member => familyOf(member.id) == familyOf(node.id))
      val anchor   = members.minBy(member => (-member.weight, member.id))
      // Breadth-first over shared keys from the node to the anchor: the links that put it here.
      val byKey    = members.flatMap(member => blockKeysOf(member.id).map(_ -> member)).groupMap(_._1)(_._2)
      val via      = scala.collection.mutable.HashMap(node.id -> Option.empty[(String, EvidenceNode)])
      val queue    = scala.collection.mutable.Queue(node)
      while (queue.nonEmpty && !via.contains(anchor.id)) {
        val current = queue.dequeue()
        blockKeysOf(current.id).toSeq.sorted.foreach(key => byKey.getOrElse(key, Nil).foreach { next =>
          if (!via.contains(next.id)) { via(next.id) = Some(key -> current); queue.enqueue(next) } })
      }
      def path(at: EvidenceNode): List[(String, String)] = via.get(at.id).flatten match {
        case Some((key, from)) => path(from) :+ (key -> at.label)
        case None              => Nil
      }
      val decision = resolution.decisionOf(listing)
      ListingExplanation(node.label, familyOf(node.id), members.map(_.weight).sum, keyed.kept ++ accepted, keyed.dropped,
        if (anchor eq node) Nil else path(anchor), decision, scopeOf(node).of(node).map(ResolveReports.CandidateScore.of(_, calibration)))
    }
  }

  /** One of the largest families, taken apart: its size, the block keys holding the most of its
   *  nodes, and for each of those the nodes the family's largest piece keeps once that key is dropped
   *  — a key that alone glues the family together leaves a small piece. For a report finding why
   *  a family grew far past one film. */
  final case class FamilyAnatomy(listings: Int, nodes: Int, keys: Seq[(String, Int)], withoutKey: Seq[(String, Int)],
                                 connectors: Seq[(String, Seq[String])] = Nil, withoutKind: Seq[(String, Int)] = Nil) {
    def render: Seq[String] =
      s"family of $listings listing(s) in $nodes node(s)" +:
        (s"  keys by nodes: ${keys.map { case (key, count) => s"$key ×$count" }.mkString(", ")}" +:
          (withoutKind.map { case (kind, largest) => s"  without every $kind key: largest piece $largest node(s)" } ++
            withoutKey.map { case (key, largest) => s"  without $key: largest piece $largest node(s)" } ++
            connectors.map { case (node, shared) => s"  connector $node: ${shared.mkString(", ")}" }))
  }

  def familyAnatomy(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                    calibration: IdentityCalibration = IdentityCalibration.resolver,
                    decorations: TitleDecorations = TitleDecorations.resolver)(largest: Int): Seq[FamilyAnatomy] = {
    val stages   = new ResolveStages(listings, lookups, normalizer, calibration, Mutation.None, PinConstraints(Nil), decorations)
    val anatomy  = new FamilyAnatomies(stages.families.blockKeysOf, stages.generation.context)
    stages.generation.nodes.groupBy(node => stages.families.familyOf(node.id)).values.toSeq
      .sortBy(members => -members.map(_.weight).sum).take(largest).map(anatomy.of)
  }

  /** One candidate as a node's family scored it: the calibrated probability, the search rank, whether the node's own
   *  evidence denies it, whether the title names it, and whether its record is the listing's season or house production
   *  — the model's side of the unified evidence model's signals ([[UnifiedEvidence]]) — the measures it was scored
   *  on, which a refit of the calibration counts (`scripts.IdentityRefit`), and, a namesake the release veto took out
   *  ([[ReleaseVeto]]), the namesake it stood beside that TMDB dates a release of in the venue's country. */
  final case class CandidateEvidence(tmdbId: Int, film: IdentityMeasures.Film, probability: Double, rank: Option[Int], denied: Boolean,
                                     titleNamesIt: Boolean, seasonProduction: Boolean, houseProduction: Boolean,
                                     measures: Map[String, IdentityMeasures.Measure] = Map.empty, releasedBeside: Option[Int] = None)
  /** A node's listings and its candidates as its family scored them (best first). */
  final case class NodeEvidence(keys: Seq[ListingKey], candidates: Seq[CandidateEvidence])

  /** Every node holding a listing `focused` selects, its candidates scored as the resolve scores them. */
  def evidenceOf(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                 calibration: IdentityCalibration = IdentityCalibration.resolver,
                 decorations: TitleDecorations = TitleDecorations.resolver)(focused: Listing => Boolean): Seq[NodeEvidence] =
    evidenceIn(new ResolveStages(listings, lookups, normalizer, calibration, Mutation.None, PinConstraints(Nil), decorations))(focused)

  private def evidenceIn(stages: ResolveStages)(focused: Listing => Boolean): Seq[NodeEvidence] =
    focusedScored(stages)(focused).map { case (node, scored) =>
      NodeEvidence(node.listings.map(_.key), scored.map(s => CandidateEvidence(s.candidate.tmdbId, s.candidate.film, s.probability, s.rank, s.denied,
        s.titleNamesIt, s.seasonProduction, s.houseProduction, s.measures, s.denial.flatMap(ReleaseVeto.releasedBeside))))
    }

  /** Every node holding a listing `focused` selects, with its candidates as its family scores them (best first). */
  private[identity] def focusedScored(stages: ResolveStages)(focused: Listing => Boolean): Seq[(EvidenceNode, Seq[services.identity.Scored])] =
    stages.generation.nodes.filter(_.listings.exists(focused)).map(node => node -> stages.families.scopeOf(node).of(node))

  /** A [[resolve]], and every node's candidates as that same resolve scored them ([[evidenceOf]], read only when asked):
   *  one pass for a caller wanting both — building the stages a second time was a whole second resolve. A family's verdict
   *  (`agreement.Agreement.verdict`, its only caller) takes a film one piece of the billing names ([[MembersApart]]): what
   *  the families agree on is the agreement stage's to read against the billing (`AgreementStage.pieceOf`), which also keeps
   *  a fill off it. */
  final class Scored private[IdentityResolver] (val resolution: Resolution, stages: ResolveStages) {
    lazy val evidence: Seq[NodeEvidence] = evidenceIn(stages)(_ => true)
  }
  def resolveScored(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                    calibration: IdentityCalibration = IdentityCalibration.resolver): Scored = {
    val stages = new ResolveStages(listings, lookups, normalizer, calibration, Mutation.None, PinConstraints(Nil), TitleDecorations.resolver)
    new Scored(run(stages, Mutation.None, pieces = false), stages)
  }

  /** The corpus-wide facts a resolve of `listings` reads ([[CorpusContext]]): what one family,
   *  resolved alone with `corpus = Some(...)`, needs to decide as the whole resolve does. */
  private[identity] def contextOf(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
                                  pins: PinConstraints = PinConstraints(Nil),
                                  decorations: TitleDecorations = TitleDecorations.None): CorpusContext =
    new CandidateGeneration(listings.toSeq.sorted.distinctBy(_.key), lookups, normalizer, pins, decorations, lazyLookups = false).context

  private[identity] def resolveWith(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
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
  private[identity] def resolveRegion(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleNormalizer,
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

  /** `pieces`: whether a member billing its cluster's film as one piece beside another thing is decided apart, as no film
   *  ([[billedAsPiece]]) — the model's own takes; not a family's verdict ([[resolveScored]]). */
  private def run(stages: ResolveStages, mutation: Mutation, pieces: Boolean = true): Resolution =
    new ResolverRun(stages, mutation, pieces).resolution
}
