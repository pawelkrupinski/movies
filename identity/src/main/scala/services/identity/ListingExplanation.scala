package services.identity

import models.ListingKey

import services.titles.TitleForms
import services.identity.vocabulary.{PinConstraints, TitleDecorations}

/** Why a listing resolved as it did and sits where it does — the resolve's own stages, told:
 *  its node; every family key it has and why, and every title piece it does not block under and
 *  why not; the chain of shared keys linking it to its family's heaviest member (why THIS
 *  family); its decision with the decision's own explanation; and its candidates as its family
 *  scored them. */
final case class ListingExplanation(node: String, family: Int, familyListings: Int, keys: Seq[(String, String)],
                                    dropped: Seq[(String, String)], chain: Seq[(String, String)], decision: ResolverDecision,
                                    candidates: Seq[NodeCandidates.CandidateScore]) {
  def render: Seq[String] =
    Seq(s"listing in node $node, family $family ($familyListings listing(s))",
        s"  decision: ${decision.render}") ++
      keys.map { case (key, why) => s"  key $key — $why" } ++
      dropped.map { case (piece, why) => s"  not a key: $piece — $why" } ++
      (if (chain.isEmpty) Seq("  the family's heaviest node itself") else s"  in this family by:" +: chain.map { case (key, next) => s"    —$key→ $next" }) ++
      candidates.take(8).map(candidate => s"  candidate ${candidate.render}")
}

object ListingExplanation {
  /** Why each listing `wanted` selects resolved as it did, one explanation per node holding one. */
  def of(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleForms,
         calibration: IdentityCalibration = IdentityCalibration.resolver,
         pins: PinConstraints = PinConstraints(Nil),
         decorations: TitleDecorations = TitleDecorations.resolver)(wanted: Set[ListingKey]): Seq[ListingExplanation] = {
    val stages = new ResolveStages(listings, lookups, normalizer, calibration, IdentityResolver.Mutation.None, pins, decorations)
    import stages.families.{blockKeysOf, familyOf, scoredOf}
    lazy val resolution = IdentityResolver.run(stages, IdentityResolver.Mutation.None)
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
        if (anchor eq node) Nil else path(anchor), decision, scoredOf(node).map(NodeCandidates.CandidateScore.of(_, calibration)))
    }
  }
}
