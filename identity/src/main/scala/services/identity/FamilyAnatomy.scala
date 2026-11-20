package services.identity

import services.titles.TitleForms
import services.identity.vocabulary.{FamilyClosure, PinConstraints, TitleDecorations}
import services.identity.measures.{EvidenceNode, Listing}

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

object FamilyAnatomy {
  /** The `count` largest families of `listings`' resolve, each taken apart. */
  def largest(listings: Iterable[Listing], lookups: IdentityLookups, normalizer: TitleForms,
              calibration: IdentityCalibration = IdentityCalibration.resolver,
              decorations: TitleDecorations = TitleDecorations.resolver)(count: Int): Seq[FamilyAnatomy] = {
    val stages = new ResolveStages(listings, lookups, normalizer, calibration, IdentityResolver.Mutation.None, PinConstraints(Nil), decorations)
    stages.generation.nodes.groupBy(node => stages.families.familyOf(node.id)).values.toSeq
      .sortBy(members => -members.map(_.weight).sum).take(count).map(of(_, stages.families.blockKeysOf, stages.generation.context))
  }

  /** `members` taken apart: the block keys holding the most of them, the largest piece left once a key (or every key of a
   *  kind) is dropped, and the nodes welding them together. */
  private def of(members: Seq[EvidenceNode], blockKeysOf: String => Set[String], context: CorpusContext): FamilyAnatomy = {
    val keysOf = members.map(node => node.id -> blockKeysOf(node.id)).toMap
    val byKey  = keysOf.toSeq.flatMap { case (id, keys) => keys.map(_ -> id) }.groupMap(_._1)(_._2)
    val ranked = byKey.toSeq.map { case (key, ids) => key -> ids.size }.sortBy { case (key, count) => (-count, key) }
    FamilyAnatomy(members.map(_.weight).sum, members.size, ranked.take(25),
      ranked.take(15).map { case (key, _) => key -> largestPieceKeeping(keysOf, _ != key) },
      connectors(members, keysOf, byKey, context), withoutKind(keysOf))
  }

  /** The nodes joining the most keys other members carry: in a mesh no single key bridges, these weld it. */
  private def connectors(members: Seq[EvidenceNode], keysOf: Map[String, Set[String]],
                         byKey: Map[String, Seq[String]], context: CorpusContext): Seq[(String, Seq[String])] =
    members.map(node => node -> keysOf(node.id).filter(key => byKey(key).sizeIs > 1).toSeq.sorted)
      .filter(_._2.sizeIs >= 2).sortBy { case (node, shared) => (-shared.size, node.id) }.take(40)
      .map { case (node, shared) => s"${node.label} «${node.evidence.published.rawTitle.getOrElse(node.evidence.title)}»" -> shared.map(explained(_, context)) }

  /** Each title key with the inputs the segment rule read: its spread and whether it is a whole title. */
  private def explained(key: String, context: CorpusContext): String =
    if (key.startsWith("t:")) s"$key(spread ${context.segmentSpread(key.drop(2))}${if (context.wholeTitle(key.drop(2))) ", whole" else ""})" else key

  /** The nodes of the family's largest piece once only the keys `kept` holds join it. */
  private def largestPieceKeeping(keysOf: Map[String, Set[String]], kept: String => Boolean): Int =
    FamilyClosure.families(keysOf.map { case (id, keys) => id -> keys.filter(kept) }).groupBy(_._2).values.map(_.size).maxOption.getOrElse(0)

  /** Which KIND of key meshes it: a sanitised title or segment (t:), a search form (q:), a film members accepted (id:). */
  private def withoutKind(keysOf: Map[String, Set[String]]): Seq[(String, Int)] =
    Seq("t:", "q:", "id:").map(kind => kind -> largestPieceKeeping(keysOf, key => !key.startsWith(kind))) :+
      ("t: and q:" -> largestPieceKeeping(keysOf, key => !key.startsWith("t:") && !key.startsWith("q:")))
}
