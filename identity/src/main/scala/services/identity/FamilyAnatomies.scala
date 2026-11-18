package services.identity

/** One family taken apart ([[IdentityResolver.FamilyAnatomy]]): the block keys holding the most of its nodes, the
 *  largest piece left once a key (or every key of a kind) is dropped, and the nodes welding it together. For a report
 *  finding why a family grew far past one film. */
private[identity] final class FamilyAnatomies(blockKeysOf: String => Set[String], context: CorpusContext) {

  def of(members: Seq[EvidenceNode]): IdentityResolver.FamilyAnatomy = {
    val keysOf = members.map(node => node.id -> blockKeysOf(node.id)).toMap
    val byKey  = keysOf.toSeq.flatMap { case (id, keys) => keys.map(_ -> id) }.groupMap(_._1)(_._2)
    val ranked = byKey.toSeq.map { case (key, ids) => key -> ids.size }.sortBy { case (key, count) => (-count, key) }
    IdentityResolver.FamilyAnatomy(members.map(_.weight).sum, members.size, ranked.take(25),
      ranked.take(15).map { case (key, _) => key -> FamilyAnatomies.largestPieceKeeping(keysOf, _ != key) },
      connectors(members, keysOf, byKey), FamilyAnatomies.withoutKind(keysOf))
  }

  /** The nodes joining the most keys other members carry: in a mesh no single key bridges, these weld it. */
  private def connectors(members: Seq[EvidenceNode], keysOf: Map[String, Set[String]],
                         byKey: Map[String, Seq[String]]): Seq[(String, Seq[String])] =
    members.map(node => node -> keysOf(node.id).filter(key => byKey(key).sizeIs > 1).toSeq.sorted)
      .filter(_._2.sizeIs >= 2).sortBy { case (node, shared) => (-shared.size, node.id) }.take(40)
      .map { case (node, shared) => s"${node.label} «${node.evidence.published.rawTitle.getOrElse(node.evidence.title)}»" -> shared.map(explained) }

  /** Each title key with the inputs the segment rule read: its spread and whether it is a whole title. */
  private def explained(key: String): String =
    if (key.startsWith("t:")) s"$key(spread ${context.segmentSpread(key.drop(2))}${if (context.wholeTitle(key.drop(2))) ", whole" else ""})" else key
}

private[identity] object FamilyAnatomies {
  /** The nodes of the family's largest piece once only the keys `kept` holds join it. */
  private def largestPieceKeeping(keysOf: Map[String, Set[String]], kept: String => Boolean): Int =
    FamilyClosure.families(keysOf.map { case (id, keys) => id -> keys.filter(kept) }).groupBy(_._2).values.map(_.size).maxOption.getOrElse(0)

  /** Which KIND of key meshes it: a sanitised title or segment (t:), a search form (q:), a film members accepted (id:). */
  private def withoutKind(keysOf: Map[String, Set[String]]): Seq[(String, Int)] =
    Seq("t:", "q:", "id:").map(kind => kind -> largestPieceKeeping(keysOf, key => !key.startsWith(kind))) :+
      ("t: and q:" -> largestPieceKeeping(keysOf, key => !key.startsWith("t:") && !key.startsWith("q:")))
}
