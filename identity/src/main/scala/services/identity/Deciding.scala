package services.identity
import services.identity.measures.{DecisionTrace, EvidenceNode, ResolverDecision}

/** The DECIDING stage of a resolve ([[IdentityResolver]]): each final cluster of a family decided ([[ResolverDecisions]])
 *  -- its film, or, where some of its members bill several works, bill its film as one piece beside another thing
 *  ([[NoFilmMembers]]) or take it uncorroborated ([[Corroboration]]), the rest's film and each such group's none.
 *  `pieces`: whether a member billing its cluster's film as one piece beside another thing is decided apart, as no film
 *  -- the model's own takes; not a family's verdict ([[IdentityResolver.resolveScored]]). */
private[identity] final class Deciding(stages: ResolveStages, voting: ClusterVoting, pieces: Boolean) {
  import stages.{acceptance, families, generation, scoring}
  import Deciding.Clustered

  private val decisions = new ResolverDecisions(scoring, families, acceptance)

  /** `family`'s decisions, read off its final clusters: its members' bills, pieces and corroboration found at once. */
  def of(family: Clustered): FamilyDecisions = new FamilyDecisions(family)

  final class FamilyDecisions(family: Clustered) {
    import family.{accepted, members, scope, votes}
    private val edges        = family.round.edges
    private val clusterIndex = family.round.clusterIndex

    // A member billing several works is none of them, whichever film its cluster took or voted
    // ("We're Going on a Bear Hunt + The Tiger Who Came to Tea" {Joanna Harrison, Robin Shaw}, joined by its
    // title to the spelling crediting Harrison alone — itself a bill): it is decided apart, as no film.
    private val bills   = members.filter(node => acceptance.billsSeveralWorks(scope.of(node))).map(_.id).toSet
    private val apart   = new NoFilmMembers(scope, generation)
    private val billing = bills ++ apart.billingUnderBanner(members, bills, family.filmOf, scope.of)
    private def linked(node: EvidenceNode) = scope.feedLinked(node).map(_._2)
    private val backed  = Corroboration.support(family.round.clusters, _.iterator.flatMap(node => family.filmOf(node.id)).nextOption(), scope.of, linked)

    /** Every final cluster's decisions. */
    def all: Seq[ResolverDecision] = family.round.clusters.flatMap(decide)

    /** One cluster's decisions: the cluster's own, or — where some of its members bill several works, bill its film as
     *  one piece beside another thing, or take it uncorroborated — the rest's, and each such group's as none. */
    private def decide(cluster: Seq[EvidenceNode]): Seq[ResolverDecision] = {
      val (bills, unbilled) = cluster.partition(node => billing(node.id))
      // a member billing its cluster's film as one piece beside another thing is none of it either — unless a pin took it
      val film   = cluster.iterator.flatMap(node => family.filmOf(node.id)).nextOption()
      val pinned = cluster.exists(node => families.pinnedFilm.contains(node.id))
      val piecesOf = billedAsPieces(unbilled, film, pinned)
      val placed = unbilled.filterNot(piecesOf.contains)
      // a member nothing of its own corroborates the film by, whose take a fact or an evidenced namesake speaks against
      val weak   = film.filterNot(_ => pinned).toSeq.flatMap(Corroboration.refused(placed, _, scope.of, linked, edges, backed)).toMap
      val rest   = placed.filterNot(weak.contains)
      if ((bills.isEmpty && piecesOf.isEmpty && weak.isEmpty) || film.isEmpty)
        Seq(decisions.of(cluster, scope, family.filmOf, accepted, votes, edges, clusterIndex))
      else
        restDecided(rest, film) ++
          Option.when(bills.nonEmpty)(none(bills, Seq("bills several works: none of them"), DecisionTrace.BillsBoth)) ++
          Option.when(piecesOf.nonEmpty)(none(unbilled.filter(piecesOf.contains), unbilled.flatMap(piecesOf.get).distinct, DecisionTrace.BilledAsPiece)) ++
          Option.when(weak.nonEmpty)(none(placed.filter(weak.contains), placed.flatMap(weak.get).distinct, DecisionTrace.Uncorroborated))
    }

    /** The members of `unbilled` billing `film` as one piece beside another thing, or beside their screening, with why. */
    private def billedAsPieces(unbilled: Seq[EvidenceNode], film: Option[Int], pinned: Boolean): Map[EvidenceNode, String] =
      film.filterNot(_ => pinned || !pieces).fold(Map.empty[EvidenceNode, String])(apart.billedAsPieces(unbilled, _))

    // a cluster of bills or pieces alone takes no film its vote reaches either
    private def none(nodes: Seq[EvidenceNode], why: Seq[String], veto: String): ResolverDecision = {
      val neither = decisions.of(nodes, scope, _ => None, Map.empty, Map.empty, edges, clusterIndex)
      neither.copy(explanation = why ++ neither.explanation)(neither.trace.copy(vetoed = Some(DecisionTrace.Veto(veto, None))))
    }

    /** The decision of a cluster's `rest`, its members decided apart as none of `film` taken out. A member decided apart
     *  as none of the film lends the rest nothing: when the film was the cluster's pooled vote (no member of the rest took
     *  it alone, no pin, no family majority), the rest vote again on their own evidence and keep the film only if it still
     *  wins. DE: KAP-Torgau's "TACHELES – Kurzfilmprogramm" (a bill, kinoheld page endlich-tacheles) carried Endlich
     *  Tacheles onto the bare "Kurzfilmprogramm" billings. */
    private def restDecided(rest: Seq[EvidenceNode], film: Option[Int]): Seq[ResolverDecision] = {
      val pooledOnly = rest.exists(node => votes.contains(node.id)) && rest.forall(node => !accepted.contains(node.id) &&
        !families.pinnedFilm.contains(node.id) && votes.get(node.id).forall(_.majority.isEmpty))
      val restVotes =
        if (!pooledOnly) votes
        else votes -- rest.map(_.id) ++ voting.vote(rest, scope).collect {
          case (id, (taken, probability)) if film.contains(taken) => id -> ResolverDecisions.Vote(taken, probability, None)
        }
      val restFilm: String => Option[Int] = id => accepted.get(id).orElse(restVotes.get(id).map(_.film))
      Option.when(rest.nonEmpty)(decisions.of(rest, scope, restFilm, accepted, restVotes, edges, clusterIndex)).toSeq
    }
  }
}

private[identity] object Deciding {
  /** A family as clustering and voting left it: its members and scope, the films they took alone (`accepted`) or voted
   *  (`votes`) -- `filmOf` either --, and its final round of clusters. */
  final case class Clustered(members: Seq[EvidenceNode], scope: FamilyScope, accepted: Map[String, Int],
                             votes: Map[String, ResolverDecisions.Vote], filmOf: String => Option[Int], round: Clustering.Round)
}
