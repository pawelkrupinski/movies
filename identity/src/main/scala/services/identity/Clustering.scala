package services.identity

import IdentityResolver.{FamilyCrossing, Mutation}
import services.identity.vocabulary.{FamilyClosure, ResolverEdge}

/** The CLUSTERING stage of a resolve ([[IdentityResolver]]): the constraint edges between a family's nodes
 *  ([[ConstraintEdges]]) and the clusters [[ConstraintSolver]] makes of them -- round A over the films the nodes took
 *  alone ([[acceptedIn]]), the final round over those and the films the votes added. */
private[identity] final class Clustering(stages: ResolveStages, mutation: Mutation) {
  import stages.{families, links, ordered, scoring}
  import stages.generation.{nodeById, nodes}
  import families.familyOf
  import Clustering.Round

  private val edges = new ConstraintEdges(scoring, families, links)

  private val ownFilms: Map[String, Int] = families.bestOf.map { case (id, (scored, _)) => id -> scored.candidate.tmdbId }
  // Nodes one chain lists under ONE catalogue id that accepted different films, each on its own
  // evidence: neither acceptance stands, so the id's must-link unites them and the cluster votes on
  // its pooled evidence. Showcase's "9 to 5 (2026)" credits Colin Higgins at two venues; its two
  // bare venues, dated only by the re-release, had taken a two-minute 2026 short.
  private val contested: Set[String] =
    nodes.flatMap(node => edges.catalogueKeys(node).map(_ -> node.id)).groupMap(_._1)(_._2).values
      .flatMap(ids => if (ids.distinct.flatMap(ownFilms.get).distinct.sizeIs > 1) ids else Nil).toSet
  private val acceptedAll: Map[String, Int] = ownFilms.filterNot { case (id, _) => contested(id) } ++ families.pinnedFilm
  // Round A's edges over EVERY pair of nodes sharing a block key, then the family check: an
  // edge between two families means the scoping would silently drop it, so the resolve stops.
  private val roundAEdges = edges.of(nodes, acceptedAll.get)
  private val crossings = FamilyClosure.crossings(familyOf, roundAEdges.map(edge => FamilyClosure.Edge(edge.a, edge.b, edge.must, edge.reason)))
  if (crossings.nonEmpty) throw new FamilyCrossing(crossings.size, s"${crossings.size} edge(s) cross a family, e.g. ${crossings.head}")
  private val roundAByFamily = roundAEdges.groupBy(edge => familyOf(edge.a))

  /** The films `members` took alone -- their own matches, but those a catalogue id contests, and their pins' films. */
  def acceptedIn(members: Seq[EvidenceNode]): Map[String, Int] = members.flatMap(node => acceptedAll.get(node.id).map(node.id -> _)).toMap

  /** Round A of `family`: its edges over the films its nodes took alone (`accepted`), and the clusters they make. */
  def roundA(family: Int, members: Seq[EvidenceNode], accepted: Map[String, Int]): Round = {
    val familyEdges = roundAByFamily.getOrElse(family, Nil)
    Round(familyEdges, solve(members, familyEdges, accepted.get))
  }

  /** The final round of `members`: their edges over the films they took or voted (`filmOf`), and the clusters they make
   *  -- the bare spellings an ambiguity left alone grouped (ConstraintSolver rule 4); round A, which the votes read,
   *  leaves each alone, so each voted on its own evidence. */
  def finalRound(members: Seq[EvidenceNode], filmOf: String => Option[Int]): Round = {
    val familyEdges = edges.of(members, filmOf)
    Round(familyEdges, solve(members, familyEdges, filmOf, ambiguousTogether = true))
  }

  private def solve(members: Seq[EvidenceNode], edges: Seq[ResolverEdge], filmOf: String => Option[Int],
                    ambiguousTogether: Boolean = false): Seq[Seq[EvidenceNode]] = {
    val constraints = edges.map(edge => ConstraintSolver.Constraint(edge.a, edge.b, edge.must, edge.tier, edge.reason))
    val presentation = if (mutation == Mutation.FirstWins) ConstraintSolver.Presentation.AsGiven else ConstraintSolver.Presentation.Canonical
    val presented = if (mutation == Mutation.FirstWins) {
      val position = ordered.zipWithIndex.map { case (listing, index) => listing.sortKey -> index }.toMap
      members.sortBy(node => node.listings.map(listing => position(listing.sortKey)).min)
    } else members
    val orderedConstraints = if (mutation == Mutation.FirstWins) {
      val position = presented.zipWithIndex.map { case (node, index) => node.id -> index }.toMap
      constraints.sortBy(constraint => (math.min(position(constraint.a), position(constraint.b)), math.max(position(constraint.a), position(constraint.b))))
    } else constraints
    ConstraintSolver.solveAs(presented.map(_.id), orderedConstraints, presentation, members.flatMap(node => filmOf(node.id).map(node.id -> _)).toMap,
      ambiguousTogether)
      .map(_.map(nodeById))
  }
}

private[identity] object Clustering {
  /** One round of a family's clustering: its edges and the clusters the solver made of them. */
  final case class Round(edges: Seq[ResolverEdge], clusters: Seq[Seq[EvidenceNode]]) {
    /** The round's title must-links (same title, search form, original title, segment), however the solver then split
     *  the nodes they join: the title family a facts-free cluster's vote reads ([[ClusterVoting.familyMajority]]). */
    lazy val titleEdges: Seq[ResolverEdge] = edges.filter(edge => edge.must && ConstraintEdges.TitleTiers(edge.tier))
    /** Each node's cluster, by its position. */
    lazy val clusterIndex: Map[String, Int] = clusters.zipWithIndex.flatMap { case (cluster, index) => cluster.map(_.id -> index) }.toMap
    /** The cannot-links the solver could not keep: both nodes in one cluster. */
    def violations: Int = edges.count(edge => !edge.must && clusterIndex(edge.a) == clusterIndex(edge.b))
  }
}
