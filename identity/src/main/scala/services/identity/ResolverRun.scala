package services.identity

import models.ListingKey

import IdentityResolver.Mutation
import services.identity.vocabulary.ResolverEdge

/** Stage B of a resolve ([[IdentityResolver]]), per family: it SEQUENCES the stages and owns none of their steps --
 *  [[Clustering]]'s round A, [[ClusterVoting]]'s votes for the clusters no member took a film alone in, Clustering's final
 *  round over those films, and [[Deciding]]'s decisions of the final clusters. `pieces`: whether a member billing its
 *  cluster's film as one piece beside another thing is decided apart ([[Deciding]]). */
private[identity] final class ResolverRun(stages: ResolveStages, mutation: Mutation, pieces: Boolean) {
  import stages.{families, generation}
  import generation.{candidateOf, issued, nodes}
  import families.{familyOf, scopes}

  private val clustering = new Clustering(stages, mutation)
  private val voting     = new ClusterVoting(stages.scoring, families, stages.acceptance)
  private val deciding   = new Deciding(stages, voting, pieces)

  /** The resolution: every family's decisions, and the resolve's counts. */
  def resolution: Resolution = {
    val runs      = nodes.groupBy(node => familyOf(node.id)).toSeq.sortBy(_._1).map { case (family, members0) => new FamilyRun(family, members0.sortBy(_.id)) }
    val perFamily = runs.map(_.decided)
    val decided = perFamily.flatMap(_._2).sortBy(_.members.head)(using ListingKey.ordering)
    Resolution(
      decisions      = decided,
      nodes          = nodes.size,
      familyOf       = nodes.flatMap(node => node.listings.map(_.key -> familyOf(node.id))).toMap,
      edges          = perFamily.flatMap(_._1),
      queries        = issued.toSeq,
      filmLookups    = generation.filmLookups,
      unknownQueries = generation.unknownQueries,
      unknownDetails = generation.unknownDetails,
      unknownFilms   = generation.unknownFilms,
      violations     = perFamily.map(_._3).sum,
      films          = decided.flatMap(_.film).distinct.flatMap(id => candidateOf(id).map(id -> _.film)).toMap,
      scorings       = families.scorings,
      titleRelations = families.titleRelations,
      contested      = runs.flatMap(_.contested).toMap)
  }

  /** One family's assignment: round A's clusters, the votes of those no member matched alone, the final clusters, and
   *  each cluster's decisions. */
  private final class FamilyRun(family: Int, members: Seq[EvidenceNode]) {
    private val scope      = scopes(family)
    private val accepted   = clustering.acceptedIn(members)
    private val roundA     = clustering.roundA(family, members, accepted)
    private val unaccepted = roundA.clusters.filter(_.forall(node => !accepted.contains(node.id)))
    private val votes: Map[String, ResolverDecisions.Vote] =
      if (mutation == Mutation.NoVoting) Map.empty else voting.votes(unaccepted, members, accepted, roundA.titleEdges, scope)
    /** Each listing of a facts-free cluster whose title family backs two films or more by its siblings' own statements
     *  ([[ClusterVoting.contested]]), with those films and their venues: report only. */
    def contested: Seq[(ListingKey, Map[Int, Set[String]])] = voting.contested(unaccepted, members, accepted, roundA.titleEdges)
    private val filmOf: String => Option[Int] = id => accepted.get(id).orElse(votes.get(id).map(_.film))
    private val finalRound = clustering.finalRound(members, filmOf)
    private val decisions  = deciding.of(Deciding.Clustered(members, scope, accepted, votes, filmOf, finalRound))

    def decided: (Seq[ResolverEdge], Seq[ResolverDecision], Int) = (finalRound.edges, decisions.all, finalRound.violations)
  }
}
