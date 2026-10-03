package services.identity

import services.identity.Scored.Accepted
import services.movies.TitleNormalizer

import scala.annotation.tailrec

/** The FAMILIES: the closure over title keys and the films members accept, grown until stable —
 *  a node matching a film another family's listings match joins that family, and its pool. Each
 *  round scores every family ([[FamilyScope]]) and takes what each node accepts ALONE
 *  ([[Acceptance.alone]]), unless a title-linked sibling denies it ([[withoutSiblingDenials]]).
 *  `narrow` is the teeth tests' `Mutation.NarrowFamilies`: families by sanitised title only, never grown. */
private[identity] final class Families(scoring: CandidateScoring, acceptance: Acceptance, links: TitleLinks,
                                       normalizer: TitleNormalizer, narrow: Boolean) {
  import scoring.{evidenceDenies, houses, pins}
  import scoring.generation.{candidateOf, nodeById, nodes}

  val pinnedFilm: Map[String, Int] = nodes.flatMap(node => pins.filmOf(node.listings.head.key).map(node.id -> _)).toMap

  private def familiesOf(ids: Map[String, Set[Int]]): Map[String, Int] =
    FamilyClosure.families(nodes.map(node => node.id -> (
      if (narrow) Set("t:" + normalizer.sanitize(node.evidence.cleanTitle))
      else links.titleKeys(node) ++ ids.getOrElse(node.id, Set.empty[Int]).map(filmId => s"id:${filmId}"))).toMap)

  /** A node's ALONE acceptance, withdrawn when a title-linked sibling that accepted nothing itself
   *  DENIES the film: a bare "Samson i Dalila" beside the same venue family's "…: live in hd
   *  2026/27" has no evidence of its own against DeMille's 1949 film, but its sibling's season
   *  is. The node then goes to the group vote with its siblings, where every member's denial holds. */
  private def withoutSiblingDenials(members: Seq[EvidenceNode], scope: FamilyScope,
                                    alone: Map[String, Accepted]): Map[String, Accepted] =
    alone.filter { case (id, (best, _)) => deniedBySibling(nodeById(id), members, scope, best.candidate.tmdbId, alone.contains).isEmpty }

  /** The title-linked sibling, itself taken by no rule alone, whose own evidence denies `film` — and its denial: why a
   *  node's own match of `film` is withdrawn ([[withoutSiblingDenials]]), which the node's trace says. */
  def deniedBySibling(node: EvidenceNode, members: Seq[EvidenceNode], scope: FamilyScope, film: Int,
                      takenAlone: String => Boolean): Option[(EvidenceNode, String)] =
    members.iterator.filter(sibling => sibling.id != node.id && !takenAlone(sibling.id) && links.titleLinked(node, sibling))
      .flatMap(sibling => scope.of(sibling).find(other => other.candidate.tmdbId == film && other.denied).map(other => sibling -> other.denial.getOrElse("denied")))
      .nextOption()

  /** How many listings every scope of every round scored ([[Resolution.scorings]]) — counted, not
   *  read off the scopes, so a round's replaced scopes and their scores are not kept alive. */
  private var listingsScored = 0
  def scorings: Int = listingsScored

  private final case class Round(matchedIds: Map[String, Set[Int]], familyOf: Map[String, Int],
                                 scopes: Map[Int, FamilyScope], bestOf: Map[String, Accepted])

  /** Each round scores every family, but a family the last round left as it was scores as it did:
   *  its scope (and the node scores it memoises) is kept, keyed by its members. Only the families a
   *  round merged are scored again. */
  @tailrec private def grow(matchedIds: Map[String, Set[Int]], familyOf: Map[String, Int],
                            scored: Map[Seq[String], FamilyScope] = Map.empty): Round = {
    val scopes = nodes.groupBy(node => familyOf(node.id)).map { case (family, members) =>
      val sorted = members.sortBy(_.id)
      family -> scored.getOrElse(sorted.map(_.id), new FamilyScope(sorted, scoring, () => listingsScored += 1))
    }
    val bestOf = nodes.groupBy(node => familyOf(node.id)).toSeq.flatMap { case (family, members) =>
      val scope = scopes(family)
      withoutSiblingDenials(members, scope, members.flatMap(node => acceptance.alone(scope.of(node)).map(node.id -> _)).toMap)
    }.toMap
    val grown = nodes.map(node => node.id -> (matchedIds.getOrElse(node.id, Set.empty[Int]) ++ bestOf.get(node.id).map(_._1.candidate.tmdbId) ++
      pinnedFilm.get(node.id))).toMap
    if (grown == matchedIds || narrow) Round(grown, familyOf, scopes, bestOf)
    else grow(grown, familiesOf(grown), scopes.values.map(scope => scope.members.map(_.id) -> scope).toMap)
  }
  private val round = grow(Map.empty, familiesOf(Map.empty))

  /** Each node's family. */
  val familyOf: Map[String, Int] = round.familyOf
  /** Each family's scoring. */
  val scopes: Map[Int, FamilyScope] = round.scopes
  /** What each node accepted alone, in the final round. */
  val bestOf: Map[String, Accepted] = round.bestOf
  /** Each node's block keys: its title keys and the films its family's round matched. */
  val blockKeysOf: Map[String, Set[String]] =
    nodes.map(node => node.id -> (links.titleKeys(node) ++ round.matchedIds.getOrElse(node.id, Set.empty[Int]).map(filmId => s"id:${filmId}"))).toMap

  def scopeOf(node: EvidenceNode): FamilyScope = scopes(familyOf(node.id))

  /** Does `n`'s evidence deny `film` — scored, or, for a film it has no evidence path to, its own
   *  evidence against the film's record? */
  def denies(node: EvidenceNode, film: Int): Boolean =
    scopeOf(node).of(node).find(_.candidate.tmdbId == film).fold(pins.deniedFilms(node.listings.head.key)(film) || {
      candidateOf(film).exists { candidate =>
        evidenceDenies(node.evidence.measured, candidate.film, IdentityMeasures.listingFilm(node.evidence.measured, candidate.film, None, 0, 0, houses, scopeOf(node).qualifiers))
      }
    })(_.denied)
}
