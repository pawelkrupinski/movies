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
  import scoring.generation.{candidateById, nodeById, nodes}

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
    alone.filter { case (id, (best, _)) =>
      val node = nodeById(id)
      !members.exists(sibling => sibling.id != id && !alone.contains(sibling.id) && links.titleLinked(node, sibling) &&
        scope.of(sibling).exists(other => other.candidate.tmdbId == best.candidate.tmdbId && other.denied))
    }

  private final case class Round(matchedIds: Map[String, Set[Int]], familyOf: Map[String, Int],
                                 scopes: Map[Int, FamilyScope], bestOf: Map[String, Accepted])

  @tailrec private def grow(matchedIds: Map[String, Set[Int]], familyOf: Map[String, Int]): Round = {
    val scopes = nodes.groupBy(node => familyOf(node.id)).map { case (family, members) => family -> new FamilyScope(members.sortBy(_.id), scoring) }
    val bestOf = nodes.groupBy(node => familyOf(node.id)).toSeq.flatMap { case (family, members) =>
      val scope = scopes(family)
      withoutSiblingDenials(members, scope, members.flatMap(node => acceptance.alone(scope.of(node)).map(node.id -> _)).toMap)
    }.toMap
    val grown = nodes.map(node => node.id -> (matchedIds.getOrElse(node.id, Set.empty[Int]) ++ bestOf.get(node.id).map(_._1.candidate.tmdbId) ++
      pinnedFilm.get(node.id))).toMap
    if (grown == matchedIds || narrow) Round(grown, familyOf, scopes, bestOf)
    else grow(grown, familiesOf(grown))
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
      candidateById.get(film).exists { candidate =>
        evidenceDenies(node.evidence.measured, candidate.film, IdentityMeasures.listingFilm(node.evidence.measured, candidate.film, None, 0, 0, houses, scopeOf(node).qualifiers))
      }
    })(_.denied)
}
