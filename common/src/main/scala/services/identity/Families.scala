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

  val pinnedFilm: Map[String, Int] = nodes.flatMap(n => pins.filmOf(n.listings.head.key).map(n.id -> _)).toMap

  private def familiesOf(ids: Map[String, Set[Int]]): Map[String, Int] =
    FamilyClosure.families(nodes.map(n => n.id -> (
      if (narrow) Set("t:" + normalizer.sanitize(n.evidence.cleanTitle))
      else links.titleKeys(n) ++ ids.getOrElse(n.id, Set.empty[Int]).map(i => s"id:$i"))).toMap)

  /** A node's ALONE acceptance, withdrawn when a title-linked sibling that accepted nothing itself
   *  DENIES the film: a bare "Samson i Dalila" beside the same venue family's "…: live in hd
   *  2026/27" has no evidence of its own against DeMille's 1949 film, but its sibling's season
   *  is. The node then goes to the group vote with its siblings, where every member's denial holds. */
  private def withoutSiblingDenials(members: Seq[EvidenceNode], scope: FamilyScope,
                                    alone: Map[String, Accepted]): Map[String, Accepted] =
    alone.filter { case (id, (best, _)) =>
      val n = nodeById(id)
      !members.exists(y => y.id != id && !alone.contains(y.id) && links.titleLinked(n, y) &&
        scope.of(y).exists(o => o.c.tmdbId == best.c.tmdbId && o.denied))
    }

  private final case class Round(matchedIds: Map[String, Set[Int]], familyOf: Map[String, Int],
                                 scopes: Map[Int, FamilyScope], bestOf: Map[String, Accepted])

  @tailrec private def grow(matchedIds: Map[String, Set[Int]], familyOf: Map[String, Int]): Round = {
    val scopes = nodes.groupBy(n => familyOf(n.id)).map { case (f, ms) => f -> new FamilyScope(ms.sortBy(_.id), scoring) }
    val bestOf = nodes.groupBy(n => familyOf(n.id)).toSeq.flatMap { case (f, members) =>
      val scope = scopes(f)
      withoutSiblingDenials(members, scope, members.flatMap(n => acceptance.alone(scope.of(n)).map(n.id -> _)).toMap)
    }.toMap
    val grown = nodes.map(n => n.id -> (matchedIds.getOrElse(n.id, Set.empty[Int]) ++ bestOf.get(n.id).map(_._1.c.tmdbId) ++
      pinnedFilm.get(n.id))).toMap
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
    nodes.map(n => n.id -> (links.titleKeys(n) ++ round.matchedIds.getOrElse(n.id, Set.empty[Int]).map(i => s"id:$i"))).toMap

  def scopeOf(n: EvidenceNode): FamilyScope = scopes(familyOf(n.id))

  /** Does `n`'s evidence deny `film` — scored, or, for a film it has no evidence path to, its own
   *  evidence against the film's record? */
  def denies(n: EvidenceNode, film: Int): Boolean =
    scopeOf(n).of(n).find(_.c.tmdbId == film).fold(pins.deniedFilms(n.listings.head.key)(film) || {
      candidateById.get(film).exists { c =>
        evidenceDenies(n.evidence.measured, c.film, IdentityMeasures.listingFilm(n.evidence.measured, c.film, None, 0, 0, houses, scopeOf(n).qualifiers))
      }
    })(_.denied)
}
