package services.identity

import services.identity.IdentityMeasures.ListingFilm
import services.identity.Scored.Accepted
import services.movies.TitleNormalizer

import scala.annotation.tailrec

/** The FAMILIES: the closure over title keys and the films members accept, grown until stable —
 *  a node matching a film another family's listings match joins that family, and its pool. Each
 *  round scores every family ([[FamilyScope]]) and takes what each node accepts ALONE
 *  ([[Acceptance.aloneNamed]]), unless a title-linked sibling denies it ([[withoutSiblingDenials]]).
 *  `narrow` is the teeth tests' `Mutation.NarrowFamilies`: families by sanitised title only, never grown. */
private[identity] final class Families(scoring: CandidateScoring, acceptance: Acceptance, links: TitleLinks,
                                       normalizer: TitleNormalizer, narrow: Boolean) {
  import scoring.{evidenceDenies, houses, pins}
  import scoring.generation.{candidateOf, nodeById, nodes}

  val pinnedFilm: Map[String, Int] = nodes.flatMap(node => pins.filmOf(node.listings.head.key).map(node.id -> _)).toMap

  /** A node's block keys: its title keys and the films `ids` holds it matched. */
  private def keysOf(node: EvidenceNode, ids: Map[String, Set[Int]]): Set[String] =
    links.titleKeys(node) ++ ids.getOrElse(node.id, Set.empty[Int]).map(filmId => s"id:${filmId}")

  private def familiesOf(ids: Map[String, Set[Int]]): Map[String, Int] =
    FamilyClosure.families(nodes.map(node => node.id -> (
      if (narrow) Set("t:" + normalizer.sanitize(node.evidence.cleanTitle)) else keysOf(node, ids))).toMap)

  /** A node's ALONE acceptance, withdrawn when a title-linked sibling that accepted nothing itself
   *  DENIES the film: a bare "Samson i Dalila" beside the same venue family's "…: live in hd
   *  2026/27" has no evidence of its own against DeMille's 1949 film, but its sibling's season
   *  is — or a sibling that took the film the node's title names more closely ([[siblingDenying]]).
   *  The node then goes to the group vote with its siblings, where every member's denial holds. */
  private def withoutSiblingDenials(members: Seq[EvidenceNode], scope: FamilyScope,
                                    alone: Map[String, Accepted]): Map[String, Accepted] =
    alone.filter { case (id, (best, _)) => deniedBySibling(nodeById(id), members, scope, best, alone.get(_).map(_._1)).isEmpty }

  /** The title-linked sibling, itself taken by no rule alone, whose own evidence denies `best`'s film — and its denial:
   *  why a node's own match is withdrawn ([[withoutSiblingDenials]]), which the node's trace says. A denial by the
   *  probability cut alone ([[Scored.deniedByCutOnly]]) — the sibling's title and facts reading low, no rule — withdraws
   *  it only when the sibling compared a fact the node did not publish: the year a yearless listing lacks, a director
   *  beside a bare title. When the node published every fact the sibling did, its own reading of them is the answer:
   *  UK "Fallen Angels by Noel Coward" (×95), the 2026 recording by its credited screen director and its runtime, lost
   *  it to the "…by Noël Coward" spelling crediting only the stage director at a longer runtime.
   *
   *  A node whose own title names the record's production — its house by its banner and its season
   *  ([[Scored.seasonProduction]] and [[StageRelays.billsItsHouse]]), or its relay's record of its house, work and
   *  season ([[RelayStage.particularRecordOf]]: US "MetOpera: Carmen (2009)" beside Lark Theater's "MetOpera: Carmen (2023-24)") —
   *  is withdrawn by no sibling: one house stages a work once a season, so a sibling's denial (another season, another
   *  house, other credits) says the sibling is another production, never that the node is. And a node whose banner alone bills the record under its house
   *  ([[Scored.houseProduction]]) is withdrawn by no sibling denying the record as ANOTHER house's season production
   *  ([[CandidateScoring.AnotherHousesSeason]]): that denial is about the sibling's banner, not the work — two houses'
   *  stagings are two productions, and a banner wrongly learned as another house denies nothing. UK, run 38355584482:
   *  Flicks' "RBO Cinema Season 2026-27: Macbeth" ×34 (RBO's cinema season relays the Met) withdrew Savoy's "The
   *  Metropolitan Opera: Macbeth" ×6 from the Met's 2026/27 record, and its Fanciulla billing (the original title Vue's
   *  feed carries) withdrew Vue's "MET Opera 2026-27: La Fanciulla Del West" ×81 from the Met's 2026/27 Fanciulla.
   *
   *  A sibling a rule took alone (`takenAlone`: its own take) withdraws nothing — its denial says it is another film, not
   *  that the node is — unless the film it took is one the node's own title names exactly while the node's take reads its
   *  title otherwise (another country's title, the original one) and no other venue's listing corroborates that take,
   *  and its denial compared a fact the node did not publish. The node's title then names the sibling's film no less
   *  than its own take, and nothing singles that take out: DE CinemaxX's bare "Titans" ×3 took "Remember the Titans"
   *  (2000), whose Danish title it is, beside CinemaxX Essen's "Titans" crediting the 2026 Malayalam film's director —
   *  and back whenever no untaken sibling denied it (recording 573; the drift replay's seed-1 flip of convergence
   *  #2375). A corroborated take stands: Werkstattkino's bare "CAMOUFLAGE" keeps the 2026 "Камуфляж", which four venues
   *  bill, beside Kino Krokodil's "Camouflage" [2016] crediting Imge Özbilge. So does a take of the film the node's own
   *  feed links, its link standing ([[FamilyScope.overruled]]): DE Metro-Kino im Schloßhof's 96′ "Billie", which
   *  Cineamo links to the 2020 Billie Holiday documentary, beside Hansa Kino's "Billie" crediting Sheri Hagen's 2026
   *  film (recording 573).
   *
   *  Nor does a sibling's denial that the film is the WORK its billing names, one a credit overrules
   *  ([[Denial.Kind.NotTheWork]] `creditOverrules`: a screen namesake of its stage house's work), withdraw a node crediting
   *  the film's director: the rule spares the node's own billing so, and a sibling crediting nobody says nothing of its
   *  credit. UK red 629 (seed 42, recording 573): Keswick Alhambra's "Scottish Ballet: These Violent Delights" {Christopher
   *  Hampson, Oscar Sansom} [95′] lost the ballet's 2026 record whenever Kiln Kilburn's uncredited billing of it arrived —
   *  the pair then voted together, Vetoed, its pooled runtime off the record's — and took it back once a third billing's
   *  facts carried the pair: a take hanging on the arrival order. */
  def deniedBySibling(node: EvidenceNode, members: Seq[EvidenceNode], scope: FamilyScope, best: Scored,
                      takenAlone: String => Option[Scored]): Option[(EvidenceNode, String)] =
    siblingDenying(node, members, scope, best, takenAlone).map { case (sibling, other) => sibling -> other.denial.fold("denied")(_.why) }

  /** [[deniedBySibling]]'s sibling and its own scoring of the film, which holds the denial. */
  def siblingDenying(node: EvidenceNode, members: Seq[EvidenceNode], scope: FamilyScope, best: Scored,
                     takenAlone: String => Option[Scored]): Option[(EvidenceNode, Scored)] = {
    lazy val ownFacts = IdentityMeasures.comparedFacts(ListingFilm, best.measures).keySet
    def comparedUnpublished(denial: Scored) =
      (IdentityMeasures.comparedFacts(ListingFilm, denial.measures).keySet -- ownFacts).nonEmpty
    // the provenance table's withdrawal rows (Authority.withdraws): the sibling's provenance may, and the node's own
    // statement does not answer the denial; then a weak denial (the cut alone, or a sibling's own take) only by a fact
    // the node did not publish
    def withdraws(sibling: EvidenceNode, denial: Scored, byTaken: Boolean) =
      Authority.withdraws(sibling, denial, best) && (!(denial.deniedByCutOnly || byTaken) || comparedUnpublished(denial))
    // the node's take read by another title, corroborated by no other venue nor linked by its own feed: only then may a
    // sibling taken alone withdraw it
    lazy val readOtherwise = !best.category("title").contains("exact") && best.number("venues.corroborating").forall(_ <= 0) &&
      !(scope.feedLinked(node).exists(_._2 == best.candidate.tmdbId) && scope.overruled(node).isEmpty)
    def namedCloser(taken: Scored) = readOtherwise && taken.candidate.tmdbId != best.candidate.tmdbId &&
      scope.of(node).exists(own => own.candidate.tmdbId == taken.candidate.tmdbId && own.category("title").contains("exact"))
    if (Authority.sparesEvery(best)) None
    else members.iterator.filter(_.id != node.id)
      .flatMap { sibling =>
        val taken = takenAlone(sibling.id)
        // the title link last: a sibling taken alone almost never may withdraw, and a family can hold thousands of them
        if (!taken.forall(namedCloser) || !links.titleLinked(node, sibling)) None
        else scope.of(sibling).find(other => other.candidate.tmdbId == best.candidate.tmdbId && other.denied &&
          withdraws(sibling, other, taken.isDefined)).map(other => sibling -> other)
      }
      .nextOption()
  }

  /** How many listings every scope of every round scored ([[Resolution.scorings]]) — counted, not
   *  read off the scopes, so a round's replaced scopes and their scores are not kept alive. */
  private var listingsScored = 0
  def scorings: Int = listingsScored
  /** How many listing-film title relations the scopes read rather than shared ([[Resolution.titleRelations]]). */
  private var relationsRead = 0
  def titleRelations: Int = relationsRead

  private final case class Round(matchedIds: Map[String, Set[Int]], familyOf: Map[String, Int],
                                 scopes: Map[Int, FamilyScope], bestOf: Map[String, Accepted])

  /** Each round scores every family, but a family the last round left as it was scores as it did:
   *  its scope (and the node scores it memoises) is kept, keyed by its members. Only the families a
   *  round merged are scored again. */
  @tailrec private def grow(matchedIds: Map[String, Set[Int]], familyOf: Map[String, Int],
                            scored: Map[Seq[String], FamilyScope] = Map.empty): Round = {
    val grouped = nodes.groupBy(node => familyOf(node.id))
    val scopes = grouped.map { case (family, members) =>
      val sorted = members.sortBy(_.id)
      family -> scored.getOrElse(sorted.map(_.id), new FamilyScope(sorted, scoring, acceptance, () => listingsScored += 1, () => relationsRead += 1))
    }
    val bestOf = grouped.toSeq.flatMap { case (family, members) =>
      val scope = scopes(family)
      withoutSiblingDenials(members, scope, members.flatMap(node => scope.takenAlone(node).map(taken => node.id -> taken._1)).toMap)
    }.toMap
    val grown = nodes.map(node => node.id -> (matchedIds.getOrElse(node.id, Set.empty[Int]) ++ bestOf.get(node.id).map(_._1.candidate.tmdbId) ++
      pinnedFilm.get(node.id))).toMap
    if (grown == matchedIds || narrow) Round(grown, familyOf, scopes, bestOf)
    else {
      // the films taken merged no families: the next round would score the very same scopes (kept by their members)
      // to the very same takes, so this one is already stable (6,004 of the five captures' 6,005 resolves end here)
      val regrouped = familiesOf(grown)
      if (regrouped == familyOf) Round(grown, familyOf, scopes, bestOf)
      else grow(grown, regrouped, scopes.values.map(scope => scope.members.map(_.id) -> scope).toMap)
    }
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
    nodes.map(node => node.id -> keysOf(node, round.matchedIds)).toMap

  def scopeOf(node: EvidenceNode): FamilyScope = scopes(familyOf(node.id))

  /** Does `node`'s page read as REUSED ([[Acceptance.copiedBeyondItsBilling]]): a record its copied original title and the
   *  director it credits name stands in its pool, though its billing says more than that record's titles? Its facts are
   *  then the record's, copied onto the page, not its billing's film's. */
  def readsAsReused(node: EvidenceNode): Boolean = scopeOf(node).of(node).exists(acceptance.copiedBeyondItsBilling)

  /** Does `n`'s evidence deny `film` — scored, or, for a film it has no evidence path to, its own
   *  evidence against the film's record? */
  def denies(node: EvidenceNode, film: Int): Boolean =
    scopeOf(node).of(node).find(_.candidate.tmdbId == film).fold(pins.deniedFilms(node.listings.head.key)(film) || {
      candidateOf(film).exists { candidate =>
        val stated = scopeOf(node).evidenceOf(node).measured
        evidenceDenies(stated, candidate.film, IdentityMeasures.listingFilm(stated, candidate.film, None, 0, 0, houses, scopeOf(node).qualifiers,
          node.country))
      }
    })(_.denied)
}
