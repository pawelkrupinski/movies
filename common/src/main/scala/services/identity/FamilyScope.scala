package services.identity

import services.identity.IdentityMeasures.ListingFilm

import scala.collection.mutable

/** One family's scoring: every member node's candidates — the family's pool — scored on the node's
 *  own evidence ([[of]]), or on a cluster's evidence pooled into one listing ([[pooled]]). */
private[identity] final class FamilyScope(val members: Seq[EvidenceNode], scoring: CandidateScoring,
                                          counted: () => Unit) {
  import scoring.{backing, calibration, evidenceDenies, houses, namesItsSeasonProduction, namesOnlyItsVenue, pins}
  import scoring.generation.{candidateById, ownSearch, ownWalk, sharedOf}

  val pool: Seq[Candidate] = members.flatMap(member => ownSearch(member.id).keys ++ ownWalk(member.id)).distinct.sorted.map(candidateById)
  /** Which pieces of the members' titles are qualifiers — an edition, a banner — rather than
   *  works, learned from how the pool's records bill them (`IdentityMeasures.Qualifiers`): a
   *  record titled only a listing's qualifier does not name it. The family's own pool, so a
   *  family resolves alone as it does among the others. */
  val qualifiers: IdentityMeasures.Qualifiers = IdentityMeasures.Qualifiers.learn(pool.map(_.film))

  /** Every candidate `l` has an evidence path to, scored; `denies` marks the ones its own
   *  evidence rules out (`ListingConstraints.learnedListingFilm`), which are never eligible. */
  def score(listing: IdentityMeasures.Listing, venue: String, ranks: Map[Int, Int], walked: Set[Int], shared: Set[Int],
            deniedByPins: Int => Boolean): Seq[Scored] = {
    counted()
    val relation  = pool.map(candidate => candidate.tmdbId -> IdentityMeasures.titleRelation(listing, candidate.film, houses, qualifiers).value).toMap
    val reachable = pool.filter(candidate => ranks.contains(candidate.tmdbId) || walked(candidate.tmdbId) || shared(candidate.tmdbId) ||
      IdentityMeasures.names(relation(candidate.tmdbId), listing, candidate.film))
    val close     = reachable.count(candidate => IdentityMeasures.Rivalling(relation(candidate.tmdbId)))
    val groups    = IdentityMeasures.titleGroups(listing)
    val candidates = reachable.map { candidate =>
      val rivals   = close - (if (IdentityMeasures.Rivalling(relation(candidate.tmdbId))) 1 else 0)
      val measures = IdentityMeasures.listingFilm(listing, candidate.film, ranks.get(candidate.tmdbId), rivals,
        backing.corroborating(groups, candidate.film, venue), houses, qualifiers)
      val probability = calibration.probability(ListingFilm, measures)
      Scored(candidate, probability, measures, deniedByPins(candidate.tmdbId) || evidenceDenies(listing, candidate.film, measures), listing, ranks.get(candidate.tmdbId),
        namesItsSeasonProduction(listing, candidate.film), deniedByPins(candidate.tmdbId), IdentityMeasures.billsUnderItsHouse(listing, candidate.film, houses))
    }
    // The listing's whole title (or its original or an alternative title) and its credited
    // director name ONE film together: another film of that director, which its title does not
    // name, is not the listing's — however its runtime or year fits. A director's filmography is a path to candidates, never a reason to leave
    // the one its title names (Syndicated's "Zodiac", Fincher, 139 minutes, is not Fight Club).
    val titled = candidates.exists(scored => !scored.denied && IdentityMeasures.Rivalling(relation(scored.candidate.tmdbId)) && IdentityMeasures.sameDirector(scored.measures))
    // The listing's title numbers its instalment as an eligible record of its series does
    // (`numeral` `same`): another instalment of that series — one the listing numbers and it
    // does not, or numbers otherwise — is another film, however its crew or runtime fits (Kinoteka's
    // "Niesamowite przygody skarpetek 4. Do roboty! – zestaw" is part 4, not the 2025 first set
    // whose animators it credits).
    val instalment = candidates.exists(scored => !scored.denied && scored.category("numeral").contains("same"))
    candidates.map(scored =>
      if (titled && !IdentityMeasures.NamingRelations(relation(scored.candidate.tmdbId)) && IdentityMeasures.sameDirector(scored.measures)) scored.copy(denied = true)
      else if (instalment && scored.category("numeral").exists(IdentityMeasures.OtherInstalment)) scored.copy(denied = true)
      else scored)
      .sortBy(scored => (-scored.probability, scored.candidate.tmdbId))
  }

  private val memo = mutable.HashMap.empty[String, Seq[Scored]]
  def of(node: EvidenceNode): Seq[Scored] = memo.getOrElseUpdate(node.id,
    score(node.evidence.measured, node.venue, ownSearch(node.id), ownWalk(node.id), sharedOf(node),
      id => pins.deniedFilms(node.listings.head.key)(id) || namesOnlyItsVenue(node, candidateById(id))))

  /** The cluster's members read as ONE listing: the title most of its listings carry (the
   *  smaller node on a tie), the year most of them publish (a title's bracket or season stays the lead title's own measure), every director and country, the
   *  median runtime, the modal original title, and every candidate any of them named. The
   *  directors credited beside that year are only those of the members publishing it. */
  def pooled(cluster: Seq[EvidenceNode]): Seq[Scored] = pooledMemo.getOrElseUpdate(cluster.map(_.id), scorePooled(cluster))
  // Voting, the vote on a cluster's rest and the decisions each pool the same clusters again.
  private val pooledMemo = mutable.HashMap.empty[Seq[String], Seq[Scored]]

  private def scorePooled(cluster: Seq[EvidenceNode]): Seq[Scored] = {
    def modal[A: Ordering](values: Seq[(A, Int)]): Option[A] =
      values.groupMapReduce(_._1)(_._2)(_ + _).toSeq.sortBy { case (value, weight) => (-weight, value) }.headOption.map(_._1)
    val lead     = cluster.minBy(node => (-node.weight, node.id))
    val runtimes = cluster.flatMap(node => node.evidence.runtime.toSeq.flatMap(runtime => Seq.fill(node.weight)(runtime))).sorted
    val year     = modal(cluster.flatMap(node => node.evidence.year.map(_ -> node.weight)))
    val listing  = lead.evidence.measured.copy(
      year          = year,
      yearCredits   = Some(cluster.filter(node => year.nonEmpty && node.evidence.year == year).flatMap(_.evidence.directors).distinct.sorted),
      originalTitle = modal(cluster.flatMap(node => node.evidence.originalTitle.map(_ -> node.weight))),
      directors     = cluster.flatMap(_.evidence.directors).distinct.sorted,
      runtime       = runtimes.lift(runtimes.size / 2),
      countries     = cluster.flatMap(_.evidence.countries).distinct.sorted)
    val ranks = cluster.flatMap(node => ownSearch(node.id)).groupMapReduce(_._1)(_._2)(math.min)
    score(listing, lead.venue, ranks, cluster.flatMap(node => ownWalk(node.id)).toSet, cluster.flatMap(sharedOf).toSet,
      id => cluster.exists(node => pins.deniedFilms(node.listings.head.key)(id) || namesOnlyItsVenue(node, candidateById(id))))
      .map(scored => if (scored.denied || cluster.forall(node => !of(node).exists(other => other.candidate.tmdbId == scored.candidate.tmdbId && other.denied))) scored else scored.copy(denied = true))
  }
}
