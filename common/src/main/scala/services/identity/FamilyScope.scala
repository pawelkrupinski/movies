package services.identity

import services.identity.IdentityMeasures.ListingFilm

import scala.collection.mutable

/** One family's scoring: every member node's candidates — the family's pool — scored on the node's
 *  own evidence ([[of]]), or on a cluster's evidence pooled into one listing ([[pooled]]). */
private[identity] final class FamilyScope(members: Seq[EvidenceNode], scoring: CandidateScoring) {
  import scoring.{backing, calibration, evidenceDenies, houses, namesItsSeasonProduction, namesOnlyItsVenue, pins}
  import scoring.generation.{candidateById, ownSearch, ownWalk, sharedOf}

  val pool: Seq[Candidate] = members.flatMap(m => ownSearch(m.id).keys ++ ownWalk(m.id)).distinct.sorted.map(candidateById)
  /** Which pieces of the members' titles are qualifiers — an edition, a banner — rather than
   *  works, learned from how the pool's records bill them (`IdentityMeasures.Qualifiers`): a
   *  record titled only a listing's qualifier does not name it. The family's own pool, so a
   *  family resolves alone as it does among the others. */
  val qualifiers: IdentityMeasures.Qualifiers = IdentityMeasures.Qualifiers.learn(pool.map(_.film))

  /** Every candidate `l` has an evidence path to, scored; `denies` marks the ones its own
   *  evidence rules out (`ListingConstraints.learnedListingFilm`), which are never eligible. */
  def score(l: IdentityMeasures.Listing, venue: String, ranks: Map[Int, Int], walked: Set[Int], shared: Set[Int],
            deniedByPins: Int => Boolean): Seq[Scored] = {
    val relation  = pool.map(c => c.tmdbId -> IdentityMeasures.titleRelation(l, c.film, houses, qualifiers).value).toMap
    val reachable = pool.filter(c => ranks.contains(c.tmdbId) || walked(c.tmdbId) || shared(c.tmdbId) ||
      IdentityMeasures.names(relation(c.tmdbId), l, c.film))
    val close     = reachable.count(c => IdentityMeasures.Rivalling(relation(c.tmdbId)))
    val groups    = IdentityMeasures.titleGroups(l)
    val scored = reachable.map { c =>
      val rivals   = close - (if (IdentityMeasures.Rivalling(relation(c.tmdbId))) 1 else 0)
      val measures = IdentityMeasures.listingFilm(l, c.film, ranks.get(c.tmdbId), rivals,
        backing.corroborating(groups, c.film, venue), houses, qualifiers)
      val p = calibration.probability(ListingFilm, measures)
      Scored(c, p, measures, deniedByPins(c.tmdbId) || evidenceDenies(l, c.film, measures), l, ranks.get(c.tmdbId),
        namesItsSeasonProduction(l, c.film), deniedByPins(c.tmdbId), IdentityMeasures.billsUnderItsHouse(l, c.film, houses))
    }
    // The listing's whole title (or its original or an alternative title) and its credited
    // director name ONE film together: another film of that director, which its title does not
    // name, is not the listing's — however its runtime or year fits. A director's filmography is a path to candidates, never a reason to leave
    // the one its title names (Syndicated's "Zodiac", Fincher, 139 minutes, is not Fight Club).
    val titled = scored.exists(s => !s.denied && IdentityMeasures.Rivalling(relation(s.c.tmdbId)) && IdentityMeasures.sameDirector(s.measures))
    // The listing's title numbers its instalment as an eligible record of its series does
    // (`numeral` `same`): another instalment of that series — one the listing numbers and it
    // does not, or numbers otherwise — is another film, however its crew or runtime fits (Kinoteka's
    // "Niesamowite przygody skarpetek 4. Do roboty! – zestaw" is part 4, not the 2025 first set
    // whose animators it credits).
    val instalment = scored.exists(s => !s.denied && s.category("numeral").contains("same"))
    scored.map(s =>
      if (titled && !IdentityMeasures.NamingRelations(relation(s.c.tmdbId)) && IdentityMeasures.sameDirector(s.measures)) s.copy(denied = true)
      else if (instalment && s.category("numeral").exists(IdentityMeasures.OtherInstalment)) s.copy(denied = true)
      else s)
      .sortBy(s => (-s.p, s.c.tmdbId))
  }

  private val memo = mutable.HashMap.empty[String, Seq[Scored]]
  def of(n: EvidenceNode): Seq[Scored] = memo.getOrElseUpdate(n.id,
    score(n.evidence.measured, n.venue, ownSearch(n.id), ownWalk(n.id), sharedOf(n.id),
      id => pins.deniedFilms(n.listings.head.key)(id) || namesOnlyItsVenue(n, candidateById(id).film)))

  /** The cluster's members read as ONE listing: the title most of its listings carry (the
   *  smaller node on a tie), the year most of them publish (a title's bracket or season stays the lead title's own measure), every director and country, the
   *  median runtime, the modal original title, and every candidate any of them named. The
   *  directors credited beside that year are only those of the members publishing it. */
  def pooled(cluster: Seq[EvidenceNode]): Seq[Scored] = {
    def modal[A: Ordering](values: Seq[(A, Int)]): Option[A] =
      values.groupMapReduce(_._1)(_._2)(_ + _).toSeq.sortBy { case (v, w) => (-w, v) }.headOption.map(_._1)
    val lead     = cluster.minBy(n => (-n.weight, n.id))
    val runtimes = cluster.flatMap(n => n.evidence.runtime.toSeq.flatMap(r => Seq.fill(n.weight)(r))).sorted
    val year     = modal(cluster.flatMap(n => n.evidence.year.map(_ -> n.weight)))
    val listing  = lead.evidence.measured.copy(
      year          = year,
      yearCredits   = Some(cluster.filter(n => year.nonEmpty && n.evidence.year == year).flatMap(_.evidence.directors).distinct.sorted),
      originalTitle = modal(cluster.flatMap(n => n.evidence.originalTitle.map(_ -> n.weight))),
      directors     = cluster.flatMap(_.evidence.directors).distinct.sorted,
      runtime       = runtimes.lift(runtimes.size / 2),
      countries     = cluster.flatMap(_.evidence.countries).distinct.sorted)
    val ranks = cluster.flatMap(n => ownSearch(n.id)).groupMapReduce(_._1)(_._2)(math.min)
    score(listing, lead.venue, ranks, cluster.flatMap(n => ownWalk(n.id)).toSet, cluster.flatMap(n => sharedOf(n.id)).toSet,
      id => cluster.exists(n => pins.deniedFilms(n.listings.head.key)(id) || namesOnlyItsVenue(n, candidateById(id).film)))
      .map(s => if (s.denied || cluster.forall(n => !of(n).exists(o => o.c.tmdbId == s.c.tmdbId && o.denied))) s else s.copy(denied = true))
  }
}
