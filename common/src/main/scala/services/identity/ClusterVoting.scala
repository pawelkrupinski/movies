package services.identity

import services.identity.Scored.Accepted

/** GROUP-LEVEL VOTING for the clusters no member matched alone: a facts-free cluster in a split
 *  title family follows the family's clear majority ([[familyMajority]]); every other cluster votes
 *  on its members' pooled evidence ([[vote]]). */
private[identity] final class ClusterVoting(scoring: CandidateScoring, families: Families, acceptance: Acceptance) {
  import scoring.calibration
  import scoring.generation.ownSearch
  import acceptance.weights

  /** Does `n`'s own title evidence name `film`: its title searches returned it, or its title (a
   *  whole spelling, its original title or a segment) names the film's and not another
   *  instalment of its series (`IdentityMeasures.namesFilm`)? */
  private def titleNames(n: EvidenceNode, film: Candidate): Boolean =
    ownSearch(n.id).contains(film.tmdbId) || IdentityMeasures.namesFilm(n.evidence.measured, film.film)

  /** The group vote over a cluster's POOLED scoring: the accepted film — but a film no member's
   *  title names, which only a credited director's filmography reached, only when nothing else
   *  the walk reached fits the pooled facts as well: every rival's own facts fit worse or equally,
   *  and the calibration rates it strictly lower. A walk is a path to candidates; it cannot pick
   *  among a director's films the listing's facts favour another of (a lecture on "Trzy kolory:
   *  Niebieski" is not "Czerwony"), or that the calibration cannot tell apart. */
  private def votedFor(cluster: Seq[EvidenceNode], ranked: Seq[Scored]): Option[Accepted] =
    acceptance.pooled(ranked).filter { case (s, _) =>
      cluster.exists(titleNames(_, s.c)) ||
        ranked.filterNot(r => r.denied || (r eq s)).forall(r => r.p < s.p && weights.own(r) <= weights.own(s))
    }

  /** The GROUP VOTE of a cluster no member matched alone: each voting member → the film and its
   *  confidence. The pooled scoring marks a film denied when ANY member's own evidence denies it
   *  (`FamilyScope.pooled`). When that vetoes the cluster's best film but the cluster's POOLED
   *  facts (the modal year, the median runtime, every director — weighted by listings) still carry
   *  it, the denying members are split off instead — the rest vote without them, and their
   *  denial becomes a cannot-link to the film the rest take ("denies-film"), so no cluster holds both. The rest take the film only
   *  when their OWN pooled facts carry it past the calibration's cut by themselves
   *  ([[EvidenceWeights.carriedByOwnFacts]]): bare siblings never outvote a member's denial on a
   *  title and the database's ranking. */
  def vote(cluster: Seq[EvidenceNode], scope: FamilyScope): Seq[(String, (Int, Double))] = {
    def to(voters: Seq[EvidenceNode], accepted: Accepted) = voters.map(n => n.id -> (accepted._1.c.tmdbId, accepted._2))
    val ranked = scope.pooled(cluster)
    votedFor(cluster, ranked).map(to(cluster, _)).getOrElse(ranked.headOption.filter(s => s.denied && weights.carriedByOwnFacts(s)).toSeq.flatMap { vetoed =>
      val rest = cluster.filterNot(n => scope.of(n).exists(o => o.c.tmdbId == vetoed.c.tmdbId && o.denied))
      Option.when(rest.nonEmpty && rest.size < cluster.size)(rest).flatMap(rest => votedFor(rest, scope.pooled(rest))
        .filter { case (s, _) => s.c.tmdbId == vetoed.c.tmdbId && weights.carriedByOwnFacts(s) }
        .map(to(rest, _))).getOrElse(Nil)
    })
  }

  /** The film a cluster of listings that publish NOTHING but their titles takes from its TITLE
   *  FAMILY — the siblings outside it a title must-link of round A joins it to (`titleEdges`,
   *  [[ConstraintEdges.TitleTiers]]) that accepted a film on their own evidence — when those
   *  siblings are split across films. A bare "Sense and Sensibility" beside 836 venues' "Sense and
   *  Sensibility (2026) {Oakley}" and 9 venues' "(1995) {Ang Lee}" has no evidence of its own for
   *  either: the database's ranking prefers the older, the listings around it the current release.
   *
   *  It takes the family's majority film only when the majority is CLEAR: the one-sided 95%
   *  Wilson lower bound ([[RateBounds.lower95]]) of that film's share of the family's venues —
   *  venues, as the calibration counts units, and the cluster's own venues among them as NOT the
   *  majority's, since nothing they publish says so — clears the calibration's show-ratings cut,
   *  which is the probability it is filed at. A family too thin to outweigh the cluster decides
   *  nothing, and the cluster votes on its pooled evidence as before: 78 Cineworld venues' bare
   *  "The Omen" beside 2 venues' credited 1976 film and 4 venues' 2006 one is the 50th-anniversary
   *  re-release, and 4 of 84 is no majority. Siblings whose titles name a season do not count
   *  (Kino Amok's bare "Manon", a Met broadcast, beside 16 venues' "RBO Sezon Kinowy 2026-27:
   *  Manon"). `None` also when a member publishes a fact or the siblings hold fewer than two films. */
  def familyMajority(cluster: Seq[EvidenceNode], members: Seq[EvidenceNode], accepted: Map[String, Int],
                     titleEdges: Seq[ResolverEdge]): Option[(Int, Double, String)] =
    Option.when(!cluster.exists(_.evidence.measured.publishesAFact)) {
      val inside   = cluster.map(_.id).toSet
      val linked   = titleEdges.flatMap(e => if (inside(e.a)) Seq(e.b) else if (inside(e.b)) Seq(e.a) else Nil).toSet -- inside
      // A sibling whose title names a SEASON names a house's production of the work, which the
      // bare title does not: its venues say nothing about which house's the bare one is.
      val venuesOf = members.filter(y => linked(y.id) && accepted.contains(y.id) && y.evidence.measured.seasonYear.isEmpty)
        .groupMapReduce(y => accepted(y.id))(_.venues)(_ ++ _)
      Option.when(venuesOf.sizeIs >= 2) {
        val (film, venues) = venuesOf.toSeq.minBy { case (f, vs) => (-vs.size, f) }
        val own   = cluster.flatMap(_.venues).toSet -- venues
        val total = (venuesOf.values.flatten ++ own).toSet.size
        val bound = RateBounds.lower95(venues.size, total)
        Option.when(calibration.showsRatings(bound) && !cluster.exists(families.denies(_, film)))(
          (film, bound, s"title family's majority film $film: ${venues.size} of $total venue(s), at least ${ResolverDecision.percent(bound)}"))
      }.flatten
    }.flatten
}
