package services.identity

import services.identity.Scored.Accepted

/** GROUP-LEVEL VOTING for the clusters no member matched alone: a facts-free cluster in a split
 *  title family follows the family's clear majority ([[familyMajority]]); every other cluster votes
 *  on its members' pooled evidence ([[vote]]). */
private[identity] final class ClusterVoting(scoring: CandidateScoring, families: Families, acceptance: Acceptance) {
  import scoring.calibration
  import acceptance.weights

  /** Does `n`'s own title evidence name `film`: its title searches returned it, or its title (a
   *  whole spelling, its original title or a segment) names the film's and not another
   *  instalment of its series (`IdentityMeasures.namesFilm`)? */
  private def titleNames(node: EvidenceNode, film: Candidate): Boolean = {
    val scope = families.scopeOf(node)
    scope.reach(node).search.contains(film.tmdbId) || IdentityMeasures.namesFilm(scope.evidenceOf(node).measured, film.film)
  }

  /** The group vote over a cluster's POOLED scoring: the accepted film — but a film no member's
   *  title names, which only a credited director's filmography reached, only when nothing else
   *  the walk reached fits the pooled facts as well: every rival's own facts fit worse or equally,
   *  and the calibration rates it strictly lower. A walk is a path to candidates; it cannot pick
   *  among a director's films the listing's facts favour another of (a lecture on "Trzy kolory:
   *  Niebieski" is not "Czerwony"), or that the calibration cannot tell apart. */
  private def votedFor(cluster: Seq[EvidenceNode], ranked: Seq[Scored]): Option[Accepted] =
    acceptance.pooled(ranked).filter { case (winner, _) =>
      !onlyWithdrawn(cluster, winner.candidate.tmdbId) && (cluster.exists(titleNames(_, winner.candidate)) ||
        ranked.filterNot(rival => rival.denied || (rival eq winner)).forall(rival => rival.probability < winner.probability && weights.own(rival) <= weights.own(winner)))
    }

  /** Is `film` one EVERY member's own match was WITHDRAWN from ([[Families.deniedBySibling]]) by a sibling's denial that
   *  it is not the WORK billed ([[Denial.Kind.NotTheWork]]: a short beside the sibling's feed link to a feature of the
   *  title; a screen namesake of the work the sibling's stage house bills, unless the member credits the film's director,
   *  as that rule spares its own listing — UK "These Violent Delights" {Christopher Hampson} ×10 keeps Scottish Ballet's
   *  record, recording 573)? The withdrawn node goes to the group vote so
   *  that the sibling's denial holds there; but the solver may split the denying sibling off, and a cluster of such
   *  withdrawn nodes alone would then vote the film straight back on their own evidence, the very match the denial
   *  withdrew. Such a cluster votes no film it was withdrawn from. A denial by the sibling's own facts or billing (another
   *  year, the director its title names) says only that the sibling is another film: DE "Glaube" ×7 (withdrawn from the
   *  2025 film by 'Glaube' [1998]) and "ES - Kapitel 1" (by 'ES (ULRICH SCHAMONI)') still vote theirs — every withdrawal
   *  read so lost 242 DE takes on prod-latest 1622 (run 1777). A cluster holding members that took nothing alone votes the
   *  film only on THEIR pooled evidence, the withdrawn members left out. Nor a node whose own feed links the film (a TMDB-keyed feed's id names it): DE Bali-Kino's
   *  and PUSCHKINO's "Nosferatu", Cineamo-linked to Eggers' 2024 film, beside the "Eine Symphonie des Grauens – Ballett
   *  Extra" billing that denies it as a screen namesake. Prod-latest 1622: UK Magic Lantern Tywyn's and Riverside Woodbridge's "Notre-Dame de
   *  Paris" (the Paris Opera Ballet's relay) re-took the 1956 Hunchback, a screen namesake of the ballet billings' work,
   *  and DE Mal seh'n Kino's "THE THING WITH FEATHERS" re-took Oelofse's 24-minute short (kinoheld's copied credit) beside
   *  the Cineamo link of "Das Ding mit den Federn" to Southern's feature. */
  private def onlyWithdrawn(cluster: Seq[EvidenceNode], film: Int): Boolean = {
    // a member publishing no fact brings no evidence of its own to answer the denial: its title is the others' already,
    // so it neither keeps the vote nor withdraws it (DE Filmwelt Grünstadt's "TITANS AWAKENING" re-took 1690206 whenever
    // the bare "Titans" joined it, and lost it again as the family grew: drift slice dammtor-titans-0bd19a4c)
    val stating   = cluster.filter(_.evidence.measured.publishesAFact)
    lazy val withdrawn = stating.filter(withdrawnFrom(_, film))
    // the members withdrawn from it bring it no vote: the rest vote it back only on their own pooled evidence (UK Plough
    // Torrington's and Quad Derby's "Notre-Dame de Paris", stating runtimes against the 1956 Hunchback and taking nothing
    // alone, re-voted it with Magic Lantern's and Riverside's withdrawn takes in their pool)
    !keptByASibling(cluster, film) && stating.nonEmpty && withdrawn.nonEmpty && (withdrawn.size == stating.size || {
      val rest = cluster.filterNot(withdrawn.contains)
      !acceptance.pooled(families.scopeOf(rest.head).pooled(rest)).exists(_._1.candidate.tmdbId == film)
    })
  }

  /** Was `node`'s own match of `film` WITHDRAWN by a sibling's denial that it is not the work billed ([[onlyWithdrawn]])? */
  private def withdrawnFrom(node: EvidenceNode, film: Int): Boolean = {
    val scope = families.scopeOf(node)
    !families.bestOf.contains(node.id) && !FeedLink.linked(node.listings).exists(_._2 == film) && scope.takenAlone(node).exists { case ((best, _), _) =>
      best.candidate.tmdbId == film && families.siblingDenying(node, scope.members, scope, best,
        id => scope.takenAlone(scoring.generation.nodeById(id)).map(_._1._1)).exists { case (_, denial) =>
        denial.denial.exists(_.kind match {
          case kind @ Denial.Kind.NotTheWork(_) => Authority.spares(best, kind).isEmpty
          case _                                       => false
        }) }
    }
  }

  /** Does a member of the cluster's family keep `film` by its own match ([[Families.bestOf]])? Its own evidence then says
   *  the family's work is screened as that film, which a sibling's denial does not withdraw from the rest: UK recording
   *  573's uncredited Savoy "These Violent Delights" billings beside the credited ones that take Scottish Ballet's record. */
  private def keptByASibling(cluster: Seq[EvidenceNode], film: Int): Boolean =
    cluster.headOption.exists(node => families.scopeOf(node).members.exists(member => families.bestOf.get(member.id).exists(_._1.candidate.tmdbId == film)))

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
    def to(voters: Seq[EvidenceNode], accepted: Accepted) = voters.map(node => node.id -> (accepted._1.candidate.tmdbId, accepted._2))
    val ranked = scope.pooled(cluster)
    votedFor(cluster, ranked).map(to(cluster, _)).getOrElse(ranked.headOption.filter(winner => winner.denied && weights.carriedByOwnFacts(winner)).toSeq.flatMap { vetoed =>
      val rest = cluster.filterNot(node => scope.of(node).exists(other => other.candidate.tmdbId == vetoed.candidate.tmdbId && other.denied))
      Option.when(rest.nonEmpty && rest.size < cluster.size)(rest).flatMap(rest => votedFor(rest, scope.pooled(rest))
        .filter { case (winner, _) => winner.candidate.tmdbId == vetoed.candidate.tmdbId && weights.carriedByOwnFacts(winner) }
        .map(to(rest, _))).getOrElse(Nil)
    })
  }

  /** The films a facts-free cluster's TITLE FAMILY backs by OWN statements, each with the venues backing it: the siblings a
   *  title must-link of round A joins it to ([[familyMajority]]'s) that publish a fact of their own (a year, a director,
   *  a runtime) and took a film ALONE on it (`accepted`), a season's billing apart. Empty for a cluster publishing a
   *  fact: its own evidence decides. What the flip-unification design (docs/investigations/2026-11-01-flip-unification.md,
   *  stage 5) calls a CONTESTED family when it names two films or more: a bare listing there borrows no film by a
   *  majority. Read by the population report only ([[Resolution.contested]]); no take reads it yet. */
  def ownBacked(cluster: Seq[EvidenceNode], members: Seq[EvidenceNode], accepted: Map[String, Int],
                titleEdges: Seq[ResolverEdge]): Map[Int, Set[String]] =
    if (cluster.exists(_.evidence.measured.publishesAFact)) Map.empty
    else {
      val inside = cluster.map(_.id).toSet
      val linked = titleEdges.flatMap(edge => if (inside(edge.a)) Seq(edge.b) else if (inside(edge.b)) Seq(edge.a) else Nil).toSet -- inside
      members.filter(sibling => linked(sibling.id) && accepted.contains(sibling.id) && sibling.evidence.measured.publishesAFact &&
        sibling.evidence.measured.seasonYear.isEmpty).groupMapReduce(sibling => accepted(sibling.id))(_.venues.toSet)(_ ++ _)
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
      val linked   = titleEdges.flatMap(edge => if (inside(edge.a)) Seq(edge.b) else if (inside(edge.b)) Seq(edge.a) else Nil).toSet -- inside
      // A sibling whose title names a SEASON names a house's production of the work, which the
      // bare title does not: its venues say nothing about which house's the bare one is.
      val venuesOf = members.filter(sibling => linked(sibling.id) && accepted.contains(sibling.id) && sibling.evidence.measured.seasonYear.isEmpty)
        .groupMapReduce(sibling => accepted(sibling.id))(_.venues)(_ ++ _)
      Option.when(venuesOf.sizeIs >= 2) {
        val (film, venues) = venuesOf.toSeq.minBy { case (filmId, filmVenues) => (-filmVenues.size, filmId) }
        val own   = cluster.flatMap(_.venues).toSet -- venues
        val total = (venuesOf.values.flatten ++ own).toSet.size
        val bound = RateBounds.lower95(venues.size, total)
        Option.when(calibration.showsRatings(bound) && !cluster.exists(families.denies(_, film)))(
          (film, bound, s"title family's majority film $film: ${venues.size} of $total venue(s), at least ${ResolverDecision.percent(bound)}"))
      }.flatten
    }.flatten
}
