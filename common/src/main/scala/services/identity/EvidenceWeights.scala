package services.identity

import services.identity.IdentityMeasures.{ListingFilm, Measure}

/** How much a scored candidate is the listing's film on what the LISTING'S OWN facts say — the
 *  title, year, director, runtime, original title and country measures — as opposed to the film
 *  database's ranking priors (search rank, popularity, rivals) and the family's pooled count
 *  (`venues.corroborating`), which [[Priors]] names. */
private[identity] final class EvidenceWeights(calibration: IdentityCalibration) {

  val Priors: Set[String] = IdentityMeasures.RankingPriors ++ IdentityMeasures.PooledMeasures

  /** What the listing's own facts contribute to the calibrated score of `measures`. */
  def ownContributions(measures: Map[String, Measure]): Double =
    calibration.contributions(ListingFilm, measures).collect { case (name, weight) if !Priors(name) => weight }.sum

  def own(scored: Scored): Double = ownContributions(scored.measures)

  /** What the listing's published FACTS alone contribute: its own evidence without the title relation. */
  def facts(scored: Scored): Double =
    own(scored) - calibration.contributions(ListingFilm, scored.measures).collect { case ("title", weight) => weight }.sum

  /** The calibrated probability on the listing's own facts alone — what a cannot-link reads. A
   *  film the listing's facts do not contradict is never vetoed merely for ranking second in
   *  TMDB's search or for having same-titled rivals: that is ambiguity, not evidence of a
   *  different film. */
  def factsProbability(measures: Map[String, Measure]): Double = {
    // A measure that only AGREES (`IdentityMeasures.AgreesOnly`) never pushes toward a veto; nor
    // does a title carrying the other's whole (`IdentityMeasures.ContainingRelations`): DE's "…In
    // Buenos Aires: Live" sits inside TMDB's "…: Live Viewing", which is no sign of another film.
    val contains = measures.get("title").exists {
      case IdentityMeasures.Category(relation) => IdentityMeasures.ContainingRelations(relation)
      case _                                   => false
    }
    val veto = calibration.contributions(ListingFilm, measures).collect {
      case (name, weight) if !Priors(name) && !((IdentityMeasures.AgreesOnly(name) || (name == "title" && contains)) && weight < 0) => weight }.sum
    calibration.scopes(ListingFilm).calibration(calibration.scopes(ListingFilm).prior + veto)
  }

  /** Does anything the listing PUBLISHED weigh against the film — an own-fact measure the listing
   *  did not leave missing, with a negative weight? */
  def speaksAgainst(scored: Scored): Boolean = against(scored).nonEmpty
  /** The published facts weighing against `scored` — what [[speaksAgainst]] finds — rendered as the trace's evidence is. */
  def against(scored: Scored): Seq[String] = {
    val names = calibration.contributions(ListingFilm, scored.measures).collect { case (name, weight)
      if !Priors(name) && weight < 0 && !scored.measures.get(name).exists(_.isInstanceOf[IdentityMeasures.Missing]) => name }.toSet
    if (names.isEmpty) Nil else calibration.evidence(ListingFilm, scored.measures).filter(line => names(line.takeWhile(_ != '=')))
  }

  /** Do the listing's own facts favour `best` over `other`? Its own evidence — without the title
   *  when the title names the two by disjoint pieces ([[IdentityMeasures.namedApart]]: "Lalka
   *  (Dolly)"), since it then names both alike and how each piece spells its film is no fact
   *  about which film the listing is. */
  def favours(best: Scored, other: Scored): Boolean =
    if (IdentityMeasures.namedApart(best.listing, best.candidate.film, other.candidate.film)) facts(best) > facts(other)
    else own(best) > own(other)

  /** Does the listing's own evidence fit `rival` better than `top`, its exact top hit? A rival the
   *  listing's title does not even NAME ([[IdentityMeasures.NamingRelations]]) is weighed only on
   *  the facts `top`'s record answers: a credit or a running time the record leaves missing is
   *  missing evidence, not evidence against it. Otherwise a director's other work, reached by
   *  walking the credit, out-weighs the very record the title names on the facts that record
   *  lacks (Ocine's "BTS … IN BUENOS AIRES: LIVE VIEWING", 195 minutes, "Jungjae HA": TMDB's
   *  exact, rank-1 record credits nobody and states no runtime; his "… in Busan" credits him at
   *  195). A rival the title names as well — a namesake, an edition — is weighed on everything:
   *  there the facts are what tells the two apart. */
  def fitsBetter(rival: Scored, top: Scored): Boolean =
    if (rival.titleNamesIt) own(rival) > own(top)
    else {
      def answered(scored: Scored) = ownContributions(scored.measures.filterNot { case (name, _) => unansweredBy(top)(name) })
      answered(rival) > answered(top)
    }

  /** The measures `record`'s film leaves unanswered: a credit or a running time it does not state. */
  private def unansweredBy(record: Scored): Set[String] =
    record.measures.collect { case (name, IdentityMeasures.MissingFilm) => name }.toSet

  /** `scored`'s published [[facts]] on the measures `record`'s film answers — what may tell a
   *  record the title names less closely apart from `record`, without counting what `record` merely
   *  lacks as evidence against it. */
  def factsAnswered(scored: Scored, record: Scored): Double =
    facts(scored.copy(measures = scored.measures.filterNot { case (name, _) => unansweredBy(record)(name) }))

  /** `best`'s probability with the database's ranking priors and the family's pooled count
   *  LENDING confidence but never withdrawing it — each one's negative weight capped at 0 — when
   *  the listing's own facts decide: it compares a fact, nothing it published weighs against the
   *  film, and its facts favour the film over every other eligible candidate. Otherwise the
   *  calibrated probability: a namesake the facts fit alike is told apart only by the ranking,
   *  which then keeps its full weight. */
  def priorsLent(best: Scored, eligible: Seq[Scored]): Double =
    if (!IdentityMeasures.comparesAFact(ListingFilm, best.measures) || speaksAgainst(best) ||
        eligible.exists(rival => (rival ne best) && own(rival) >= own(best))) best.probability
    else {
      val scope = calibration.scopes(ListingFilm)
      val lent  = calibration.contributions(ListingFilm, best.measures).map { case (name, weight) => if (Priors(name)) math.max(0.0, weight) else weight }.sum
      math.max(best.probability, scope.calibration(scope.prior + lent))
    }

  /** Do the listing's OWN facts carry the film past the calibration's cut by themselves — at least
   *  one published fact compared (`IdentityMeasures.comparesAFact`), and [[factsProbability]],
   *  without the ranking priors, clearing the cut? */
  def carriedByOwnFacts(scored: Scored): Boolean =
    IdentityMeasures.comparesAFact(ListingFilm, scored.measures) && calibration.showsRatings(factsProbability(scored.measures))
}
