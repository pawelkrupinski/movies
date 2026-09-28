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
    calibration.contributions(ListingFilm, measures).collect { case (name, w) if !Priors(name) => w }.sum

  def own(s: Scored): Double = ownContributions(s.measures)

  /** What the listing's published FACTS alone contribute: its own evidence without the title relation. */
  def facts(s: Scored): Double =
    own(s) - calibration.contributions(ListingFilm, s.measures).collect { case ("title", w) => w }.sum

  /** The calibrated probability on the listing's own facts alone — what a cannot-link reads. A
   *  film the listing's facts do not contradict is never vetoed merely for ranking second in
   *  TMDB's search or for having same-titled rivals: that is ambiguity, not evidence of a
   *  different film. */
  def factsProbability(measures: Map[String, Measure]): Double = {
    // A measure that only AGREES (`IdentityMeasures.AgreesOnly`) never pushes toward a veto.
    val veto = calibration.contributions(ListingFilm, measures).collect {
      case (name, w) if !Priors(name) && !(IdentityMeasures.AgreesOnly(name) && w < 0) => w }.sum
    calibration.scopes(ListingFilm).calibration(calibration.scopes(ListingFilm).prior + veto)
  }

  /** Does anything the listing PUBLISHED weigh against the film — an own-fact measure the listing
   *  did not leave missing, with a negative weight? */
  def speaksAgainst(s: Scored): Boolean =
    calibration.contributions(ListingFilm, s.measures).exists { case (name, w) =>
      !Priors(name) && w < 0 && !s.measures.get(name).exists(_.isInstanceOf[IdentityMeasures.Missing])
    }

  /** Do the listing's own facts favour `best` over `other`? Its own evidence — without the title
   *  when the title names the two by disjoint pieces ([[IdentityMeasures.namedApart]]: "Lalka
   *  (Dolly)"), since it then names both alike and how each piece spells its film is no fact
   *  about which film the listing is. */
  def favours(best: Scored, other: Scored): Boolean =
    if (IdentityMeasures.namedApart(best.listing, best.c.film, other.c.film)) facts(best) > facts(other)
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
      val unanswered = top.measures.collect { case (name, IdentityMeasures.MissingFilm) => name }.toSet
      def answered(s: Scored) = ownContributions(s.measures.filterNot { case (name, _) => unanswered(name) })
      answered(rival) > answered(top)
    }

  /** `best`'s probability with the database's ranking priors and the family's pooled count
   *  LENDING confidence but never withdrawing it — each one's negative weight capped at 0 — when
   *  the listing's own facts decide: it compares a fact, nothing it published weighs against the
   *  film, and its facts favour the film over every other eligible candidate. Otherwise the
   *  calibrated probability: a namesake the facts fit alike is told apart only by the ranking,
   *  which then keeps its full weight. */
  def priorsLent(best: Scored, eligible: Seq[Scored]): Double =
    if (!IdentityMeasures.comparesAFact(ListingFilm, best.measures) || speaksAgainst(best) ||
        eligible.exists(r => (r ne best) && own(r) >= own(best))) best.p
    else {
      val scope = calibration.scopes(ListingFilm)
      val lent  = calibration.contributions(ListingFilm, best.measures).map { case (name, w) => if (Priors(name)) math.max(0.0, w) else w }.sum
      math.max(best.p, scope.calibration(scope.prior + lent))
    }

  /** Do the listing's OWN facts carry the film past the calibration's cut by themselves — at least
   *  one published fact compared (`IdentityMeasures.comparesAFact`), and [[factsProbability]],
   *  without the ranking priors, clearing the cut? */
  def carriedByOwnFacts(s: Scored): Boolean =
    IdentityMeasures.comparesAFact(ListingFilm, s.measures) && calibration.showsRatings(factsProbability(s.measures))
}
