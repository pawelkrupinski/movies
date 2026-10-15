package services.identity

import services.identity.IdentityMeasures.Measure

/** A candidate scored for `listing`, which its own title searches ranked at `rank` (best,
 *  1-based). `seasonProduction`: the film's record names the listing's season production
 *  (`StageRelays.namesSeasonProduction`). `houseProduction`: the film's record bills the
 *  listing's work under the listing's own house (`StageRelays.billsUnderItsHouse`).
 *  `deniedByPin`: a pin, not the listing's evidence, is (part of) why it is `denied`. */
private[identity] final case class Scored(candidate: Candidate, probability: Double, measures: Map[String, Measure], denial: Option[Denial],
                                          listing: IdentityMeasures.Listing, rank: Option[Int],
                                          seasonProduction: Boolean = false, deniedByPin: Boolean = false,
                                          houseProduction: Boolean = false, imdb: Option[Scored.ImdbPlace] = None,
                                          suggestedOnly: Boolean = false, soleResult: Boolean = false, imdbTitled: Set[String] = Set.empty,
                                          vetoedRivals: Int = 0, untaken: Option[String] = None) {
  /** Is the film ruled out for this listing — `denial` says why. */
  def denied: Boolean = denial.isDefined
  /** May a rule take it? Not a denied film, nor an UNTAKEN one — `untaken` says why: a film no rule takes that stays in
   *  the pool as a rival, so that leaving it out lifts no other candidate (a short beside the feature its title names,
   *  [[ShortFilms]]). */
  def takeable: Boolean = !denied && untaken.isEmpty
  /** Denied by the probability cut alone — no learned rule, no pin: the weakest denial, the listing's title and few facts
   *  reading low, which another listing's own match outweighs (`Families.deniedBySibling`, `Acceptance.soleResult`). */
  def deniedByCutOnly: Boolean = !deniedByPin && denial.exists(_.kind == Denial.Kind.ProbabilityCut)
  def category(measure: String): Option[String] = measures.get(measure).collect { case IdentityMeasures.Category(value) => value }
  def number(measure: String): Option[Double]   = measures.get(measure).collect { case IdentityMeasures.Number(value) => value }
  /** Does the listing's title NAME the film ([[IdentityMeasures.NamingRelations]])? */
  def titleNamesIt: Boolean = category("title").exists(IdentityMeasures.NamingRelations)
  // `suggestedOnly`: only IMDb's suggestions reached the film, under a title the listing does not carry
  // (IMDb matched another-language title of it) — a candidate for `Acceptance.imdbSuggested` alone.
  // `vetoedRivals`: how many of the namesakes its `rivals` measure counts the release veto took away ([[ReleaseVeto]]) —
  // still counted in its calibrated probability, never in the evidence class it is accepted by (`Acceptance.classAccepted`).
  // `imdbTitled`: the listing's search titles IMDb lists the film under, in some language (an AKA: "Camino dla
  // opornych" is IMDb's Polish title of "Compostelle") — `Acceptance.imdbSuggested`'s titled rung.
}

private[identity] object Scored {
  /** Where IMDb's suggestions for the listing's own title put the film: its 1-based place, among `of` films. */
  final case class ImdbPlace(place: Int, of: Int)
  /** A scored candidate the resolver takes, with the confidence it is taken at. */
  type Accepted = (Scored, Double)
}
