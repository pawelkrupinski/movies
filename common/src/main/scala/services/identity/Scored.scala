package services.identity

import services.identity.IdentityMeasures.Measure

/** A candidate scored for `listing`, which its own title searches ranked at `rank` (best,
 *  1-based). `seasonProduction`: the film's record names the listing's season production
 *  (`IdentityMeasures.namesSeasonProduction`). `houseProduction`: the film's record bills the
 *  listing's work under the listing's own house (`IdentityMeasures.billsUnderItsHouse`).
 *  `deniedByPin`: a pin, not the listing's evidence, is (part of) why it is `denied`. */
private[identity] final case class Scored(c: Candidate, p: Double, measures: Map[String, Measure], denied: Boolean,
                                          listing: IdentityMeasures.Listing, rank: Option[Int],
                                          seasonProduction: Boolean = false, deniedByPin: Boolean = false,
                                          houseProduction: Boolean = false) {
  def category(measure: String): Option[String] = measures.get(measure).collect { case IdentityMeasures.Category(v) => v }
  def number(measure: String): Option[Double]   = measures.get(measure).collect { case IdentityMeasures.Number(v) => v }
  /** Does the listing's title NAME the film ([[IdentityMeasures.NamingRelations]])? */
  def titleNamesIt: Boolean = category("title").exists(IdentityMeasures.NamingRelations)
}

private[identity] object Scored {
  /** A scored candidate the resolver takes, with the confidence it is taken at. */
  type Accepted = (Scored, Double)
}
