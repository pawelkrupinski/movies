package services.identity

import services.titles.billing.BillingPieces

import scala.collection.mutable

/** What a node's billing says it is, which rules a film out for it ([[NodeDenials]]): a film festival billed by its name
 *  alone, a silent film of the sound era, a cast field naming none of the film's cast. */
private[identity] final class BillingDenials {

  /** Why `node`'s billing rules `film` out, if it does. */
  def of(node: EvidenceNode, film: Film): Option[String] =
    Option.when(billingOf(node).festival)("its title bills a film festival by its name alone: the programme, no film")
      // a billing as a silent film dates it to the silent era, as the agreement reads it (DE "STUMMFILM MIT TOBIAS
      // RANK:CHARLIE CHAPLIN" is no 1999 "Charlie Chaplin")
      .orElse(Option.when(ListingShape.silentBillingDenies(film.year, billingOf(node).silent))(
        "it bills a silent film, and this one is of the sound era"))
      // the venues' own cast fields name none of its cast, and none of their facts agrees with it
      .orElse(CastEvidence.mismatch(node.listings, film, node.country.flatMap(TitleRelationMeasures.languageOf),
        billingOf(node).castIsTheFilms))

  /** What `node`'s billing says it is beside a film, once per node — asked of every candidate of its pool: a film
   *  festival billed by its name alone ([[services.titles.billing.BillingParse.billsAFilmFestival]]), a silent film ([[ListingShape.billsASilentFilm]]),
   *  a cast field listing the film's cast ([[CastEvidence.castIsTheFilms]]). */
  private final case class Billing(festival: Boolean, silent: Boolean, castIsTheFilms: Boolean)
  private def billingOf(node: EvidenceNode): Billing = billingMemo.getOrElseUpdate(node.id, {
    val listing = node.evidence.published
    Billing(BillingPieces.billsAFilmFestival(listing.rawTitle.getOrElse(listing.title)), node.listings.exists(ListingShape.billsASilentFilm),
      node.listings.exists(_.listedCast.nonEmpty) && node.castIsTheFilms)
  })
  private val billingMemo = mutable.HashMap.empty[String, Billing]
}
