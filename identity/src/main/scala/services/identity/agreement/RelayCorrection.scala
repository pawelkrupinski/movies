package services.identity.agreement

import scala.collection.mutable

import services.identity.measures.{Film, Listing, ListingShape}
import services.identity.vocabulary.Question

/** The RELAY evidence against a film the MODEL took ([[CorrectionReading]]): a relay screening years after its record's
 *  broadcast, superseded by a newer record of its title, is no production the take can be; a stage work billed on the
 *  day one record of it was broadcast is that record. Read through the cluster's `candidates` (the broadcast join) and
 *  the correction's `records`. */
private[agreement] final class RelayCorrection(candidates: AgreementCandidates, records: CorrectionRecords) {
  import candidates.{candidatesOf, titledFilms}

  /** The take `model` withdrawn or switched by the relay its listings bill, if they bill one: superseded, else relayed. */
  def against(digested: AgreementEvidence.Cluster, model: Int, asked: mutable.Set[Question]): Option[Correction.Outcome] =
    records.record(model).flatMap(film => superseded(digested.listings, model, film).orElse(relayed(digested, model, film, asked)))

  /** A relay screening years after its record's broadcast, months after a newer record of its title: no production the
   *  take `model` (its record `film`) can be, whatever else is read. */
  private def superseded(listings: Seq[Listing], model: Int, film: Film): Option[Correction.Outcome] =
    candidates.supersededBy(listings, film, titledFilms(listings).filter(_ != model).flatMap(id => records.named(id).map(id -> _)))
      .map(why => Correction.Outcome(None, s"withdrawn ${records.title(model)} — relay: $why"))

  /** A listing billing a stage work on the day one record of it was broadcast is that record ([[Broadcast.take]]): a
   *  model take of a screen adaptation yields to it, as an agreed one does (PL Kino Amok's bare "Manon" on the Met's
   *  day) — a house's production taken is the model's reading of the relay, never corrected here. */
  private def relayed(digested: AgreementEvidence.Cluster, model: Int, film: Film, asked: mutable.Set[Question]): Option[Correction.Outcome] = {
    val listings = digested.listings
    Option.when(listings.exists(ListingShape.stagesAWork) && !Broadcast.billsAProduction(film.title)) {
      val known = (model +: candidatesOf(digested.id, digested.digest, listings)).distinct.flatMap(id => records.named(id).map(id -> _))
      // a correction reads no film database's productions; a record of the billed work stored with its year alone is read
      // again before the day decides, as the stage's own take reads it (UK Theatr Gwaun Fishguard's bare "Romeo and
      // Juliet" on the RBO relay's day stood as Zeffirelli's film while the relay's record held only 2027)
      candidates.relayTakeOf(listings, known, asked)
    }.flatten.filter(_.film != model).map(taken => Correction.Outcome(Some(taken.film), s"corrected from ${records.title(model)} — ${taken.line}"))
  }
}
