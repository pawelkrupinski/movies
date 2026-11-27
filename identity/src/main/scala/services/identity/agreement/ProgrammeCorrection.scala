package services.identity.agreement

import scala.collection.mutable

import services.identity.vocabulary.{Answer, Dependency, Question}

/** The venues' PROGRAMMES as evidence against a film the MODEL took ([[CorrectionReading]], [[Correction.Filmweb]]):
 *  the record their own programmes on the pass's listings site list under the take's title on its days
 *  ([[VenueListings]]), where it is another film than the take's and its facts contradict the take's record
 *  ([[CorrectionRecords.contradicts]]). Read through the family answers the pass holds ([[AgreementPass.Read.programmes]]),
 *  each noted in the correction's receipt. */
private[agreement] final class ProgrammeCorrection(records: CorrectionRecords) {

  /** The record the venues' programmes list against the take `model` — asked only of a take a poster `questioned`: a
   *  few venues' a day. */
  def listed(digested: AgreementEvidence.Cluster, model: Int, questioned: Boolean, reading: AgreementPass.Read,
             reads: mutable.Map[Dependency, Long], asked: mutable.Set[Question]): Option[SourceRecord] =
    records.source(model).flatMap { modelRecord =>
      val listed = reading.programmes.filter(_ => questioned).fold[Answer[Option[SourceRecord]]](Answer.Known(None))(answers =>
        VenueListings.listed(digested.listings, AgreementEvidence.recording(answers, asked, reads)))
      listed.answered.flatten.filter(record => !SameFilm.sameFilm(record, modelRecord) && records.contradicts(record, model))
    }
}
