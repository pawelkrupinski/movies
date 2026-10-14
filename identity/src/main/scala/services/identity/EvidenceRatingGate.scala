package services.identity

import models.ResolvedMovie
import services.movies.StoredMovieRecord
import services.readmodel.RatingGate

/** The live [[RatingGate]] (docs/design/identity-resolver.md, "Confidence-gated ratings"): its confidence the
 *  calibration artefact's probability over the row's own stored evidence. */
object EvidenceRatingGate {
  /** The gate over each row's own stored evidence ([[StoredIdentityConfidence]]): a row whose
   *  calibrated confidence is below the artefact's display threshold is withheld. A row with
   *  nothing to measure (no TMDB record, no venue listing) keeps its card. */
  def apply(calibration: IdentityCalibration): RatingGate = new RatingGate {
    def apply(stored: StoredMovieRecord, movie: ResolvedMovie): ResolvedMovie =
      if (StoredIdentityConfidence.of(stored.record, calibration).exists(p => !calibration.showsRatings(p))) RatingGate.withheld(movie)
      else movie
    val version = calibration.## | 1
  }

}
