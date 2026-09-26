package services.identity

import models.{RatingSearchUrls, ResolvedMovie, ResolvedRatings}
import services.movies.StoredMovieRecord

/**
 * CONFIDENCE-GATED RATINGS (docs/design/identity-resolver.md, "Confidence-gated ratings"): a
 * card whose identity decision is below the calibrated threshold is served with no ratings and
 * no direct rating links — only each site's search page — rather than ratings that may be
 * another film's. The title, showtimes and everything the venues published are untouched.
 *
 * The confidence is the calibration artefact's probability over the row's own stored evidence
 * ([[StoredIdentityConfidence]]), and the line is the artefact's `showRatings` threshold — both
 * fitted by `scripts.IdentityCalibrate`, neither set by hand. Off in production:
 * `ReadModelProjector` takes [[RatingGate.off]] unless the worker's composition root sets
 * `KINOWO_IDENTITY_RATING_GATE`.
 */
trait RatingGate {
  /** The card as served: `movie` itself, or with its ratings withheld. */
  def apply(stored: StoredMovieRecord, movie: ResolvedMovie): ResolvedMovie
  /** Changes whenever a verdict of this gate could: the projector's metadata-reuse memo is keyed
   *  by it, so a new gate re-projects rather than serving a card gated under the old one. */
  def version: Int
}

object RatingGate {

  val off: RatingGate = new RatingGate {
    def apply(stored: StoredMovieRecord, movie: ResolvedMovie): ResolvedMovie = movie
    val version = 0
  }

  /** The gate over each row's own stored evidence ([[StoredIdentityConfidence]]): a row whose
   *  calibrated confidence is below the artefact's display threshold is withheld. A row with
   *  nothing to measure (no TMDB record, no venue listing) keeps its card. */
  def fromEvidence(calibration: IdentityCalibration): RatingGate = new RatingGate {
    def apply(stored: StoredMovieRecord, movie: ResolvedMovie): ResolvedMovie =
      if (StoredIdentityConfidence.of(stored.record, calibration).exists(p => !calibration.showsRatings(p))) withheld(movie)
      else movie
    val version = calibration.## | 1
  }

  /** `movie` with no rating, no IMDb link, each site's search page for the others, and the
   *  unrated sort key. */
  def withheld(movie: ResolvedMovie): ResolvedMovie = {
    val title = movie.originalTitle.getOrElse(movie.title)
    movie.copy(
      ratings = ResolvedRatings(
        imdb              = None,
        imdbUrl           = None,
        metascore         = None,
        metacriticUrl     = RatingSearchUrls.metacritic(title),
        rottenTomatoes    = None,
        rottenTomatoesUrl = RatingSearchUrls.rottenTomatoes(title),
        filmweb           = None,
        filmwebUrl        = RatingSearchUrls.filmweb(title)),
      weightedRating = 0.0)
  }
}
