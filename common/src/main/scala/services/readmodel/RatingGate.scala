package services.readmodel

import models.{RatingSearchUrls, ResolvedMovie, ResolvedRatings}
import services.movies.StoredMovieRecord

/**
 * CONFIDENCE-GATED RATINGS (docs/design/identity-resolver.md, "Confidence-gated ratings"): a
 * card whose identity decision is below the calibrated threshold is served with no ratings and
 * no direct rating links — only each site's search page — rather than ratings that may be
 * another film's. The title, showtimes and everything the venues published are untouched.
 *
 * The confidence is the calibration artefact's probability over the row's own stored evidence
 * (`services.identity.StoredIdentityConfidence`), and the line is the artefact's `showRatings` threshold — both
 * fitted by `scripts.IdentityCalibrate`, neither set by hand. Off in production:
 * `ReadModelProjector` takes [[RatingGate.off]] unless the worker's composition root sets
 * `KINOWO_IDENTITY_RATING_GATE` (`services.identity.EvidenceRatingGate`).
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
