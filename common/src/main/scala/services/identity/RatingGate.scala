package services.identity

import models.{RatingSearchUrls, ResolvedMovie, ResolvedRatings}
import services.identity.ConfidenceCalibration.Calibration
import services.movies.{ListingKey, StoredMovieRecord}

/**
 * CONFIDENCE-GATED RATINGS (docs/design/identity-resolver.md, "Confidence-gated ratings"): a
 * card whose identity decision is below the calibrated threshold is served with no ratings and
 * no direct rating links — only each site's search page — rather than ratings that may be
 * another film's. The title, showtimes and everything the venues published are untouched.
 *
 * Off in production: `ReadModelProjector` takes [[RatingGate.off]] unless the worker's
 * composition root sets `KINOWO_IDENTITY_RATING_GATE`, and even then the gate withholds nothing
 * until the resolver's shadow run supplies decisions and labelled verdicts.
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

  /** A gate over ONE snapshot of the shadow resolver's output: its decisions, and the threshold
   *  calibrated from its labelled verdicts. */
  def fromShadow(shadow: ShadowDecisions): RatingGate = {
    val decisions   = shadow.latest()
    val calibration = ConfidenceCalibration.calibrate(shadow.verdicts())
    val byListing   = decisions.flatMap(d => d.listings.map(_ -> d)).toMap
    new RatingGate {
      def apply(stored: StoredMovieRecord, movie: ResolvedMovie): ResolvedMovie =
        gate(movie, confidenceOf(listingsOf(stored), byListing), calibration)
      val version = (decisions.map(d => (d.listings, d.confidence)).toSet, calibration).## | 1
    }
  }

  /** The listings a stored row serves, one per venue slot. */
  def listingsOf(stored: StoredMovieRecord): Set[ListingKey] =
    stored.record.cinemaShowings.map { case (cinema, slot) => ListingKey.ofSlot(cinema, slot) }.toSet

  /** The confidence of the film made of `listings`: its least confident decision's — a film
   *  spanning several resolver clusters is only as sure as its weakest. None when the resolver
   *  decided none of them. */
  def confidenceOf(listings: Set[ListingKey], decisions: Seq[Decision]): Option[Double] =
    confidenceOf(listings, decisions.flatMap(d => d.listings.map(_ -> d)).toMap)

  private def confidenceOf(listings: Set[ListingKey], byListing: Map[ListingKey, Decision]): Option[Double] =
    listings.flatMap(byListing.get).map(_.confidence).minOption

  /** Pure: `movie`, withheld when its confidence is below the calibrated threshold. No decision
   *  or no calibration withholds nothing. */
  def gate(movie: ResolvedMovie, confidence: Option[Double], calibration: Option[Calibration]): ResolvedMovie =
    if (confidence.exists(c => calibration.exists(_.gates(c)))) withheld(movie) else movie

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
