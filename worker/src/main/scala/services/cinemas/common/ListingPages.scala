package services.cinemas.common

import scala.util.Try

/**
 * The failure rule for a listing spread over several pages (one per day, month,
 * date window or event): one page failing is tolerated, since the others still
 * carry most of the programme, but EVERY page failing means the source itself
 * is down. That case has to fail the scrape (red on /uptime, with its cause)
 * rather than come back as an empty "0 showtimes" success — which reads white,
 * indistinguishable from a genuinely dormant venue.
 */
object ListingPages {

  /** Throw the first failure when `attempts` is non-empty and none of them
   *  succeeded; otherwise do nothing. */
  def requireAnyReached(attempts: Iterable[Try[?]]): Unit =
    if (attempts.nonEmpty && attempts.forall(_.isFailure)) attempts.head.failed.foreach(throw _)

}
