package services.cinemas.common

import tools.ParallelDetailFetch

import java.util.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.util.{Failure, Try}

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


  /** Each of `keys`' pages read side by side ([[ParallelDetailFetch]], a few at a time), under
   *  [[requireAnyReached]]: the reads that answered, in `keys` order. One that failed or timed
   *  out drops only itself, unless every one did. */
  def readEach[K, T](label: String, keys: Seq[K], urlOf: K => String)(read: String => T): Seq[(K, T)] = {
    val distinct = keys.distinct
    val fetched  = ParallelDetailFetch.keyed(label, distinct, PageTimeout)(urlOf)(url => Try(read(url)))
    val attempts = distinct.map(key => key -> fetched.getOrElse(key, Failure(new TimeoutException(s"$label: ${urlOf(key)} timed out"))))
    requireAnyReached(attempts.map(_._2))
    attempts.flatMap { case (key, attempt) => attempt.toOption.map(key -> _) }
  }

  private val PageTimeout: FiniteDuration = 30.seconds
}
