package services.cinemas.common

import tools.ParallelDetailFetch

import java.util.concurrent.TimeoutException
import scala.concurrent.duration._
import scala.util.{Failure, Try}

/**
 * The failure rule for a listing spread over several pages (one per day, month,
 * date window or event): a page that fails is left out and makes the LISTING
 * INCOMPLETE ([[ListingReads]]) — the others still carry most of the programme, but
 * the cache must not prune the films that only that page lists. EVERY page failing
 * means the source itself is down: that fails the scrape (red on /uptime, with its
 * cause) rather than coming back as an empty "0 showtimes" success — which reads
 * white, indistinguishable from a genuinely dormant venue.
 */
object ListingPages {

  /** Throw the first failure when `attempts` is non-empty and none of them
   *  succeeded; otherwise report each failed one as a page the listing lacks. */
  def requireAnyReached(attempts: Iterable[Try[?]]): Unit =
    if (attempts.nonEmpty && attempts.forall(_.isFailure)) attempts.head.failed.foreach(throw _)
    else reportFailed(attempts)


  /** Each of `keys`' pages read side by side ([[ParallelDetailFetch]], a few at a time), under
   *  [[requireAnyReached]]: the reads that answered, in `keys` order. One that failed or timed
   *  out drops only itself — and makes the listing incomplete — unless every one did. */
  def readEach[K, T](label: String, keys: Seq[K], urlOf: K => String)(read: String => T): Seq[(K, T)] = {
    val distinct = keys.distinct
    val fetched  = ParallelDetailFetch.keyed(label, distinct, PageTimeout)(urlOf)(url => Try(read(url)))
    val attempts = distinct.map(key => key -> fetched.getOrElse(key, Failure(new TimeoutException(s"$label: ${urlOf(key)} timed out"))))
    requireAnyReached(attempts.map(_._2))
    attempts.flatMap { case (key, attempt) => attempt.toOption.map(key -> _) }
  }

  /** The pages of a listing whose FIRST page already answered — later days, next pages,
   *  spillover pages: read side by side ([[ParallelDetailFetch]]), the ones that answered in
   *  `keys` order. A page that failed or timed out is left out and makes the listing
   *  incomplete; none of them failing the scrape, since the first page proved the source up. */
  def readMore[K, T](label: String, keys: Seq[K], urlOf: K => String, maxConcurrent: Int = 2,
                     timeout: FiniteDuration = PageTimeout)(read: String => T): Seq[(K, T)] = {
    val attempts = attemptEach(label, keys, urlOf, maxConcurrent, timeout)(read)
    reportFailed(attempts.map(_._2))
    attempts.flatMap { case (key, attempt) => attempt.toOption.map(key -> _) }
  }

  /** Pages that only ENRICH films the listing already holds — a release year, a poster, an
   *  original title — read side by side like [[readMore]], every key's attempt returned in
   *  `keys` order. A failed one is NOT reported: no film is dropped for want of it, so it says
   *  nothing about what the cache may prune, and reporting it kept a venue whose one detail page
   *  always timed out from ever pruning. A caller that DOES drop a film for a failed page reports
   *  that page itself ([[reportFailed]]). */
  def readEnrichment[K, T](label: String, keys: Seq[K], urlOf: K => String, maxConcurrent: Int = 2,
                           timeout: FiniteDuration = PageTimeout)(read: String => T): Seq[(K, Try[T])] =
    attemptEach(label, keys, urlOf, maxConcurrent, timeout)(read)

  private def attemptEach[K, T](label: String, keys: Seq[K], urlOf: K => String, maxConcurrent: Int,
                                timeout: FiniteDuration)(read: String => T): Seq[(K, Try[T])] = {
    val distinct = keys.distinct
    val fetched  = ParallelDetailFetch.keyed(label, distinct, timeout, maxConcurrent)(urlOf)(url => Try(read(url)))
    distinct.map(key => key -> fetched.getOrElse(key, Failure(new TimeoutException(s"$label: ${urlOf(key)} timed out"))))
  }

  /** Report each failed page of a listing whose other pages answered, without failing it. */
  def reportFailed(attempts: Iterable[Try[?]]): Unit = attempts.foreach(_.failed.foreach(ListingReads.pageFailed))

  private val PageTimeout: FiniteDuration = 30.seconds
}
