package services.review

import com.github.benmanes.caffeine.cache.{CacheLoader, LoadingCache, Ticker}
import services.identity.ResolverDecision

import java.time.{Clock, Duration, Instant}
import java.util.concurrent.{Executor, ForkJoinPool, TimeUnit}
import scala.concurrent.duration._

/**
 * A country's [[ReviewSource]] whose page reads are kept, and answered while a re-read runs behind them: the two
 * WHOLE-CORPUS reads every review page makes on each load to pick the few cards it shows — every decision, and every
 * slot time in a look-back window (the US alone: ~2,200 decisions and ~104,000 slot times) — and the per-card reads of
 * the cards it shows, by the rows they ask for: a reload shows the same cards, and the US's biggest ask a thousand
 * venues' whole programmes for theirs. The corpus moves only as the mirror catches prod up.
 *
 * A kept read older than `refreshAfter` is answered as kept and read again on `refreshOn`; one older than
 * `expireAfter` (nobody looked for a while) is read again before it is answered. A failed re-read keeps the read it
 * would have replaced. The Why fold's traces and the export's film links, read on a click, go straight through.
 */
final class CachingReviewSource(underlying: ReviewSource, clock: Clock,
                                      refreshAfter: FiniteDuration = 30.seconds,
                                      // an hour: a page opened after a break answers at once, from reads at most that old
                                      expireAfter: FiniteDuration = 1.hour,
                                      // what both ages are measured on: the system's nanosecond ticker outside specs
                                      ticker: Ticker = Ticker.systemTicker(),
                                      refreshOn: Executor = ForkJoinPool.commonPool()) extends ReviewSource {
  export underlying.{decisions as _, updatedSince as _, slots as _, venuePages as _, feeds as _, films as _, filmRecords as _, *}

  private def kept[K, V](entries: Long)(read: K => V): LoadingCache[K, V] =
    tools.BoundedCache.ofSize(entries)
      .refreshAfterWrite(refreshAfter.toNanos, TimeUnit.NANOSECONDS)
      .expireAfterWrite(expireAfter.toNanos, TimeUnit.NANOSECONDS)
      .ticker(ticker)
      .executor(refreshOn)
      .build[K, V](new CacheLoader[K, V] { def load(key: K): V = read(key) })

  // by `unmatchedOnly`
  private val decisionsKept = kept[java.lang.Boolean, Seq[ResolverDecision]](2)(unmatchedOnly => underlying.decisions(unmatchedOnly))
  // by the look-back in whole minutes, read back from the moment of each (re-)read: a later look-back of the same window
  // starts no earlier than the kept read did, so its rows are the kept ones written since it
  private val slotTimesKept = kept[java.lang.Long, Map[String, Instant]](8)(minutes =>
    underlying.updatedSince(clock.instant().minus(Duration.ofMinutes(minutes))))

  // by the rows asked for: each page of cards asks once per load, the queue's, the matchable and the recent pages' apart
  private val slotsKept   = kept[Seq[String], Map[String, SlotFacts]](8)(underlying.slots)
  private val pagesKept   = kept[Seq[String], Map[String, VenueFacts]](8)(underlying.venuePages)
  private val feedsKept   = kept[Seq[(String, String)], Map[(String, String), ListingFeed]](8)(underlying.feeds)
  private val filmsKept   = kept[Seq[Int], Map[Int, FilmCard]](8)(underlying.films)
  private val recordsKept = kept[Seq[Int], Map[Int, FilmCard]](8)(underlying.filmRecords)

  def decisions(unmatchedOnly: Boolean): Seq[ResolverDecision] = decisionsKept.get(unmatchedOnly)
  def slots(listingKeys: Seq[String]): Map[String, SlotFacts]                    = slotsKept.get(listingKeys)
  def venuePages(urls: Seq[String]): Map[String, VenueFacts]                     = pagesKept.get(urls)
  def feeds(listings: Seq[(String, String)]): Map[(String, String), ListingFeed] = feedsKept.get(listings)
  def films(tmdbIds: Seq[Int]): Map[Int, FilmCard]                               = filmsKept.get(tmdbIds)
  def filmRecords(tmdbIds: Seq[Int]): Map[Int, FilmCard]                         = recordsKept.get(tmdbIds)

  def updatedSince(since: Instant): Map[String, Instant] = {
    val minutes = math.ceil(Duration.between(since, clock.instant()).toMillis / 60000.0).toLong.max(0L)
    slotTimesKept.get(minutes).filter { case (_, at) => !at.isBefore(since) }
  }
}
