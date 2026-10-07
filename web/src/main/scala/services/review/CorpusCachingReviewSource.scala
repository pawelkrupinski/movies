package services.review

import com.github.benmanes.caffeine.cache.{CacheLoader, LoadingCache, Ticker}
import services.identity.ResolverDecision

import java.time.{Clock, Duration, Instant}
import java.util.concurrent.{Executor, ForkJoinPool, TimeUnit}
import scala.concurrent.duration._

/**
 * A country's [[ReviewSource]] whose two WHOLE-CORPUS reads — every decision, and every slot time in a look-back
 * window — are kept, and answered while a re-read runs behind them. Every review page reads the whole corpus on each
 * load to pick the few cards it shows (the US alone: ~2,200 decisions and ~104,000 slot times, most of a load), and
 * the corpus moves only as the mirror catches prod up.
 *
 * A kept read older than `refreshAfter` is answered as kept and read again on `refreshOn`; one older than
 * `expireAfter` (nobody looked for a while) is read again before it is answered. A failed re-read keeps the read it
 * would have replaced. The per-card reads, bounded by the keys on screen, go straight through.
 */
final class CorpusCachingReviewSource(underlying: ReviewSource, clock: Clock,
                                      refreshAfter: FiniteDuration = 30.seconds,
                                      expireAfter: FiniteDuration = 10.minutes,
                                      // what both ages are measured on: the system's nanosecond ticker outside specs
                                      ticker: Ticker = Ticker.systemTicker(),
                                      refreshOn: Executor = ForkJoinPool.commonPool()) extends ReviewSource {
  export underlying.{decisions as _, updatedSince as _, *}

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

  def decisions(unmatchedOnly: Boolean): Seq[ResolverDecision] = decisionsKept.get(unmatchedOnly)

  def updatedSince(since: Instant): Map[String, Instant] = {
    val minutes = math.ceil(Duration.between(since, clock.instant()).toMillis / 60000.0).toLong.max(0L)
    slotTimesKept.get(minutes).filter { case (_, at) => !at.isBefore(since) }
  }
}
