package services.users

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import play.api.Logging

import java.time.Instant
import java.util.concurrent.TimeUnit
import scala.concurrent.duration._

/** When a user's stored state last changed, if this cache currently has an
 *  answer. `None` means "don't know" — never seen, evicted, expired, or the
 *  cache cleared itself after the stream failed — and the caller falls back
 *  to reading storage directly; a `None` is never wrong, only unoptimized. */
trait UserChangeTimeCache {
  def lastChangeAt(userId: String): Option[Instant]
}

/** For a deployment with no repository-level change stream (`watchChanges`
 *  returns `None`) — every lookup is a miss, i.e. every caller falls back to
 *  storage, exactly as if this cache didn't exist. */
object NoUserChangeTimeCache extends UserChangeTimeCache {
  def lastChangeAt(userId: String): Option[Instant] = None
}

/**
 * Bounded, change-stream-fed cache of the ~1000 most recently active users'
 * last-change instants. Backs `UserStateController.hiddenFilms()`'s
 * `If-Modified-Since`-only fast path: when this cache can prove nothing
 * changed since the client's validator, the 304 is answered with ZERO reads
 * from storage.
 *
 * DELIBERATELY DOES NOT follow `MovieCache`'s "keep serving stale, self-heal
 * via a periodic full rehydrate" philosophy. That's fine for movie listings —
 * display data, tolerant of hours of staleness — but this cache gates an HTTP
 * freshness *decision*: a stale positive here means a real change (made on
 * another app-server instance, most likely) is silently told "not modified".
 * So on any detected stream failure this cache clears ENTIRELY rather than
 * keep answering from what it has — see `onDisconnect` below. A cleared or
 * missing entry is always safe: `lastChangeAt` returning `None` just routes
 * the caller to storage, same as a cold cache.
 *
 * `expireAfterWrite` is defence-in-depth against a SILENT stall (cursor open,
 * stopped delivering, no error) — the failure mode `ChangeStreamLiveness`/
 * `ChangeStreamReopen` exist to catch for `MovieCache` (see their doc
 * comments — a Mongo migration once left that stream dead for hours behind
 * green panels). Rather than add a second watchdog here, an entry simply
 * stops being trusted after `entryTtl` regardless of stream health, bounding
 * the blast radius of a stall this cache's own `onDisconnect` didn't catch.
 */
final class CaffeineUserChangeTimeCache(
  repository: UserStateRepository,
  maxEntries: Long = 1000,
  entryTtl:   FiniteDuration = 10.minutes
) extends UserChangeTimeCache with Logging {

  // A synchronous executor: Caffeine's default runs eviction/expiry MAINTENANCE
  // asynchronously (on the common ForkJoinPool), so a write past `maxEntries`
  // or past an entry's TTL isn't guaranteed to be reflected by the very next
  // read — fine for a high-throughput cache, but this one already goes out of
  // its way to avoid surprising staleness (see the class doc), and its real
  // load (a handful of user-state writes a second, not a hot path) makes the
  // synchronous cost negligible.
  private val cache: Cache[String, Instant] =
    Caffeine.newBuilder()
      .maximumSize(maxEntries)
      .expireAfterWrite(entryTtl.toMillis, TimeUnit.MILLISECONDS)
      .executor((r: Runnable) => r.run())
      .build()

  @volatile private var watchHandle: Option[AutoCloseable] = None

  def lastChangeAt(userId: String): Option[Instant] = Option(cache.getIfPresent(userId))

  def start(): Unit = {
    watchHandle = repository.watchChanges(
      onUpsert     = state => cache.put(state.userId, state.updatedAt),
      onDelete     = userId => cache.invalidate(userId),
      onDisconnect = () => {
        logger.warn("UserChangeTimeCache: change stream disconnected — invalidating the whole cache.")
        cache.invalidateAll()
      }
    )
    if (watchHandle.isEmpty)
      logger.info("UserChangeTimeCache: repository does not support watchChanges — cache stays empty (every lookup falls back to storage).")
  }

  def stop(): Unit = { watchHandle.foreach(_.close()); watchHandle = None }
}
