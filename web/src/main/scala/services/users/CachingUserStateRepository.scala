package services.users

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import models.UserState

import java.util.concurrent.TimeUnit

/**
 * `UserStateRepository` decorator that caches positive `find` lookups. Each
 * logged-in page load calls `bootMergeFromServer` (in shared.js) which
 * fires `GET /api/me/state` — this resolves through `find(userId)`.
 * Without caching, every page-load triggers a Frankfurt-Atlas
 * round-trip; with caching, repeat loads hit memory.
 *
 * The state changes on every debounced LS write (shared.js's
 * `_serverSyncTimer`), so the cache turn-over is faster than for User
 * rows — TTL is shorter (10 min) to match. `upsert` refreshes the
 * cache directly with the canonical just-written value so an immediate
 * follow-up `find` (same browser, same session) returns the up-to-date
 * row without another round-trip.
 *
 * Misses are not cached: a fresh user with no state row yet should not
 * carry a phantom-empty across an upsert that lands between two calls.
 */
class CachingUserStateRepository(inner: UserStateRepository) extends UserStateRepository {

  private val byUserIdCache: Cache[String, UserState] =
    Caffeine.newBuilder()
      .maximumSize(10_000)
      .expireAfterWrite(10, TimeUnit.MINUTES)
      .build()

  def enabled: Boolean = inner.enabled

  def find(userId: String): Option[UserState] = {
    val cached = byUserIdCache.getIfPresent(userId)
    if (cached != null) Some(cached)
    else {
      val fresh = inner.find(userId)
      fresh.foreach(s => byUserIdCache.put(userId, s))
      fresh
    }
  }

  def upsert(state: UserState): Unit = {
    inner.upsert(state)
    byUserIdCache.put(state.userId, state)
  }

  def delete(userId: String): Unit = {
    inner.delete(userId)
    byUserIdCache.invalidate(userId)
  }

  def close(): Unit = inner.close()
}
