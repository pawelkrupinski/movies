package services.users

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import models.User

import java.util.concurrent.TimeUnit

/**
 * `UserRepo` decorator that caches positive `findById` lookups in a
 * Caffeine map. Every authenticated page request resolves the session
 * cookie's `userId` through `findById` — caching avoids the per-request
 * Frankfurt-Atlas round-trip (~50–150 ms apiece) once the user's row is
 * in memory.
 *
 * Cache rules:
 *   - **positive only.** `None` is left uncached so a stale session
 *     cookie pointing at a deleted user still re-hits Mongo (rare; the
 *     row may have been re-created by an OAuth callback in the interim).
 *   - **populated on read AND write.** `upsert(user)` writes the
 *     canonical just-persisted value into the cache so the next
 *     `findById` sees it immediately, without an extra round-trip.
 *   - **invalidated on delete.** `delete(id)` evicts the entry so a
 *     subsequent `findById` either repopulates (race) or stays None.
 *   - **bounded + TTL.** 10 000 entries, 1-hour TTL — defensive against
 *     out-of-band Mongo edits (admin scripts, manual updates) so they
 *     surface within an hour without a redeploy.
 *
 * `findByProviderSub` / `findByEmail` are not cached: they fire during
 * the OAuth callback at most once per login, not on the page-render hot
 * path.
 */
class CachingUserRepo(inner: UserRepo) extends UserRepo {

  private val byIdCache: Cache[String, User] =
    Caffeine.newBuilder()
      .maximumSize(10_000)
      .expireAfterWrite(1, TimeUnit.HOURS)
      .build()

  def enabled: Boolean = inner.enabled

  def findById(id: String): Option[User] = {
    val cached = byIdCache.getIfPresent(id)
    if (cached != null) Some(cached)
    else {
      val fresh = inner.findById(id)
      fresh.foreach(u => byIdCache.put(id, u))
      fresh
    }
  }

  def findByProviderSub(provider: String, providerSub: String): Option[User] =
    inner.findByProviderSub(provider, providerSub)

  def findByEmail(email: String): Option[User] =
    inner.findByEmail(email)

  def upsert(user: User): Unit = {
    inner.upsert(user)
    byIdCache.put(user.id, user)
  }

  def delete(id: String): Unit = {
    inner.delete(id)
    byIdCache.invalidate(id)
  }

  def close(): Unit = inner.close()
}
