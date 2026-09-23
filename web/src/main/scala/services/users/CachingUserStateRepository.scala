package services.users

import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import models.UserState
import services.movies.ChangeStreamLiveness

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
 *
 * OTHER WRITERS. The users database is shared by every web replica and every
 * country's host, and the controller's writes are read-modify-write full
 * replaces — so a row cached here after another pod wrote is not merely stale,
 * it is the base the next write here would overwrite that pod's change with.
 * The change stream sees every pod's writes: the `watchChanges` pass-through
 * below also drops any cached row the stream says moved on (a different
 * `updatedAt` — this pod's own echo matches and is kept), deleted, or — when
 * the stream loses track — everything. This only holds while someone watches
 * through this decorator; production's `UserChangeTimeCache` does.
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

  // Passed through to `inner` — `UserChangeTimeCache` wraps whichever repository
  // it's handed, so this MUST reach the real Mongo-backed watch underneath, or
  // that cache would be permanently empty in production (this decorator sits
  // between them in `UsersWiring`) — with each callback first evicting what
  // the event makes stale here (see the class doc's OTHER WRITERS).
  override def watchChanges(
    onUpsert:     UserState => Unit,
    onDelete:     String => Unit,
    onDisconnect: () => Unit
  ): Option[AutoCloseable] = inner.watchChanges(
    onUpsert = state => {
      Option(byUserIdCache.getIfPresent(state.userId))
        .filter(_.updatedAt != state.updatedAt)
        .foreach(_ => byUserIdCache.invalidate(state.userId))
      onUpsert(state)
    },
    onDelete = userId => { byUserIdCache.invalidate(userId); onDelete(userId) },
    onDisconnect = () => { byUserIdCache.invalidateAll(); onDisconnect() })

  override def changeStreamLiveness: ChangeStreamLiveness = inner.changeStreamLiveness
}
