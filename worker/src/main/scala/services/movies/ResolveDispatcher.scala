package services.movies

import services.tasks.{EnrichTaskKeys, TaskQueue, TaskType}

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.ExecutionContextExecutorService

/** How a needed single-movie TMDB resolution is dispatched. The resolution WORK
 *  is always the shared `MovieService.resolveTmdbOnce`; only the dispatch differs:
 *  production hands off to the durable task queue, the default runs it inline on a
 *  pool. */
trait ResolveDispatcher {
  /** `force` re-resolves a row that already has a `tmdbId` — the only way to refresh
   *  a `Tmdb` slot, whose `fullDetails` are otherwise fetched once at first resolve
   *  and then frozen (see `UnresolvedTmdbReaper`'s stale-language sweep). */
  def dispatch(title:         String,
               year:          Option[Int],
               originalTitle: Option[String],
               director:      Option[String],
               force:         Boolean = false): Unit

  /** Wait for in-flight inline resolutions, leaving the dispatcher usable. The queue
   *  dispatcher owns no pool (the TaskWorker lifecycle drains its work), so it no-ops. */
  def drain(): Unit = ()

  /** Drain any owned pool and then end it. */
  def stop(): Unit = ()
}

/** Production: enqueue a `ResolveTmdb` worker task — retried (Reschedule), deduped
 *  by dedupKey, and visible on `/debug`. */
class QueueResolveDispatcher(queue: TaskQueue) extends ResolveDispatcher {
  def dispatch(title:         String,
               year:          Option[Int],
               originalTitle: Option[String],
               director:      Option[String],
               force:         Boolean): Unit = {
    queue.enqueue(
      TaskType.ResolveTmdb,
      EnrichTaskKeys.resolveTmdbDedup(title, year),
      EnrichTaskKeys.resolveTmdbPayload(title, year, director, originalTitle, force))
    ()
  }
}

/** Default (unit specs, scripts, Mongo-less dev, the fixture/determinism harness):
 *  resolve INLINE on `ec`, deduped by the row's `CacheKey` via `dedupKey` so the
 *  same key doesn't run twice concurrently (the task queue's job in production). */
class InlineResolveDispatcher(
  ec:       ExecutionContextExecutorService,
  dedupKey: (String, Option[Int]) => CacheKey,
  resolve:  (String, Option[Int], Option[String], Option[String], Boolean) => Unit
) extends ResolveDispatcher {
  private val pending = ConcurrentHashMap.newKeySet[CacheKey]()
  private val pool    = new tools.DrainablePool(ec)

  def dispatch(title:         String,
               year:          Option[Int],
               originalTitle: Option[String],
               director:      Option[String],
               force:         Boolean): Unit = {
    val key = dedupKey(title, year)
    if (pending.add(key))
      pool.submit(try resolve(title, year, originalTitle, director, force) finally { pending.remove(key); () })
  }

  override def drain(): Unit = pool.drain()

  override def stop(): Unit = pool.stop()
}
