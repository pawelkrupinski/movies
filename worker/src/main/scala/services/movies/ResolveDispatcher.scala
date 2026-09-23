package services.movies

import services.tasks.{EnqueueResult, EnrichTaskKeys, ResolveMode, TaskQueue, TaskType}

import java.util.concurrent.ConcurrentHashMap
import scala.concurrent.ExecutionContextExecutorService

/** How a needed single-movie TMDB resolution is dispatched. The resolution WORK
 *  is always the shared `MovieService.resolveTmdbOnce`; only the dispatch differs:
 *  production hands off to the durable task queue, the default runs it inline on a
 *  pool. */
trait ResolveDispatcher {
  /** `mode` Force re-resolves a row that already has a `tmdbId` — the only way to refresh
   *  a `Tmdb` slot, whose `fullDetails` are otherwise fetched once at first resolve
   *  and then frozen (see `UnresolvedTmdbReaper`'s stale-language sweep); RetryMiss
   *  searches past the row's remembered miss (the unresolved re-try). */
  def dispatch(title:         String,
               year:          Option[Int],
               originalTitle: Option[String],
               director:      Option[String],
               mode:          ResolveMode = ResolveMode.Normal): Unit

  /** Wait for in-flight inline resolutions, leaving the dispatcher usable. The queue
   *  dispatcher owns no pool (the TaskWorker lifecycle drains its work), so it no-ops. */
  def drain(): Unit = ()

  /** Drain any owned pool and then end it. */
  def stop(): Unit = ()
}

/** Production: enqueue a `ResolveTmdb` worker task — retried (Reschedule), deduped
 *  by dedupKey, and visible on `/debug`. Every mode shares the dedupKey, so a
 *  RetryMiss/Force dispatch that finds a task already WAITING raises that task's mode
 *  instead of being dropped (a still-waiting plain resolve would otherwise stop at the
 *  very miss the re-try exists to look past). A task already being worked on keeps the
 *  mode it was claimed with. */
class QueueResolveDispatcher(queue: TaskQueue) extends ResolveDispatcher {
  def dispatch(title:         String,
               year:          Option[Int],
               originalTitle: Option[String],
               director:      Option[String],
               mode:          ResolveMode): Unit = {
    val dedupKey = EnrichTaskKeys.resolveTmdbDedup(title, year)
    queue.enqueue(TaskType.ResolveTmdb, dedupKey,
      EnrichTaskKeys.resolveTmdbPayload(title, year, director, originalTitle, mode)) match {
      case EnqueueResult.Duplicate if mode != ResolveMode.Normal =>
        queue.amendWaiting(dedupKey, EnrichTaskKeys.modeFields(mode)); ()
      case _ => ()
    }
  }
}

/** Default (unit specs, scripts, Mongo-less dev, the fixture/determinism harness):
 *  resolve INLINE on `ec`, deduped by the row's `CacheKey` via `dedupKey` so the
 *  same key doesn't run twice concurrently (the task queue's job in production). */
class InlineResolveDispatcher(
  ec:       ExecutionContextExecutorService,
  dedupKey: (String, Option[Int]) => CacheKey,
  resolve:  (String, Option[Int], Option[String], Option[String], ResolveMode) => Unit
) extends ResolveDispatcher {
  private val pending = ConcurrentHashMap.newKeySet[CacheKey]()
  private val pool    = new tools.DrainablePool(ec)

  def dispatch(title:         String,
               year:          Option[Int],
               originalTitle: Option[String],
               director:      Option[String],
               mode:          ResolveMode): Unit = {
    val key = dedupKey(title, year)
    if (pending.add(key))
      pool.submit(try resolve(title, year, originalTitle, director, mode) finally { pending.remove(key); () })
  }

  override def drain(): Unit = pool.drain()

  override def stop(): Unit = pool.stop()
}
