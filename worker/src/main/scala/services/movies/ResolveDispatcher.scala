package services.movies

import services.tasks.{EnrichTaskKeys, TaskQueue, TaskType}

import java.util.concurrent.{ConcurrentHashMap, TimeUnit}
import scala.concurrent.{ExecutionContextExecutorService, Future}

/** How a needed single-movie TMDB resolution is dispatched. The resolution WORK
 *  is always the shared `MovieService.resolveTmdbOnce`; only the dispatch differs:
 *  production hands off to the durable task queue, the default runs it inline on a
 *  pool. */
trait ResolveDispatcher {
  def dispatch(title: String, year: Option[Int], originalTitle: Option[String], director: Option[String]): Unit

  /** Drain any owned pool so in-flight inline resolutions finish before shutdown.
   *  The queue dispatcher owns no pool (the TaskWorker lifecycle drains its work),
   *  so it no-ops. */
  def stop(): Unit = ()
}

/** Production: enqueue a `ResolveTmdb` worker task — retried (Reschedule), deduped
 *  by dedupKey, and visible on `/debug`. */
class QueueResolveDispatcher(queue: TaskQueue) extends ResolveDispatcher {
  def dispatch(title: String, year: Option[Int], originalTitle: Option[String], director: Option[String]): Unit = {
    queue.enqueue(
      TaskType.ResolveTmdb,
      EnrichTaskKeys.resolveTmdbDedup(title, year),
      EnrichTaskKeys.resolveTmdbPayload(title, year, director, originalTitle))
    ()
  }
}

/** Default (unit specs, scripts, Mongo-less dev, the fixture/determinism harness):
 *  resolve INLINE on `ec`, deduped by the row's `CacheKey` via `dedupKey` so the
 *  same key doesn't run twice concurrently (the task queue's job in production). */
class InlineResolveDispatcher(
  ec:       ExecutionContextExecutorService,
  dedupKey: (String, Option[Int]) => CacheKey,
  resolve:  (String, Option[Int], Option[String], Option[String]) => Unit
) extends ResolveDispatcher {
  private val pending = ConcurrentHashMap.newKeySet[CacheKey]()

  def dispatch(title: String, year: Option[Int], originalTitle: Option[String], director: Option[String]): Unit = {
    val key = dedupKey(title, year)
    if (pending.add(key)) {
      Future(try resolve(title, year, originalTitle, director) finally { pending.remove(key); () })(using ec)
      ()
    }
  }

  override def stop(): Unit = {
    ec.shutdown()
    while (!ec.isTerminated) ec.awaitTermination(1, TimeUnit.HOURS)
  }
}
