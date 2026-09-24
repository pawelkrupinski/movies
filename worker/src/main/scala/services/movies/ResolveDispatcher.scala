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
 *  mode it was claimed with — that re-try is lost, and `duplicates` is told which. */
class QueueResolveDispatcher(queue: TaskQueue, duplicates: ResolveDuplicateMetrics = ResolveDuplicateMetrics.noop)
    extends ResolveDispatcher {
  def dispatch(title:         String,
               year:          Option[Int],
               originalTitle: Option[String],
               director:      Option[String],
               mode:          ResolveMode): Unit = {
    val dedupKey = EnrichTaskKeys.resolveTmdbDedup(title, year)
    queue.enqueue(TaskType.ResolveTmdb, dedupKey,
      EnrichTaskKeys.resolveTmdbPayload(title, year, director, originalTitle, mode)) match {
      case EnqueueResult.Duplicate =>
        ResolveDispatcher.onDuplicate(mode, duplicates)(queue.amendWaiting(dedupKey, EnrichTaskKeys.modeFields(mode)))
      case _ => ()
    }
  }
}

object ResolveDispatcher {
  /** THE duplicate rule both dispatchers follow: a dispatch that finds its row's resolve
   *  already pending tries to `raise` the pending one to its mode — which succeeds only while
   *  that resolve is still WAITING, and never lowers a mode (see [[EnrichTaskKeys.raisedMode]])
   *  — and a re-try's outcome is counted. A plain duplicate adds nothing to raise, so it is
   *  neither attempted nor counted. */
  def onDuplicate(mode: ResolveMode, duplicates: ResolveDuplicateMetrics)(raise: => Boolean): Unit =
    if (mode != ResolveMode.Normal) duplicates.recordDuplicate(mode, upgraded = raise)
}

/** What became of a re-try (RetryMiss / Force) resolve that found its film's resolve already
 *  queued: `upgraded` onto the waiting task, or not — the task was already claimed with its
 *  old mode, so this re-try's search did not happen. A plain duplicate loses nothing and is not
 *  reported. Before 2026-09-23 every such re-try was dropped, and the enqueue counter's
 *  `deduped` could not tell a lost re-try from a harmless duplicate. */
trait ResolveDuplicateMetrics {
  def recordDuplicate(mode: ResolveMode, upgraded: Boolean): Unit
}

object ResolveDuplicateMetrics {
  val noop: ResolveDuplicateMetrics = (_, _) => ()
}

/** Default (unit specs, scripts, Mongo-less dev, the fixture/determinism harness):
 *  resolve INLINE on `ec`, deduped by the row's `CacheKey` via `dedupKey` so the
 *  same key doesn't run twice concurrently (the task queue's job in production). */
class InlineResolveDispatcher(
  ec:         ExecutionContextExecutorService,
  dedupKey:   (String, Option[Int]) => CacheKey,
  resolve:    (String, Option[Int], Option[String], Option[String], ResolveMode) => Unit,
  duplicates: ResolveDuplicateMetrics = ResolveDuplicateMetrics.noop
) extends ResolveDispatcher {
  // Each pending key's state, mirroring a queued task's: `Some(mode)` while it WAITS for a
  // pool slot (a duplicate may still raise its mode, as `amendWaiting` does), `None` once it
  // is RUNNING with the mode it started with (a duplicate is too late, as for a claimed task).
  private val pending = new ConcurrentHashMap[CacheKey, Option[ResolveMode]]()
  private val pool    = new tools.DrainablePool(ec)

  def dispatch(title:         String,
               year:          Option[Int],
               originalTitle: Option[String],
               director:      Option[String],
               mode:          ResolveMode): Unit = {
    val key = dedupKey(title, year)
    // One atomic step decides new / waiting / running, so a dispatch can never slip between
    // a resolve finishing and its key being freed.
    var submit = false
    var raised = false
    pending.compute(key, (_, state) => state match {
      case null           => submit = true; Some(mode)
      case Some(waiting)  => val next = EnrichTaskKeys.raisedMode(waiting, mode); raised = next != waiting; Some(next)
      case running @ None => running
    })
    if (submit)
      pool.submit {
        // Take the (possibly raised) mode and mark the key running in one step, so a
        // duplicate lands either before (and raises it) or after (and is too late).
        val started = Option(pending.replace(key, None)).flatten.getOrElse(mode)
        try resolve(title, year, originalTitle, director, started) finally { pending.remove(key); () }
      }
    else ResolveDispatcher.onDuplicate(mode, duplicates)(raised)
  }

  override def drain(): Unit = pool.drain()

  override def stop(): Unit = pool.stop()
}
