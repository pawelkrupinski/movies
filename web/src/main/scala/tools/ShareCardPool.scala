package tools

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue, RejectedExecutionException, SynchronousQueue, ThreadPoolExecutor, TimeUnit}
import scala.concurrent.{Future, Promise}
import scala.util.Try

/**
 * The threads the share cards (`/:city/movie/og-image`, `/:city/og-image`) render on, and the
 * bound on how much card work may wait for them.
 *
 * A card render BLOCKS: a poster download with a 15s connect / 20s read budget
 * ([[HttpPosterFetch]]), a decode behind the process-wide [[PosterDecodeGate]], then the
 * composite. Run as a plain `Action` it did all of that on Play's default dispatcher -- the pool
 * every page on the site is served from -- so a crawler sweeping the cards (AhrefsBot, 612 requests
 * on 2026-09-21 17:01-18:25Z) held those threads in poster I/O, and the city pages' p95 rose to
 * 1.4-2.1s alongside the og-image endpoint's own ~2s.
 *
 * FULL MEANS "NO", NOT "WAIT". Once `threads` renders are running and `queueDepth` more are
 * waiting, [[submit]] refuses at once and the caller answers 503 with a `Retry-After` instead of
 * queueing without bound -- a preview scraper retries, and a queue that grows with a crawler is
 * the memory problem this pool exists to bound.
 */
class ShareCardPool(threads: Int, queueDepth: Int) {
  private val queue: BlockingQueue[Runnable] =
    if (queueDepth > 0) new ArrayBlockingQueue[Runnable](queueDepth) else new SynchronousQueue[Runnable]()

  private val executor = {
    val counter = new AtomicInteger(0)
    val pool = new ThreadPoolExecutor(threads, threads, 60, TimeUnit.SECONDS, queue, (task: Runnable) => {
      val thread = new Thread(task, s"${ShareCardPool.ThreadPrefix}-${counter.incrementAndGet()}")
      thread.setDaemon(true)
      thread
    }, new ThreadPoolExecutor.AbortPolicy())
    // Idle threads go away, so a pool that has not rendered a card since boot holds none.
    pool.allowCoreThreadTimeOut(true)
    pool
  }

  /** `work` running on this pool, or None -- at once -- when the pool and its queue are full. */
  def submit[A](work: => A): Option[Future[A]] = {
    val promise = Promise[A]()
    try {
      executor.execute(() => promise.complete(Try(work)))
      Some(promise.future)
    } catch { case _: RejectedExecutionException => None }
  }
}

object ShareCardPool {
  /** What a card render's thread is called, in a thread dump or a spec. */
  val ThreadPrefix = "og-card"

  /** Four renders at once, sixteen waiting. The decode itself is gated at two
   *  ([[PosterDecodeGate.Shared]]); the other two threads keep a slow poster DOWNLOAD from holding
   *  up a card whose poster is already fast. Sixteen waiting is several seconds of work at a cold
   *  render's pace -- enough for any burst of real shares, not enough for a crawler to pile up. */
  def production(): ShareCardPool = new ShareCardPool(threads = 4, queueDepth = 16)
}
