package tools

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference, AtomicReferenceArray}
import scala.concurrent.duration._
import scala.util.{Failure, Try}

/**
 * Map a function over items on a small worker pool that backs off when the
 * upstream says so — the shape the `external-api-rate-limits` skill asks of any
 * script making many round-trips to one service: a fixed pool of a few workers,
 * HALVED on each 429/503, and throughput reported at the end.
 *
 * A call `f` answers with a throttle signal (`isThrottle`) halves the pool
 * (never below one worker), sleeps that worker for `backoff` × the attempt, and
 * goes back on the queue; after `maxAttempts` throttles the item keeps its last
 * failure. Any other failure is the item's result at once. Workers above the
 * current size finish the item in hand and stop. A FATAL error (which `Try` does
 * not catch) stops every worker and is rethrown from `map` — its item would
 * otherwise never be answered, and the rest of the pool would wait for it forever.
 */
object AdaptiveParallel {

  final case class Stats(items: Int, calls: Int, elapsed: FiniteDuration, startWorkers: Int, endWorkers: Int) {
    def summary: String = {
      val seconds = elapsed.toMillis / 1000.0
      f"$items items, $calls requests in $seconds%.1fs (~${if (seconds > 0) calls / seconds else calls.toDouble}%.1f req/s), " +
        s"workers $startWorkers → $endWorkers"
    }
  }

  def map[A, B](items: Seq[A], workers: Int, backoff: FiniteDuration = 2.seconds, maxAttempts: Int = 4,
                sleep: FiniteDuration => Unit)
               (isThrottle: Throwable => Boolean)(f: A => B): (Seq[(A, Try[B])], Stats) = {
    val started = Stopwatch.start()
    val run     = new Run(items, math.max(1, workers), backoff, maxAttempts, sleep, isThrottle, f)
    val out     = run.complete()
    (out, Stats(items.size, run.calls, started.elapsed, math.max(1, workers), run.allowedAtEnd))
  }

  /** One [[map]]: its queue of (item index, attempts so far), its results, and the pool of `workers` threads that
   *  drain it, shrinking on each throttle. */
  private final class Run[A, B](items: Seq[A], workers: Int, backoff: FiniteDuration, maxAttempts: Int,
                                sleep: FiniteDuration => Unit, isThrottle: Throwable => Boolean, f: A => B) {
    private val queue     = new ConcurrentLinkedQueue[(Int, Int)]()
    items.indices.foreach(i => queue.add(i -> 0))
    private val results   = new AtomicReferenceArray[Try[B]](items.size)
    private val allowed   = new AtomicInteger(workers)
    private val remaining = new AtomicInteger(items.size)   // items without a final result yet
    private val callCount = new AtomicInteger(0)
    private val fatal     = new AtomicReference[Throwable](null)

    def calls: Int        = callCount.get
    def allowedAtEnd: Int = allowed.get

    /** Run every item to its final result on the pool, rethrowing the first fatal error a worker met. */
    def complete(): Seq[(A, Try[B])] = {
      val threads = (0 until workers).map { i =>
        val t = new Thread(() => guarded(i), s"adaptive-parallel-$i")
        t.setDaemon(true)
        t.start()
        t
      }
      threads.foreach(_.join())
      Option(fatal.get).foreach(throw _)
      items.indices.map(i => items(i) -> Option(results.get(i)).getOrElse(
        Failure(new IllegalStateException("never processed"))))
    }

    private def guarded(worker: Int): Unit =
      AnyThrowable.captured(work(worker)).left.foreach(t => fatal.compareAndSet(null, t): Unit)

    // Done is ONE atomic read of "items still owed a result", never the pair
    // "nothing in flight" + "queue empty": between those two reads another worker
    // could take the last item, be throttled, shrink the pool below its own index
    // and leave with the item re-queued — and every worker would already be gone.
    // Worker 0 is never retired (the pool never drops below one), so a re-queued
    // item always has a taker.
    @scala.annotation.tailrec
    private def work(worker: Int): Unit =
      if (remaining.get != 0 && worker < allowed.get && fatal.get == null) {
        Option(queue.poll()) match {
          case None                    => sleep(50.millis)   // another worker holds the rest; one may still come back
          case Some((index, attempts)) => attempt(index, attempts)
        }
        work(worker)
      }

    /** One call of `f` on item `index`: its final result, or -- throttled, with attempts left -- the pool halved
     *  and the item back on the queue after a backoff. */
    private def attempt(index: Int, attempts: Int): Unit = {
      callCount.incrementAndGet()
      Try(f(items(index))) match {
        case Failure(e) if isThrottle(e) && attempts + 1 < maxAttempts =>
          allowed.updateAndGet(n => math.max(1, n / 2))
          sleep(backoff * (attempts + 1).toLong)
          queue.add(index -> (attempts + 1))
        case outcome =>
          results.set(index, outcome)
          remaining.decrementAndGet()
      }
    }
  }
}
