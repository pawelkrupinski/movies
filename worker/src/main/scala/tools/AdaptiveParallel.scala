package tools

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.{AtomicInteger, AtomicReferenceArray}
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
 * current size finish the item in hand and stop.
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
                sleep: FiniteDuration => Unit = d => Thread.sleep(d.toMillis))
               (isThrottle: Throwable => Boolean)(f: A => B): (Seq[(A, Try[B])], Stats) = {
    val started  = System.nanoTime()
    val queue    = new ConcurrentLinkedQueue[(Int, Int)]()   // (item index, attempts so far)
    items.indices.foreach(i => queue.add(i -> 0))
    val results  = new AtomicReferenceArray[Try[B]](items.size)
    val allowed  = new AtomicInteger(math.max(1, workers))
    val inFlight = new AtomicInteger(0)
    val calls    = new AtomicInteger(0)

    def work(worker: Int): Unit = {
      var done = false
      while (!done) {
        if (worker >= allowed.get) done = true
        else {
          // Counted in flight BEFORE the poll, so a worker that finds the queue
          // empty can tell "all done" from "another worker holds an item it may
          // still put back".
          inFlight.incrementAndGet()
          try Option(queue.poll()) match {
            case None => ()
            case Some((index, attempts)) =>
              calls.incrementAndGet()
              Try(f(items(index))) match {
                case Failure(e) if isThrottle(e) && attempts + 1 < maxAttempts =>
                  allowed.updateAndGet(n => math.max(1, n / 2))
                  sleep(backoff * (attempts + 1).toLong)
                  queue.add(index -> (attempts + 1))
                case outcome => results.set(index, outcome)
              }
          } finally inFlight.decrementAndGet()
          // In-flight read FIRST: a throttled item is re-queued before its worker
          // leaves flight, so "none in flight, then queue empty" means finished.
          if (inFlight.get == 0 && queue.isEmpty) done = true
          else if (queue.isEmpty) sleep(50.millis)
        }
      }
    }

    val threads = (0 until math.max(1, workers)).map { i =>
      val t = new Thread(() => work(i), s"adaptive-parallel-$i")
      t.setDaemon(true)
      t.start()
      t
    }
    threads.foreach(_.join())

    val out = items.indices.map(i => items(i) -> Option(results.get(i)).getOrElse(
      Failure(new IllegalStateException("never processed"))))
    (out, Stats(items.size, calls.get, (System.nanoTime() - started).nanos, math.max(1, workers), allowed.get))
  }
}
