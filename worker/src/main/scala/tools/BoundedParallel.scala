package tools

import play.api.Logging

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

/**
 * Run a side-effecting function over a collection with bounded concurrency,
 * blocking until every item is done.
 *
 * The four `*Ratings.refreshAll` walks used to iterate the whole cache
 * serially — hundreds of HTTP round-trips back-to-back, ~90% of the wall-clock
 * spent waiting on a socket. This fans them out across a small pool so the tick
 * finishes in a fraction of the time (and, as an operator-triggered `/tasks`
 * run, doesn't sit on the worker's task lease long enough to be reaped).
 *
 * Like [[ParallelDetailFetch]], each call gets its OWN bounded pool rather than
 * drawing from the shared [[SharedExecutionBudget]]: the caller blocks in
 * `Await` here, and a thread that holds a budget permit while awaiting other
 * budget tasks can deadlock the budget. An own pool sidesteps that entirely.
 *
 * `f` MUST swallow its own per-item failures (the rating walks already wrap
 * each row in `Try`): a throwing `f` fails the whole `Future.sequence`, losing
 * the rest of the batch. `maxConcurrent` is the per-source rate-limit cap —
 * keep it under what the upstream tolerates (Filmweb soft-blocks past ~5).
 */
object BoundedParallel extends Logging {

  def foreach[A](label: String, items: Iterable[A], maxConcurrent: Int)(f: A => Unit): Unit = {
    val seq = items.toSeq
    if (seq.isEmpty) return
    val ec = DaemonExecutors.boundedEC(label, maxConcurrent)
    try {
      val futures = seq.map(a => Future(f(a))(using ec))
      Await.result(Future.sequence(futures)(using implicitly, ec), Duration.Inf)
    } finally ec.shutdown()
  }
}
