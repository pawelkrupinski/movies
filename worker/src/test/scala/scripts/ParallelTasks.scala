package scripts

import java.util.concurrent.ExecutionException
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.control.NonFatal

/** Every item's task run on `ec`, waited for together, results in the items' order.
 *
 *  A task's FATAL error (a `LinkageError` such as `NoClassDefFoundError`, a `VirtualMachineError`) fails the wait
 *  rather than hanging it: a plain `Future` never completes on one — the error kills the pool thread and the promise
 *  stays open — so an unbounded `Await.result` behind it waits forever. A stale class on a seeded worktree's
 *  classpath did exactly that to a unit run on 2026-10-19 (50 idle minutes, no error printed by the run). It is
 *  rethrown wrapped in an `ExecutionException`; an ordinary exception is rethrown as itself. */
object ParallelTasks {
  def all[A, B](items: Seq[A])(task: A => B)(using ExecutionContext): Seq[B] =
    Await.result(Future.traverse(items)(item => Future(
      try task(item) catch { case NonFatal(e) => throw e; case fatal: Throwable => throw new ExecutionException(fatal) })),
      Duration.Inf)
}
