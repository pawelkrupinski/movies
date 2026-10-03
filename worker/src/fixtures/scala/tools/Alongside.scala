package tools

import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.{Await, Promise}
import scala.util.Try

/**
 * Work a harness runs BESIDE its critical path rather than on it: a read or a sweep that depends on
 * nothing the path is doing, started on a daemon thread of its own and joined where its result is
 * first needed. A failure surfaces at the join, as it would have where the work used to run.
 *
 * On a dedicated thread, not the global pool: the work blocks on I/O for tens of seconds, which the
 * global pool's few threads are not for.
 */
object Alongside {

  /** Work started beside the caller; [[join]] waits for it and hands back its result or rethrows. */
  final class Started[A] private[Alongside] (result: Promise[A]) {
    def join(within: FiniteDuration = Duration(30, "minutes")): A = Await.result(result.future, within)
  }

  def start[A](label: String)(body: => A): Started[A] = {
    val result = Promise[A]()
    Thread.ofPlatform().daemon().name(label).start(() => { result.complete(Try(body)); () })
    new Started(result)
  }

  /** `first` on this thread with `second` running beside it; both results, once both are in. A
   *  `first` that throws leaves `second` to finish unobserved. */
  def apply[A, B](first: => A)(second: => B): (A, B) = {
    val pending = start("alongside")(second)
    val result  = first
    (result, pending.join())
  }
}
