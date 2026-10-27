package tools

import java.util.concurrent.{CompletableFuture, TimeUnit, TimeoutException}

/**
 * A call a case expects NOT to block — a get served while a read hangs, a drain that must give up on its backlog — whose
 * failure mode is to wait for good on something the case holds shut. Asserting "it returned within 1 s" measured the
 * machine's load as much as the call (LoadIndependentTestsSpec); this asserts what was meant:
 * it returned at all, while the thing it must not wait on is still shut.
 */
object Answered {

  /** `block`'s answer, run on a daemon thread of its own: a block that never answers fails the case after
   *  [[SpecTimeouts.Io]], naming `what`, instead of hanging the suite. A positive wait's bound, never a timing. */
  def at[A](what: String)(block: => A): A = {
    val answer = new CompletableFuture[A]()
    val caller = new Thread(() => { try answer.complete(block) catch { case t: Throwable => answer.completeExceptionally(t) }; () }, s"answered: $what")
    caller.setDaemon(true)
    caller.start()
    try answer.get(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS)
    catch {
      case _: TimeoutException => org.scalatest.Assertions.fail(s"$what never answered within ${SpecTimeouts.Io}: it waited on what it must not")
      case e: java.util.concurrent.ExecutionException => throw e.getCause
    }
  }
}
