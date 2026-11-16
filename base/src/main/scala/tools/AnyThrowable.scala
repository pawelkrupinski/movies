package tools

import scala.annotation.nowarn

/**
 * The two shapes that catch `Throwable` on purpose -- fatal errors (`VirtualMachineError`, `LinkageError`),
 * interrupts and control throwables included -- because neither swallows what it catches:
 *
 *  - [[rethrownAfter]]: clean-up that must run however a block ends abnormally (a waiter completed, a client
 *    closed, a claim forgotten, a failure counted), then the throwable rethrown unchanged. `finally` cannot say
 *    it: it runs on success too.
 *  - [[captured]]: a block's outcome handed to another thread (a worker thread's failure to the thread that joins
 *    it, a computation to the future others wait on): left uncaught, a fatal error would end the thread and leave
 *    its waiter waiting forever.
 *
 * Everywhere else a handler catches `NonFatal(e)` or the exception it means: Scala 3.10's E235 ("Catching
 * Throwable can lead to unexpected behavior") is an error under -Werror, and these two are its only
 * suppressions in main code.
 */
object AnyThrowable {

  /** `body`'s value; when it throws anything at all, `onThrow` sees it first, then it is rethrown. */
  @nowarn("id=E235")
  def rethrownAfter[A](body: => A)(onThrow: Throwable => Unit): A =
    try body
    catch { case thrown: Throwable => onThrow(thrown); throw thrown }

  /** `body`'s value, or whatever it threw, fatal or not. An interrupt it captures keeps its flag on this thread for
   *  whoever owns it: captured, it would otherwise be lost (the worker's shutdown drain steps, a change-stream
   *  callback), and a later wait on the thread would not know it was asked to stop. */
  @nowarn("id=E235")
  def captured[A](body: => A): Either[Throwable, A] =
    try Right(body)
    catch {
      case thrown: Throwable =>
        if (thrown.isInstanceOf[InterruptedException]) Thread.currentThread().interrupt()
        Left(thrown)
    }
}
