package modules

import java.util.concurrent.CountDownLatch

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The SIGTERM drain runs every step whatever an earlier one threw. Since `AnyThrowable.captured` keeps a captured
 * interrupt's flag on the thread, a drain that only captured each step left the flag set, and every step after an
 * interrupted one failed at its first wait (an executor's `awaitTermination`, a client's close) instead of releasing
 * its resources.
 */
class WorkerShutdownDrainSpec extends AnyFlatSpec with Matchers {

  "drain" should "run a waiting step after an interrupted one, and hand the interrupt back once it ends" in {
    var waited = false
    try {
      WorkerMain.drain(Seq(
        "interrupted" -> (() => throw new InterruptedException("stopping")),
        // A latch already open still checks the flag first: it throws at once on an interrupted thread.
        "waits"       -> (() => { new CountDownLatch(0).await(); waited = true })))
      waited shouldBe true
      Thread.currentThread().isInterrupted shouldBe true
    } finally { Thread.interrupted(); () }
  }

  it should "leave the thread uninterrupted when no step was interrupted" in {
    var ran = Vector.empty[String]
    WorkerMain.drain(Seq(
      "fails" -> (() => { ran :+= "fails"; throw new IllegalStateException("broken") }),
      "after" -> (() => ran :+= "after")))
    ran shouldBe Vector("fails", "after")
    Thread.currentThread().isInterrupted shouldBe false
  }
}
