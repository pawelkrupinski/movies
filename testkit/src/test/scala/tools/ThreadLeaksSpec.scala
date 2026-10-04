package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.{CountDownLatch, Executors}
import scala.concurrent.duration._

class ThreadLeaksSpec extends AnyFlatSpec with Matchers {

  "A thread another suite starts while the subject runs" should "not be taken for the subject's" in {
    val release = new CountDownLatch(1)
    val started = new CountDownLatch(1)
    val other   = new Thread(() => { started.countDown(); release.await() }, "other-suite")
    try {
      ThreadLeaks.of({ other.start(); started.await() }, grace = 10.millis) shouldBe empty
    } finally { release.countDown(); other.join() }
  }

  "A thread the subject starts and leaves running" should "be named, and so should a pool's thread started later" in {
    val release = new CountDownLatch(1)
    var pool    = Option.empty[java.util.concurrent.ExecutorService]
    try {
      val leaks = ThreadLeaks.of({
        new Thread(() => release.await(), "left-running").start()
        val p = Executors.newSingleThreadExecutor()
        pool = Some(p)
        p.submit((() => release.await()): Runnable)
        ()
      }, grace = 10.millis)
      leaks should contain ("left-running")
      leaks.exists(_.startsWith("pool-")) shouldBe true
    } finally { release.countDown(); pool.foreach(_.shutdownNow()) }
  }

  "A subject that stops its threads" should "leave none" in {
    ThreadLeaks.of {
      val p = Executors.newFixedThreadPool(2)
      p.submit((() => ()): Runnable)
      p.shutdown()
    } shouldBe empty
  }
}
