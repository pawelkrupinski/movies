package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._

class AdaptiveParallelSpec extends AnyFlatSpec with Matchers {

  private case object Throttled extends RuntimeException("429")
  private val noSleep: FiniteDuration => Unit = _ => ()

  "AdaptiveParallel.map" should "answer every item, in order" in {
    val (results, stats) = AdaptiveParallel.map(1 to 50, workers = 6, sleep = noSleep)(_ => false)(_ * 2)
    results.map(_._2.get) shouldBe (1 to 50).map(_ * 2)
    stats.calls shouldBe 50
    stats.endWorkers shouldBe 6
  }

  it should "halve the pool on each throttle, and retry the throttled item" in {
    val throttledOnce = TrieMap.empty[Int, Unit]
    val (results, stats) = AdaptiveParallel.map(1 to 20, workers = 8, sleep = noSleep)(_ == Throttled) { i =>
      if (i <= 2 && throttledOnce.putIfAbsent(i, ()).isEmpty) throw Throttled else i
    }
    results.map(_._2.get) shouldBe (1 to 20)
    stats.calls shouldBe 22
    stats.endWorkers shouldBe 2   // 8 → 4 → 2
  }

  it should "never shrink below one worker, and give up on an item after maxAttempts" in {
    val calls = new AtomicInteger(0)
    val (results, stats) = AdaptiveParallel.map(Seq("x"), workers = 4, maxAttempts = 3, sleep = noSleep)(_ == Throttled) { _ =>
      calls.incrementAndGet(); throw Throttled
    }
    results.head._2.failed.get shouldBe Throttled
    calls.get shouldBe 3
    stats.endWorkers shouldBe 1
  }

  // Workers used to decide "all done" from two separate reads — nothing in
  // flight, then queue empty. Between them another worker could take the last
  // item, be throttled, shrink the pool below its own index and leave; everyone
  // was then gone and the item came back "never processed" (seen in a full
  // unit run). Repeated so the interleaving actually gets a chance to occur.
  it should "never abandon an item when a throttle retires the worker holding it" in {
    (1 to 3000).foreach { _ =>
      val (results, _) = AdaptiveParallel.map(Seq("x"), workers = 16, maxAttempts = 5, sleep = noSleep)(_ == Throttled) { _ =>
        Thread.`yield`(); throw Throttled
      }
      results.head._2.failed.get shouldBe Throttled
    }
  }

  // `Try` does not catch a fatal error, so it killed its worker thread with the item
  // still owed a result: every other worker then waited on it forever, and the weekly
  // roster audit hung until the job's timeout instead of failing.
  it should "fail the run on a fatal error in f, rather than wait forever for the dead worker's item" in {
    // A plain thread, not a Future: a Future never completes on a fatal error either.
    val thrown = new java.util.concurrent.atomic.AtomicReference[Throwable]()
    val run = new Thread(() =>
      try { AdaptiveParallel.map(1 to 10, workers = 3, sleep = noSleep)(_ => false) { i =>
        if (i == 4) throw new LinkageError("fatal") else i
      }; () } catch { case t: Throwable => thrown.set(t) })
    run.setDaemon(true)
    run.start()
    run.join(10.seconds.toMillis)
    withClue("map still running after 10s: ") { run.isAlive shouldBe false }
    thrown.get shouldBe a[LinkageError]
  }

  it should "keep a non-throttle failure as that item's answer, without retrying" in {
    val (results, stats) = AdaptiveParallel.map(Seq(1, 2), workers = 2, sleep = noSleep)(_ == Throttled) { i =>
      if (i == 1) throw new IllegalArgumentException("bad") else i
    }
    results.head._2.isFailure shouldBe true
    results(1)._2.get shouldBe 2
    stats.calls shouldBe 2
    stats.summary should include ("req/s")
  }
}
