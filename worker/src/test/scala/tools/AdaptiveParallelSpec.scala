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
