package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ConcurrentLinkedQueue, CyclicBarrier, TimeUnit}
import scala.jdk.CollectionConverters._

class BoundedParallelSpec extends AnyFlatSpec with Matchers {

  "BoundedParallel.foreach" should "run items concurrently up to the cap (serial execution would deadlock the barrier)" in {
    val n = 4
    // A barrier of `n` parties only trips if all `n` tasks are inside `f` at the
    // same moment — impossible under serial execution, which would block on the
    // first await forever. The await timeout turns a serial regression into a
    // test failure rather than a hang.
    val barrier  = new CyclicBarrier(n)
    val maxSeen  = new AtomicInteger(0)
    val inFlight = new AtomicInteger(0)

    BoundedParallel.foreach("test-concurrent", 1 to n, maxConcurrent = n) { _ =>
      val cur = inFlight.incrementAndGet()
      maxSeen.updateAndGet(m => math.max(m, cur))
      barrier.await(5, TimeUnit.SECONDS)
      inFlight.decrementAndGet()
      ()
    }

    maxSeen.get shouldBe n
  }

  it should "process every item exactly once" in {
    val seen = new ConcurrentLinkedQueue[Int]()
    BoundedParallel.foreach("test-all", 1 to 50, maxConcurrent = 8) { i =>
      seen.add(i); ()
    }
    seen.asScala.toList.sorted shouldBe (1 to 50).toList
  }

  it should "be a no-op on an empty collection" in {
    var ran = false
    BoundedParallel.foreach("test-empty", Seq.empty[Int], maxConcurrent = 4) { _ => ran = true }
    ran shouldBe false
  }
}
