package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{CompletableFuture, CountDownLatch, Semaphore, TimeUnit}

class ConcurrencyLimitedHttpFetchSpec extends AnyFlatSpec with Matchers {

  /** A delegate that records peak concurrency and parks each call on `gate` so
   *  several calls overlap, exposing how many the limiter lets run at once. */
  private class BlockingShard(gate: CountDownLatch, inFlight: AtomicInteger, peak: AtomicInteger)
      extends GetOnlyHttpFetch {
    override def get(url: String): String = {
      val now = inFlight.incrementAndGet()
      peak.updateAndGet(m => math.max(m, now))
      try { gate.await(2, TimeUnit.SECONDS); "ok" }
      finally inFlight.decrementAndGet()
    }
  }

  "ConcurrencyLimitedHttpFetch" should "never run more delegate calls at once than the permit count" in {
    val gate     = new CountDownLatch(1)
    val inFlight = new AtomicInteger(0)
    val peak     = new AtomicInteger(0)
    val fetch    = new ConcurrencyLimitedHttpFetch(new BlockingShard(gate, inFlight, peak), new Semaphore(3, true))

    val calls = (1 to 8).map(i => CompletableFuture.supplyAsync(() => fetch.get(s"https://x/$i")))
    Thread.sleep(250) // let all 8 pile up against the 3 permits
    peak.get should be <= 3
    inFlight.get shouldBe 3 // exactly the permit count are running; the other 5 WAIT (not failed)

    gate.countDown()
    calls.foreach(_.get(2, TimeUnit.SECONDS))
    peak.get shouldBe 3
  }

  it should "block on contention rather than throw or fall through" in {
    // One permit, held by a call parked on the gate. A second call must BLOCK,
    // not raise — so completing it within the timeout requires releasing the gate.
    val gate  = new CountDownLatch(1)
    val fetch = new ConcurrencyLimitedHttpFetch(
      new BlockingShard(gate, new AtomicInteger(0), new AtomicInteger(0)), new Semaphore(1, true))

    val first  = CompletableFuture.supplyAsync(() => fetch.get("https://x/1"))
    Thread.sleep(100)
    val second = CompletableFuture.supplyAsync(() => fetch.get("https://x/2"))
    Thread.sleep(100)
    second.isDone shouldBe false // still waiting for the permit, not errored out

    gate.countDown()
    first.get(2, TimeUnit.SECONDS) shouldBe "ok"
    second.get(2, TimeUnit.SECONDS) shouldBe "ok"
  }

  it should "release the permit after a delegate failure so it isn't leaked" in {
    val permits = new Semaphore(1, true)
    val boom = new GetOnlyHttpFetch {
      override def get(url: String): String = throw new RuntimeException("upstream down")
    }
    val fetch = new ConcurrencyLimitedHttpFetch(boom, permits)
    an[RuntimeException] should be thrownBy fetch.get("https://x/1")
    permits.availablePermits() shouldBe 1 // permit returned despite the throw
  }
}
