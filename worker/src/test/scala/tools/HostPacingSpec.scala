package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}

/** The per-host pacing of a capture's live reads: never more at once than the host's limit, the limit halved on every
 *  429 or 503, and a share of one budget per run when runs go side by side. */
class HostPacingSpec extends AnyFlatSpec with Matchers {

  private def throttled(code: Int) = new HttpStatusException(code, "GET", "https://api.themoviedb.org/3/x", None)

  "A host's reads" should "never run more at once than its limit" in {
    val pacing = new HostPacing(budget = 3, sleep = _ => ())
    val inside = new AtomicInteger
    val peak   = new AtomicInteger
    val gate   = new CountDownLatch(1)
    val pool   = Executors.newFixedThreadPool(8)
    (1 to 8).foreach(_ => pool.submit(new Runnable {
      def run(): Unit = pacing("https://api.themoviedb.org/3/x") {
        peak.accumulateAndGet(inside.incrementAndGet(), math.max); gate.await(); inside.decrementAndGet()
      }
    }))
    // three reads are held inside; the other five wait at the host's door until the gate opens
    Eventually.eventually(inside.get shouldBe 3)
    gate.countDown(); pool.shutdown()
    pool.awaitTermination(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe true
    peak.get shouldBe 3
  }

  it should "halve the host's limit on a 429 or a 503, never below one, and retry after a growing back-off" in {
    val slept  = new AtomicLong
    val pacing = new HostPacing(budget = 8, retries = 6, backOffMillis = 100, sleep = ms => { slept.addAndGet(ms); () })
    val calls  = new AtomicInteger
    pacing("https://www.imdb.com/a") { if (calls.incrementAndGet() <= 2) throw throttled(if (calls.get == 1) 429 else 503) else "ok" } shouldBe "ok"
    pacing.limitOf("www.imdb.com") shouldBe 2
    slept.get shouldBe 100 + 200
    (1 to 5).foreach { _ =>
      val n = new AtomicInteger
      pacing("https://www.imdb.com/b") { if (n.incrementAndGet() == 1) throw throttled(429) else "ok" }
    }
    pacing.limitOf("www.imdb.com") shouldBe 1
    pacing.limitOf("api.themoviedb.org") shouldBe 8
  }

  it should "give up after its retries, and pass any other failure straight on" in {
    val pacing = new HostPacing(budget = 4, retries = 2, sleep = _ => ())
    val calls  = new AtomicInteger
    an[HttpStatusException] should be thrownBy pacing("https://x.org/") { calls.incrementAndGet(); throw throttled(429) }
    calls.get shouldBe 3
    val once = new AtomicInteger
    an[IllegalStateException] should be thrownBy pacing("https://y.org/") { once.incrementAndGet(); throw new IllegalStateException("boom") }
    once.get shouldBe 1
  }

  it should "fail a timed-out read at once, without the 429/503 retry, and free its slot" in {
    val slept  = new AtomicLong
    val pacing = new HostPacing(budget = 1, retries = 6, sleep = ms => { slept.addAndGet(ms); () })
    val calls  = new AtomicInteger
    a[java.net.http.HttpTimeoutException] should be thrownBy pacing("https://caching.graphql.imdb.com/") {
      calls.incrementAndGet(); throw new java.net.http.HttpTimeoutException("no complete response within 35000ms")
    }
    calls.get shouldBe 1
    slept.get shouldBe 0
    pacing.limitOf("caching.graphql.imdb.com") shouldBe 1
    // The host's one slot is free again: a read on another thread gets in rather than waiting forever.
    val next = java.util.concurrent.CompletableFuture.supplyAsync(() => pacing("https://caching.graphql.imdb.com/")("ok"))
    next.get(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe "ok"
  }

  "Runs side by side" should "split one host budget between them, each keeping at least one" in {
    HostPacing.share(4, runs = 1) shouldBe 4
    HostPacing.share(4, runs = 2) shouldBe 2
    HostPacing.share(4, runs = 3) shouldBe 1
    HostPacing.share(4, runs = 8) shouldBe 1
  }
}
