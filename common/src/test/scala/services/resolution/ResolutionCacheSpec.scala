package services.resolution

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.{Clock, Instant, ZoneId, ZoneOffset}
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}

/** A clock the test can advance, to drive [[ResolutionStore]]'s 24h expiry. */
private class MutableClock(start: Instant) extends Clock {
  @volatile private var now: Instant = start
  def advance(d: java.time.Duration): Unit = now = now.plus(d)
  override def instant(): Instant = now
  override def getZone: ZoneId = ZoneOffset.UTC
  override def withZone(zone: ZoneId): Clock = this
}

class ResolutionCacheSpec extends AnyFlatSpec with Matchers {

  private def newCache(store: ResolutionStore = new InMemoryResolutionStore()) =
    new WriteThroughResolutionCache(store)

  "WriteThroughResolutionCache" should "resolve once and reuse the cached hit" in {
    val calls = new AtomicInteger(0)
    val cache = newCache()
    def resolve(): Option[String] = { calls.incrementAndGet(); Some("tt123") }

    cache.getOrResolve("k")(resolve()) shouldBe Some("tt123")
    cache.getOrResolve("k")(resolve()) shouldBe Some("tt123")
    calls.get() shouldBe 1
  }

  it should "NOT cache a miss — a None result re-runs the resolver next time" in {
    val calls = new AtomicInteger(0)
    val cache = newCache()
    def resolve(): Option[String] = { calls.incrementAndGet(); None }

    cache.getOrResolve("k")(resolve()) shouldBe None
    cache.getOrResolve("k")(resolve()) shouldBe None
    calls.get() shouldBe 2
  }

  it should "write a hit through to the durable store" in {
    val store = new InMemoryResolutionStore()
    val cache = newCache(store)

    cache.getOrResolve("k")(Some("tt123"))
    store.get("k") shouldBe Some("tt123")
  }

  it should "warm a cold cache from the store without resolving" in {
    val store = new InMemoryResolutionStore()
    store.put("k", "tt999")
    val calls = new AtomicInteger(0)
    val cache = newCache(store) // fresh Caffeine, value only in the store

    cache.getOrResolve("k") { calls.incrementAndGet(); Some("other") } shouldBe Some("tt999")
    calls.get() shouldBe 0
  }

  it should "collapse concurrent identical misses to a single resolve" in {
    val calls = new AtomicInteger(0)
    val cache = newCache()
    val threads = 16
    val start = new CountDownLatch(1)
    val pool = Executors.newFixedThreadPool(threads)
    val tasks = (1 to threads).map(_ => pool.submit(new Runnable {
      override def run(): Unit = {
        start.await()
        cache.getOrResolve("k") { calls.incrementAndGet(); Thread.sleep(20); Some("tt1") }
      }
    }))
    start.countDown()
    tasks.foreach(_.get())
    pool.shutdown()
    pool.awaitTermination(5, TimeUnit.SECONDS)
    calls.get() shouldBe 1
  }

  "InMemoryResolutionStore" should "treat a value older than the 24h TTL as absent" in {
    val clock = new MutableClock(Instant.parse("2026-06-15T00:00:00Z"))
    val store = new InMemoryResolutionStore(clock)
    store.put("k", "tt1")
    store.get("k") shouldBe Some("tt1")

    clock.advance(java.time.Duration.ofHours(23))
    store.get("k") shouldBe Some("tt1") // still fresh

    clock.advance(java.time.Duration.ofHours(2)) // now 25h old
    store.get("k") shouldBe None
  }
}
