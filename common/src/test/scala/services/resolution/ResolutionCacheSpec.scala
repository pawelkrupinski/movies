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

  // The whole point of `forget`: after it, the next call must genuinely
  // re-resolve. Clearing only the durable store would leave the stale value in
  // the Caffeine layer in front of it, and the caller would never notice.
  "forget" should "drop the memoised value from BOTH layers so the next call re-resolves" in {
    val store = new InMemoryResolutionStore
    val cache = new WriteThroughResolutionCache(store)
    var resolves = 0
    def resolveTo(v: String) = cache.getOrResolve("mc|theodyssey|odyseja|2026") { resolves += 1; Some(v) }

    resolveTo("https://www.metacritic.com/movie/the-odyssey") shouldBe
      Some("https://www.metacritic.com/movie/the-odyssey")
    resolveTo("ignored — served from cache") shouldBe
      Some("https://www.metacritic.com/movie/the-odyssey")
    resolves shouldBe 1

    cache.forget("Odyseja")

    resolveTo("https://www.metacritic.com/movie/the-odyssey-2026") shouldBe
      Some("https://www.metacritic.com/movie/the-odyssey-2026")
    resolves shouldBe 2
    store.get("mc|theodyssey|odyseja|2026") shouldBe
      Some("https://www.metacritic.com/movie/the-odyssey-2026")
  }

  it should "leave other films' entries alone" in {
    val cache = new WriteThroughResolutionCache(new InMemoryResolutionStore)
    cache.getOrResolve("mc|thenorth|polnoc|2026")(Some("north")) shouldBe Some("north")
    cache.forget("Odyseja")
    var resolved = false
    cache.getOrResolve("mc|thenorth|polnoc|2026") { resolved = true; Some("other") } shouldBe Some("north")
    resolved shouldBe false
  }

  // The operator's corpus-wide button calls this before it walks: otherwise the
  // walk re-derives from the very memoised answers it exists to re-check, which
  // is why "Metacritic refresh" could report 0 changed while films sat on the
  // wrong page.
  "forgetAll" should "clear every entry from both layers" in {
    val store = new InMemoryResolutionStore
    val cache = new WriteThroughResolutionCache(store)
    cache.getOrResolve("mc|a||2026")(Some("url-a")) shouldBe Some("url-a")
    cache.getOrResolve("mc|b||2026")(Some("url-b")) shouldBe Some("url-b")

    cache.forgetAll()

    var reresolved = 0
    cache.getOrResolve("mc|a||2026") { reresolved += 1; Some("url-a2") } shouldBe Some("url-a2")
    cache.getOrResolve("mc|b||2026") { reresolved += 1; Some("url-b2") } shouldBe Some("url-b2")
    reresolved shouldBe 2
    store.get("mc|a||2026") shouldBe Some("url-a2")
  }

  it should "be a no-op on the passthrough cache" in {
    noException should be thrownBy ResolutionCache.passthrough.forgetAll()
  }

  // The four outcomes are what `kinowo_worker_resolution_total` charts, and each
  // means something different for the "is this cache worth keeping" question —
  // so each must be reported for exactly the path it names.
  "outcome reporting" should "distinguish the two hit layers from the two miss kinds" in {
    val store    = new InMemoryResolutionStore
    val recorded = scala.collection.mutable.ListBuffer.empty[String]
    def cacheOver(s: ResolutionStore) =
      new WriteThroughResolutionCache(s, (o: String) => { recorded += o; () })

    val cache = cacheOver(store)
    cache.getOrResolve("k")(Some("tt1"))  // cold both layers → resolved live
    cache.getOrResolve("k")(Some("tt1"))  // Caffeine serves it
    cache.getOrResolve("nope")(None)      // chain ran, nothing to cache

    // A fresh Caffeine over the SAME store is the post-restart path: the durable
    // half is the only thing that can still save the chain.
    cacheOver(store).getOrResolve("k")(Some("ignored"))

    recorded.toList shouldBe List(
      ResolutionOutcome.MissResolved,
      ResolutionOutcome.HitMemory,
      ResolutionOutcome.MissUnresolved,
      ResolutionOutcome.HitStore)
  }

  it should "report an uncached miss on EVERY repeat, since hits-only never memoises it" in {
    val recorded = scala.collection.mutable.ListBuffer.empty[String]
    val cache = new WriteThroughResolutionCache(
      new InMemoryResolutionStore, (o: String) => { recorded += o; () })

    (1 to 3).foreach(_ => cache.getOrResolve("unresolvable")(None))

    // Three chains run, three times nothing cached — the load the cache does not
    // absorb, and the reason the counter separates this from `miss_resolved`.
    recorded.toList shouldBe List.fill(3)(ResolutionOutcome.MissUnresolved)
  }

  "sourceOf" should "label a counter by the source its collection serves" in {
    ResolutionOutcome.sourceOf("resolve_rt") shouldBe "rt"
    ResolutionOutcome.sourceOf("resolve_filmweb") shouldBe "filmweb"
  }
}
