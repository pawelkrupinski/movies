package services.metrics

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicInteger

/**
 * Locks the property that broke the worker's Grafana panels: the `/metrics` read
 * path must NOT perform the (Mongo-backed, 10s-Await) render. The handler used to
 * call [[WorkerTaskMetrics.scrape]] inline, so a slow Mongo read blew the 10s
 * `scrape_timeout` and recorded `up=0` (every kinowo_worker_* series gapped).
 * [[MetricsSnapshotCache]] serves the last rendered bytes off the request path —
 * a non-blocking read that never re-invokes `render`.
 */
class MetricsSnapshotCacheSpec extends AnyFlatSpec with Matchers {

  "current()" should "serve the last rendered snapshot WITHOUT re-invoking the (slow) renderer" in {
    val renders = new AtomicInteger(0)
    val cache = new MetricsSnapshotCache(render = () => s"snapshot-v${renders.incrementAndGet()}", clock = _root_.tools.SpecClock.Pinned)

    cache.refresh()
    renders.get() shouldBe 1
    new String(cache.current(), "UTF-8") shouldBe "snapshot-v1"

    // Repeated reads off the request path don't touch the renderer (so they can't
    // block on Mongo) — this is the whole point of the cache.
    cache.current(); cache.current()
    renders.get() shouldBe 1
  }

  it should "keep serving the last good snapshot when a refresh fails" in {
    @volatile var fail = false
    val cache = new MetricsSnapshotCache(render = () =>
      if (fail) throw new RuntimeException("mongo timeout") else "good", clock = _root_.tools.SpecClock.Pinned)

    cache.refresh()
    new String(cache.current(), "UTF-8") shouldBe "good"

    fail = true
    noException should be thrownBy cache.refresh() // a transient blip must not propagate
    new String(cache.current(), "UTF-8") shouldBe "good" // last good, not empty (no gap)
  }

  it should "return immediately while a slow refresh is in flight, then publish the new value" in {
    val release  = new CountDownLatch(1)
    val inRender = new CountDownLatch(1)
    val calls    = new AtomicInteger(0)
    val cache = new MetricsSnapshotCache(render = () =>
      if (calls.incrementAndGet() == 1) "first"
      else { inRender.countDown(); release.await(); "second" }, clock = _root_.tools.SpecClock.Pinned)

    cache.refresh() // first render → "first"
    new String(cache.current(), "UTF-8") shouldBe "first"

    val slow = new Thread(() => cache.refresh()) // second render blocks inside render()
    slow.start()
    inRender.await() // the refresh is now parked inside render()

    // The read must not wait on the in-flight render — it returns the last good value.
    new String(cache.current(), "UTF-8") shouldBe "first"

    release.countDown()
    slow.join()
    new String(cache.current(), "UTF-8") shouldBe "second"
  }

  /** A scheduler that runs what it is handed on the calling thread. */
  private final class Inline extends java.util.concurrent.ScheduledThreadPoolExecutor(1) {
    override def execute(command: Runnable): Unit = command.run()
  }

  // It re-rendered every 10 s under a 30 s scrape interval: two renders in three — each a thousand
  // active tasks read from Mongo — were never served.
  "serve()" should "render once per scrape, for the next one, and never on a timer" in {
    val renders = new AtomicInteger(0)
    val clock   = new tools.MutableClock(java.time.Instant.EPOCH)
    val cache   = new MetricsSnapshotCache(render = () => s"v${renders.incrementAndGet()}",
      minRefresh = scala.concurrent.duration.Duration(10, "seconds"), scheduler = new Inline, clock = clock)
    cache.start()
    renders.get() shouldBe 1
    clock.advanceMillis(60000)                                       // a minute with nobody scraping: no render
    renders.get() shouldBe 1
    new String(cache.serve(), "UTF-8") shouldBe "v1"        // served the last render, and asks for the next
    renders.get() shouldBe 2
    clock.advanceMillis(30000)
    new String(cache.serve(), "UTF-8") shouldBe "v2"
    renders.get() shouldBe 3
  }

  it should "not render again within its floor however often it is read" in {
    val renders = new AtomicInteger(0)
    val clock   = new tools.MutableClock(java.time.Instant.EPOCH)
    val cache   = new MetricsSnapshotCache(render = () => s"v${renders.incrementAndGet()}",
      minRefresh = scala.concurrent.duration.Duration(10, "seconds"), scheduler = new Inline, clock = clock)
    cache.start()
    clock.advanceMillis(10000)
    (1 to 5).foreach(_ => cache.serve())
    renders.get() shouldBe 2
  }

  it should "not start a second render while one is in flight" in {
    val release  = new CountDownLatch(1)
    val inRender = new CountDownLatch(1)
    val renders  = new AtomicInteger(0)
    val clock    = new tools.MutableClock(java.time.Instant.EPOCH)
    val cache    = new MetricsSnapshotCache(render = () =>
      if (renders.incrementAndGet() == 1) "first" else { inRender.countDown(); release.await(); "slow" },
      minRefresh = scala.concurrent.duration.Duration.Zero, scheduler = java.util.concurrent.Executors.newScheduledThreadPool(2), clock = clock)
    cache.start()
    cache.serve()
    inRender.await()
    (1 to 5).foreach(_ => cache.serve())
    renders.get() shouldBe 2
    release.countDown()
    cache.stop()
  }
}
