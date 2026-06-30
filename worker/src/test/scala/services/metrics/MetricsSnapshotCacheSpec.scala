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
    val cache = new MetricsSnapshotCache(render = () => s"snapshot-v${renders.incrementAndGet()}")

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
      if (fail) throw new RuntimeException("mongo timeout") else "good")

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
      else { inRender.countDown(); release.await(); "second" })

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
}
