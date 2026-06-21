package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.tasks.{EnrichTaskKeys, InMemoryTaskQueue, TaskType}
import tools.DaemonExecutors

import java.time.Instant
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._

/**
 * Direct unit tests for the two `ResolveDispatcher` impls extracted from
 * `MovieService`'s old inline/queue flag-fork. The queue path is also asserted
 * end-to-end via `MovieServiceResolveTaskSpec`; here we exercise the dispatch
 * seams in isolation — the queue enqueue shape and the inline pool's dedup +
 * drain — which is the fail-before/pass-after evidence the new classes behave.
 */
class ResolveDispatcherSpec extends AnyFlatSpec with Matchers {

  "QueueResolveDispatcher" should "enqueue exactly one ResolveTmdb task with the right dedupKey + payload" in {
    val queue = new InMemoryTaskQueue()
    new QueueResolveDispatcher(queue)
      .dispatch("Interstellar", Some(2014), originalTitle = Some("Interstellar"), director = Some("Christopher Nolan"))

    queue.monitor().active.size shouldBe 1
    val task = queue.claim("w", 1.minute, Instant.now()).getOrElse(fail("no ResolveTmdb task enqueued"))
    task.taskType                            shouldBe TaskType.ResolveTmdb
    task.dedupKey                            shouldBe "resolve-tmdb|Interstellar|2014"
    EnrichTaskKeys.titleOf(task.payload)     shouldBe "Interstellar"
    EnrichTaskKeys.yearOf(task.payload)      shouldBe Some(2014)
    EnrichTaskKeys.directorOf(task.payload)  shouldBe Some("Christopher Nolan")
    EnrichTaskKeys.originalTitleOf(task.payload) shouldBe Some("Interstellar")
  }

  private val keyOf: (String, Option[Int]) => CacheKey =
    new CaffeineMovieCache(new InMemoryMovieRepository()).keyOf

  "InlineResolveDispatcher" should "run the resolve callback once for a key" in {
    val ec    = DaemonExecutors.boundedEC("inline-dispatch-test", 4)
    val count = new AtomicInteger(0)
    val ran   = new CountDownLatch(1)
    val d = new InlineResolveDispatcher(ec, keyOf, (_, _, _, _) => { count.incrementAndGet(); ran.countDown() })
    try {
      d.dispatch("A", Some(2020), None, None)
      ran.await(5, java.util.concurrent.TimeUnit.SECONDS) shouldBe true
      d.stop()
      count.get shouldBe 1
    } finally if (!ec.isShutdown) ec.shutdown()
  }

  it should "not run the same key twice while the first is in flight" in {
    val ec      = DaemonExecutors.boundedEC("inline-dispatch-dedup", 4)
    val count   = new AtomicInteger(0)
    val release = new CountDownLatch(1)   // holds the first resolve in-flight
    val started = new CountDownLatch(1)
    val d = new InlineResolveDispatcher(ec, keyOf, (_, _, _, _) => {
      count.incrementAndGet(); started.countDown(); release.await()
    })
    try {
      d.dispatch("A", Some(2020), None, None)
      started.await(5, java.util.concurrent.TimeUnit.SECONDS) shouldBe true
      // Same key, while the first is still blocked → must be dropped, not re-run.
      d.dispatch("A", Some(2020), None, None)
      // A DIFFERENT key still runs.
      val otherRan = new CountDownLatch(1)
      val d2 = new InlineResolveDispatcher(ec, keyOf, (_, _, _, _) => { count.incrementAndGet(); otherRan.countDown() })
      d2.dispatch("B", Some(2020), None, None)
      otherRan.await(5, java.util.concurrent.TimeUnit.SECONDS) shouldBe true
      release.countDown()
      d.stop()
      d2.stop()
      count.get shouldBe 2   // A once + B once; the duplicate A never ran
    } finally {
      release.countDown()
      if (!ec.isShutdown) ec.shutdown()
    }
  }

  it should "drain so an in-flight resolve completes on stop()" in {
    val ec        = DaemonExecutors.boundedEC("inline-dispatch-drain", 4)
    val completed = new AtomicInteger(0)
    val started   = new CountDownLatch(1)
    val d = new InlineResolveDispatcher(ec, keyOf, (_, _, _, _) => {
      started.countDown(); Thread.sleep(150); completed.incrementAndGet()
    })
    try {
      d.dispatch("A", Some(2020), None, None)
      started.await(5, java.util.concurrent.TimeUnit.SECONDS) shouldBe true
      d.stop()                       // blocks until the pool drains
      completed.get shouldBe 1
    } finally if (!ec.isShutdown) ec.shutdown()
  }
}
