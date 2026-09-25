package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.tasks.{EnrichTaskKeys, InMemoryTaskQueue, ResolveMode, TaskType}
import tools.DaemonExecutors

import java.time.Instant
import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration._
import services.movies.SingleCountryNormalizer.titleNormalizer

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
    val task = queue.claim("w", 1.minute, Instant.EPOCH).getOrElse(fail("no ResolveTmdb task enqueued"))
    task.taskType                            shouldBe TaskType.ResolveTmdb
    task.dedupKey                            shouldBe "resolve-tmdb|Interstellar|2014"
    EnrichTaskKeys.titleOf(task.payload)     shouldBe "Interstellar"
    EnrichTaskKeys.yearOf(task.payload)      shouldBe Some(2014)
    EnrichTaskKeys.directorOf(task.payload)  shouldBe Some("Christopher Nolan")
    EnrichTaskKeys.originalTitleOf(task.payload) shouldBe Some("Interstellar")
    EnrichTaskKeys.modeOf(task.payload)      shouldBe ResolveMode.Normal
  }

  it should "carry `force` into the payload, so an already-resolved row re-resolves" in {
    // The stale-language sweep re-resolves rows that already have a tmdbId; without
    // `force` on the payload the handler treats them as done and the wrong-language
    // Tmdb slot stays frozen.
    val queue = new InMemoryTaskQueue()
    new QueueResolveDispatcher(queue).dispatch("Die Odyssee", Some(2026), None, None, ResolveMode.Force)

    val task = queue.claim("w", 1.minute, Instant.EPOCH).getOrElse(fail("no ResolveTmdb task enqueued"))
    EnrichTaskKeys.modeOf(task.payload) shouldBe ResolveMode.Force
  }

  it should "upgrade a WAITING normal task to a re-try instead of dropping the re-try" in {
    // The same dedupKey serves every mode, so a re-try that lands while a plain resolve is
    // still queued came back Duplicate and was lost — and the plain resolve then stopped at
    // the very remembered miss the re-try existed to look past, for another 24h.
    val queue = new InMemoryTaskQueue()
    val dispatcher = new QueueResolveDispatcher(queue)
    dispatcher.dispatch("Tosca", None, None, None)
    dispatcher.dispatch("Tosca", None, None, None, ResolveMode.RetryMiss)

    queue.monitor().active.size shouldBe 1
    val task = queue.claim("w", 1.minute, Instant.EPOCH).getOrElse(fail("no ResolveTmdb task enqueued"))
    EnrichTaskKeys.modeOf(task.payload) shouldBe ResolveMode.RetryMiss
  }

  it should "never downgrade a queued task: a later plain or re-try dispatch leaves Force in place" in {
    val queue = new InMemoryTaskQueue()
    val dispatcher = new QueueResolveDispatcher(queue)
    dispatcher.dispatch("Tosca", None, None, None, ResolveMode.Force)
    dispatcher.dispatch("Tosca", None, None, None, ResolveMode.RetryMiss)
    dispatcher.dispatch("Tosca", None, None, None)

    val task = queue.claim("w", 1.minute, Instant.EPOCH).getOrElse(fail("no ResolveTmdb task enqueued"))
    EnrichTaskKeys.modeOf(task.payload) shouldBe ResolveMode.Force
  }

  // A re-try that finds its film's resolve already queued used to be DROPPED by the dedup, and
  // the loss was invisible: the enqueue counter says `deduped` for a harmless duplicate plain
  // resolve and for a lost re-try alike. What happened to each non-plain duplicate is counted:
  // upgraded onto the waiting task, or not (the task was already claimed with its old mode).
  it should "count each re-try that landed on a queued resolve as upgraded, or not when it was already claimed" in {
    val queue    = new InMemoryTaskQueue()
    val recorded = scala.collection.mutable.Buffer.empty[(ResolveMode, Boolean)]
    val dispatcher = new QueueResolveDispatcher(queue, (mode, upgraded) => recorded += (mode -> upgraded))
    dispatcher.dispatch("Tosca", None, None, None)
    dispatcher.dispatch("Tosca", None, None, None)                        // a plain duplicate: nothing lost, not counted
    dispatcher.dispatch("Tosca", None, None, None, ResolveMode.RetryMiss) // waiting → upgraded
    queue.claim("w", 1.minute, Instant.EPOCH).getOrElse(fail("no ResolveTmdb task enqueued"))
    dispatcher.dispatch("Tosca", None, None, None, ResolveMode.Force)     // being worked on → not upgraded

    recorded.toSeq shouldBe Seq(ResolveMode.RetryMiss -> true, ResolveMode.Force -> false)
  }

  // A re-try whose waiting resolve already runs at its mode (or above it) changes nothing, and
  // loses nothing either: its search still happens. Counting it `upgraded = false` read as a
  // lost re-try on the dashboard.
  it should "count a re-try onto a queued resolve already at its mode or above as upgraded" in {
    val queue    = new InMemoryTaskQueue()
    val recorded = scala.collection.mutable.Buffer.empty[(ResolveMode, Boolean)]
    val dispatcher = new QueueResolveDispatcher(queue, (mode, upgraded) => recorded += (mode -> upgraded))
    dispatcher.dispatch("Tosca", None, None, None, ResolveMode.RetryMiss)
    dispatcher.dispatch("Tosca", None, None, None, ResolveMode.RetryMiss) // already at RetryMiss
    dispatcher.dispatch("Tosca", None, None, None, ResolveMode.Force)
    dispatcher.dispatch("Tosca", None, None, None, ResolveMode.RetryMiss) // Force covers it

    recorded.toSeq shouldBe Seq(ResolveMode.RetryMiss -> true, ResolveMode.Force -> true, ResolveMode.RetryMiss -> true)
  }

  private val keyOf: (String, Option[Int]) => CacheKey =
    new CaffeineMovieCache(new InMemoryMovieRepository(normalizer = titleNormalizer), normalizer = titleNormalizer).keyOf

  "InlineResolveDispatcher" should "run the resolve callback once for a key" in {
    val ec    = DaemonExecutors.boundedEC("inline-dispatch-test", 4)
    val count = new AtomicInteger(0)
    val ran   = new CountDownLatch(1)
    val d = new InlineResolveDispatcher(ec, keyOf, (_, _, _, _, _) => { count.incrementAndGet(); ran.countDown() })
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
    val d = new InlineResolveDispatcher(ec, keyOf, (_, _, _, _, _) => {
      count.incrementAndGet(); started.countDown(); release.await()
    })
    try {
      d.dispatch("A", Some(2020), None, None)
      started.await(5, java.util.concurrent.TimeUnit.SECONDS) shouldBe true
      // Same key, while the first is still blocked → must be dropped, not re-run.
      d.dispatch("A", Some(2020), None, None)
      // A DIFFERENT key still runs.
      val otherRan = new CountDownLatch(1)
      val d2 = new InlineResolveDispatcher(ec, keyOf, (_, _, _, _, _) => { count.incrementAndGet(); otherRan.countDown() })
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

  // The queue's rule, inline: a re-try that finds its key's resolve still WAITING (submitted,
  // not started — the pool is busy) raises that resolve's mode instead of being dropped.
  it should "upgrade a WAITING resolve to a re-try's mode, as the queue does" in {
    val ec      = DaemonExecutors.boundedEC("inline-dispatch-upgrade", 1)
    val release = new CountDownLatch(1)
    val started = new CountDownLatch(1)
    val ranWith = new java.util.concurrent.ConcurrentHashMap[String, ResolveMode]()
    val done    = new CountDownLatch(2)
    val d = new InlineResolveDispatcher(ec, keyOf, (title, _, _, _, mode) => {
      if (title == "Busy") { started.countDown(); release.await(5, java.util.concurrent.TimeUnit.SECONDS) }
      ranWith.put(title, mode); done.countDown()
    })
    try {
      d.dispatch("Busy", Some(2020), None, None)                         // occupies the pool's one slot
      // Each task is its own virtual thread racing for the pool's one permit, in no promised
      // order: until Busy holds it, Tosca could start first (at Normal) and the re-try below
      // would rightly be too late.
      started.await(5, java.util.concurrent.TimeUnit.SECONDS) shouldBe true
      d.dispatch("Tosca", None, None, None)                              // waits behind it
      d.dispatch("Tosca", None, None, None, ResolveMode.RetryMiss)       // must raise the waiting one
      d.dispatch("Tosca", None, None, None)                              // and a plain one never lowers it
      release.countDown()
      done.await(5, java.util.concurrent.TimeUnit.SECONDS) shouldBe true
      d.stop()
      ranWith.get("Tosca") shouldBe ResolveMode.RetryMiss
    } finally { release.countDown(); if (!ec.isShutdown) ec.shutdown() }
  }

  it should "count each re-try that landed on a pending resolve as upgraded, or not once it has started" in {
    val ec       = DaemonExecutors.boundedEC("inline-dispatch-dup-metrics", 1)
    val release  = new CountDownLatch(1)
    val started  = new CountDownLatch(1)
    val recorded = new java.util.concurrent.ConcurrentLinkedQueue[(ResolveMode, Boolean)]()
    val d = new InlineResolveDispatcher(ec, keyOf, (title, _, _, _, _) =>
      if (title == "Tosca") { started.countDown(); release.await(5, java.util.concurrent.TimeUnit.SECONDS) },
      duplicates = (mode, upgraded) => { recorded.add(mode -> upgraded); () })
    try {
      d.dispatch("Tosca", None, None, None)
      started.await(5, java.util.concurrent.TimeUnit.SECONDS) shouldBe true
      d.dispatch("Tosca", None, None, None, ResolveMode.Force)      // running → not upgraded
      d.dispatch("Tosca", None, None, None)                         // a plain duplicate: not counted
      release.countDown()
      d.stop()
      import scala.jdk.CollectionConverters._
      recorded.asScala.toSeq shouldBe Seq(ResolveMode.Force -> false)
    } finally { release.countDown(); if (!ec.isShutdown) ec.shutdown() }
  }

  it should "count a re-try onto a waiting resolve already at its mode or above as upgraded, as the queue does" in {
    val ec       = DaemonExecutors.boundedEC("inline-dispatch-dup-covered", 1)
    val release  = new CountDownLatch(1)
    val started  = new CountDownLatch(1)
    val recorded = new java.util.concurrent.ConcurrentLinkedQueue[(ResolveMode, Boolean)]()
    val d = new InlineResolveDispatcher(ec, keyOf, (title, _, _, _, _) =>
      if (title == "Busy") { started.countDown(); release.await(5, java.util.concurrent.TimeUnit.SECONDS) },
      duplicates = (mode, upgraded) => { recorded.add(mode -> upgraded); () })
    try {
      d.dispatch("Busy", None, None, None) // holds the one pool thread, so Tosca stays WAITING
      started.await(5, java.util.concurrent.TimeUnit.SECONDS) shouldBe true
      d.dispatch("Tosca", None, None, None, ResolveMode.RetryMiss)
      d.dispatch("Tosca", None, None, None, ResolveMode.RetryMiss) // already at RetryMiss
      d.dispatch("Tosca", None, None, None, ResolveMode.Force)
      d.dispatch("Tosca", None, None, None, ResolveMode.RetryMiss) // Force covers it
      release.countDown()
      d.stop()
      import scala.jdk.CollectionConverters._
      recorded.asScala.toSeq shouldBe Seq(ResolveMode.RetryMiss -> true, ResolveMode.Force -> true, ResolveMode.RetryMiss -> true)
    } finally { release.countDown(); if (!ec.isShutdown) ec.shutdown() }
  }

  it should "drain so an in-flight resolve completes on stop()" in {
    val ec        = DaemonExecutors.boundedEC("inline-dispatch-drain", 4)
    val completed = new AtomicInteger(0)
    val started   = new CountDownLatch(1)
    val d = new InlineResolveDispatcher(ec, keyOf, (_, _, _, _, _) => {
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
