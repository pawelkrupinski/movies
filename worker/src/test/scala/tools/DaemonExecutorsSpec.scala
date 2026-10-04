package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayOutputStream, PrintStream}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.concurrent.Promise
import scala.concurrent.duration.DurationInt

class DaemonExecutorsSpec extends AnyFlatSpec with Matchers {

  // `Executors.newFixedThreadPool` queued without bound: a producer outrunning the pool grew the
  // queue until the heap was gone. A full bounded pool slows its producer instead.
  "boundedPool" should "run a task on the submitting thread once its queue is full" in {
    val pool    = DaemonExecutors.boundedPool("bounded-pool", threads = 1, queueCapacity = 1, WhenFull.RunOnCaller)
    val release = new CountDownLatch(1)
    try {
      pool.execute(() => release.await())          // occupies the one thread
      pool.execute(() => ())                       // fills the queue
      var ranOn: Thread = null
      pool.execute(() => ranOn = Thread.currentThread())
      ranOn shouldBe Thread.currentThread()
    } finally { release.countDown(); pool.shutdownNow() }
  }

  it should "make the submitter wait for room, keeping order, when asked to" in {
    val pool    = DaemonExecutors.singleThreadExecutor("ordered-pool", queueCapacity = 1)
    val order   = new java.util.concurrent.ConcurrentLinkedQueue[Int]()
    val release = new CountDownLatch(1)
    try {
      pool.execute(() => { release.await(); order.add(0); () })
      pool.execute(() => { order.add(1); () })
      val third = new Thread(() => pool.execute(() => { order.add(2); () }))
      third.start()
      // The third submit waits for room rather than running on its thread.
      third.join(SpecTimeouts.quiet(200.millis).toMillis)
      third.isAlive shouldBe true
      release.countDown()
      third.join(SpecTimeouts.Io.toMillis)
      pool.shutdown(); pool.awaitTermination(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe true
      order.toArray.toSeq shouldBe Seq(0, 1, 2)
    } finally { release.countDown(); pool.shutdownNow() }
  }

  // A submission a shut-down pool will never run used to be DROPPED: `submit(...).get()` and
  // `invokeAll` (the identity prefetch's) then waited on a future no one would ever complete. It
  // is cancelled instead, so whoever waits on it is released at once.
  "a submission to a shut-down pool" should "come back cancelled from a caller-runs pool, not hang its waiter" in {
    val pool = DaemonExecutors.boundedPool("shut-caller-runs", threads = 1, queueCapacity = 1, WhenFull.RunOnCaller)
    pool.shutdown()
    val task = pool.submit(() => 1)
    an[java.util.concurrent.CancellationException] should be thrownBy task.get(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS)
  }

  it should "come back cancelled from a wait-for-room pool" in {
    val pool = DaemonExecutors.singleThreadExecutor("shut-wait-for-room", queueCapacity = 1)
    pool.shutdown()
    val task = pool.submit(() => 1)
    an[java.util.concurrent.CancellationException] should be thrownBy task.get(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS)
  }

  it should "come back cancelled from a virtual-thread EC" in {
    val pool = DaemonExecutors.virtualThreadEC("shut-virtual")
    pool.shutdown()
    val task = pool.submit(() => 1)
    an[java.util.concurrent.CancellationException] should be thrownBy task.get(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS)
  }

  it should "release an invokeAll caller rather than leave it waiting forever" in {
    val pool = DaemonExecutors.boundedPool("shut-invoke-all", threads = 1, queueCapacity = 1, WhenFull.RunOnCaller)
    pool.shutdown()
    val tasks = java.util.List.of[java.util.concurrent.Callable[Int]](() => 1, () => 2)
    val done  = new CountDownLatch(1)
    val caller = new Thread(() => { pool.invokeAll(tasks); done.countDown() })
    caller.setDaemon(true)
    caller.start()
    try done.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe true finally caller.interrupt()
  }

  // A producer waiting for room got it from `shutdownNow` draining the queue — after the pool had
  // stopped taking from it — and put its task where nothing would ever run it, its future forever
  // incomplete. A task that lands in the queue of a pool shut down meanwhile is taken back, cancelled.
  it should "cancel a task whose producer got room only from the pool shutting down" in {
    val pool    = DaemonExecutors.singleThreadExecutor("stranded-wait-for-room", queueCapacity = 1)
    val release = new CountDownLatch(1)
    pool.execute(() => try release.await() catch { case _: InterruptedException => () })
    pool.execute(() => ())                                       // fills the queue
    val submitted = new java.util.concurrent.atomic.AtomicReference[java.util.concurrent.Future[Int]]()
    val producer  = new Thread(() => submitted.set(pool.submit(() => 1)))
    producer.setDaemon(true)
    producer.start()
    Eventually.eventually(producer.getState shouldBe Thread.State.WAITING, pollMs = 5)
    pool.shutdownNow()
    producer.join(SpecTimeouts.Io.toMillis)
    an[java.util.concurrent.CancellationException] should be thrownBy submitted.get.get(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS)
  }

  // A task parked on a permit gate when its executor is shut down NOW is interrupted before it ever
  // ran: its future stayed incomplete, and whoever waited on it waited forever. Cancelled instead —
  // also through two gates, as a sub-limited EC stacks them, where the inner gate holds the outer
  // gate's wrapper. Gates over a pool the spec controls, so the task is seen parked before the stop.
  it should "be cancelled when interrupted while waiting for a permit, through one gate or two" in {
    def parkedThenStopped(gates: Int): java.util.concurrent.Future[Int] = {
      val pool       = java.util.concurrent.Executors.newSingleThreadExecutor()
      val inner      = new java.util.concurrent.Semaphore(0)
      val innerGated = DaemonExecutors.semaphoreGated(pool, inner)
      val gated      = if (gates == 1) innerGated else DaemonExecutors.semaphoreGated(innerGated, new java.util.concurrent.Semaphore(1))
      val parked     = DaemonExecutors.dropRejectedAfterShutdown(gated).submit(() => 1)
      Eventually.eventually(inner.hasQueuedThreads shouldBe true, pollMs = 5)
      pool.shutdownNow()
      parked
    }
    an[java.util.concurrent.CancellationException] should be thrownBy parkedThenStopped(gates = 1).get(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS)
    an[java.util.concurrent.CancellationException] should be thrownBy parkedThenStopped(gates = 2).get(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS)
  }

  "boundedEC" should "cap concurrency for a single EC" in {
    val peak = ExecutorProbes.peakConcurrency(10, IndexedSeq(DaemonExecutors.boundedEC("bounded", 3)))
    peak should be <= 3
  }

  // The shutdown race in `Wiring.stop`: a `Future` stage is mid-run on the pool
  // when the pool is shut down; as that stage completes it notifies a terminal
  // callback whose submit lands on the now-shut-down pool. A terminal callback
  // has no downstream promise to absorb the failure into, so the
  // RejectedExecutionException is handed to the EC's failure reporter, which
  // prints a stack trace — one per dangling continuation, the storm in the bug
  // report (in prod the reporter routes through logback, hence the timestamps).
  // `dropRejectedAfterShutdown` (wired into every DaemonExecutors EC) must drop
  // the submit at its source so nothing is ever reported: stderr stays clean.
  "an EC handed out by DaemonExecutors" should "not print a RejectedExecutionException when a Future stage completes after shutdown" in {
    val executionContext      = DaemonExecutors.boundedEC("storm", 1)
    val running = new CountDownLatch(1)
    val release = new CountDownLatch(1)
    val worker  = new java.util.concurrent.atomic.AtomicReference[Thread]()
    val printed = captureStdErr {
      val p      = Promise[Int]()
      val stage1 = p.future.map { x => worker.set(Thread.currentThread()); running.countDown(); release.await(); x }(using executionContext)
      stage1.onComplete(_ => ())(using executionContext) // terminal: submit fires from stage1's run(), no
                                           // downstream to capture the failure → it's reported
      p.success(1)                         // schedules stage1 onto executionContext
      running.await(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS) shouldBe true
      executionContext.shutdown()                        // pool drained while stage1 is still running
      release.countDown()                  // stage1 finishes → notifies the callback → submit rejected
      // The rejected submit happens on stage1's own thread, and an uncaught rejection is
      // reported before that thread ends: once it has ended, anything it would print has.
      worker.get.join(java.time.Duration.ofSeconds(5)) shouldBe true
    }
    printed should not include "RejectedExecutionException"
  }

  /** Run `body` with `System.err` redirected to a buffer, restore it, and
   *  return everything written. Used to assert the shutdown-race storm no
   *  longer reaches stderr. */
  private def captureStdErr(body: => Any): String = {
    val captured = new ByteArrayOutputStream()
    val original = System.err
    System.setErr(new PrintStream(captured, true))
    try body
    finally System.setErr(original)
    captured.toString
  }
}
