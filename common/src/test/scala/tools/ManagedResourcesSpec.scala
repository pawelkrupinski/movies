package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ListBuffer

class ManagedResourcesSpec extends AnyFlatSpec with Matchers {

  "closeAll" should "close newest first, and close the rest when one fails" in {
    val closed  = ListBuffer.empty[String]
    val managed = new ManagedResources
    managed.register("a", "a")(closed += _)
    managed.register("b", "b")(_ => throw new IllegalStateException("b refuses"))
    managed.register("c", "c")(closed += _)
    managed.closeAll()
    closed.toList shouldBe List("c", "a")
    managed.open shouldBe 0
  }

  it should "shut a registered executor, so nothing it ran is left running" in {
    val managed = new ManagedResources
    val pool    = managed.executor("pool")(DaemonExecutors.scheduler("managed-spec"))
    pool.execute(() => ())
    managed.closeAll()
    pool.isTerminated shouldBe true
    managed.unterminated shouldBe empty
  }

  // `shutdownNow` hands back the tasks it pulled off the queue unrun; dropping that list left each
  // one's `Future` forever incomplete, and a thread waiting on it (an `invokeAll`) waiting forever.
  it should "cancel the tasks a closed executor never ran, releasing whoever waits on them" in {
    val managed = new ManagedResources
    val pool    = managed.executor("queued")(DaemonExecutors.boundedPool("managed-queued", threads = 1, queueCapacity = 4, WhenFull.RunOnCaller))
    val running = new java.util.concurrent.CountDownLatch(1)
    val never   = new java.util.concurrent.CountDownLatch(1)  // released only by closeAll's interrupt
    pool.execute(() => { running.countDown(); try never.await() catch { case _: InterruptedException => () } })
    running.await(5, java.util.concurrent.TimeUnit.SECONDS) shouldBe true
    val queued = pool.submit(() => 1)
    managed.closeAll()
    queued.isCancelled shouldBe true
  }

  // The pod's stop budget is fixed (web: 30 s grace less a 15 s preStop sleep), so the grace is one
  // budget for the whole stop, not one per pool: N pools whose tasks ignore their interrupt used to
  // hold the stop N x grace, past the kubelet's SIGKILL and before Mongo was ever closed.
  it should "spend one grace across every stuck executor, not one each" in {
    @volatile var release = false
    val grace   = scala.concurrent.duration.Duration(400, "millis")
    val managed = new ManagedResources(grace)
    (1 to 3).foreach { n =>
      val pool = managed.executor(s"stuck-$n")(java.util.concurrent.Executors.newSingleThreadExecutor())
      pool.execute(() => while (!release) Thread.onSpinWait()) // deaf to the interrupt
    }
    val stopwatch = Stopwatch.System.start()
    try managed.closeAll() finally release = true
    stopwatch.elapsed should be < (grace * 2)
  }

  it should "hand a stopping service what is left of the grace, to bound its own drain" in {
    @volatile var handed = Option.empty[scala.concurrent.duration.FiniteDuration]
    val grace   = scala.concurrent.duration.Duration(3, "seconds")
    val managed = new ManagedResources(grace)
    managed.stopping(new services.Stoppable {
      def stop(): Unit = ()
      override def stopWithin(budget: scala.concurrent.duration.FiniteDuration): Unit = handed = Some(budget)
    })
    managed.closeAll()
    handed.exists(budget => budget > scala.concurrent.duration.Duration.Zero && budget <= grace) shouldBe true
  }

  "A resource registered once the stop has begun" should "be closed at once" in {
    val closed  = ListBuffer.empty[String]
    val managed = new ManagedResources
    managed.closeAll()
    managed.register("late", "late")(closed += _)
    closed.toList shouldBe List("late")
  }
}
