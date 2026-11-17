package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.util.concurrent.{CompletableFuture, TimeUnit}
import scala.concurrent.Future

class ThreadPoolsSpec extends AnyFlatSpec with Matchers {

  private def threadNameOn(run: (Runnable => Unit)): String = {
    val name = new CompletableFuture[String]()
    run(() => { name.complete(Thread.currentThread().getName); () })
    name.get(SpecTimeouts.Io.toMillis, TimeUnit.MILLISECONDS)
  }

  "managed pools" should "be registered with the root's resources as they are built, so its closeAll shuts them" in {
    val resources = new ManagedResources(new Stopwatch(() => 0L))
    val pools     = ThreadPools.managedBy(resources)
    val scheduler = pools.scheduler("spec-scheduler")
    val virtual   = pools.virtualThreadEC("spec-virtual")
    val ordered   = pools.singleThreadExecutor("spec-ordered", queueCapacity = 4)
    Seq(scheduler, virtual, ordered).foreach(pool => resources.holds(pool) shouldBe true)
    resources.closeAll()
    Seq(scheduler, virtual, ordered).foreach(pool => pool.isShutdown shouldBe true)
    resources.unterminated shouldBe empty
  }

  they should "keep the names and sizes the services gave their own pools" in {
    val resources = new ManagedResources(new Stopwatch(() => 0L))
    val pools     = ThreadPools.managedBy(resources)
    try {
      threadNameOn(pools.scheduler("detail-reaper").execute(_)) shouldBe "detail-reaper"
      threadNameOn(pools.singleThreadExecutor("movie-change-apply", queueCapacity = 4).execute(_)) shouldBe "movie-change-apply"
      threadNameOn(r => { Future(r.run())(using pools.virtualThreadEC("bulk-refresh")); () }) should startWith("bulk-refresh-")
      pools.scheduler("two-threads", threads = 2) match {
        case pool: java.util.concurrent.ScheduledThreadPoolExecutor => pool.getCorePoolSize shouldBe 2
        case other                                                  => fail(s"not a scheduled pool: $other")
      }
    } finally resources.closeAll()
  }

  "unmanaged pools" should "be left to whoever asked for them" in {
    val resources = new ManagedResources(new Stopwatch(() => 0L))
    val scheduler = ThreadPools.Unmanaged.scheduler("spec-unmanaged")
    try {
      resources.holds(scheduler) shouldBe false
      resources.closeAll()
      scheduler.isShutdown shouldBe false
    } finally { scheduler.shutdownNow(); () }
  }

  "a spec's pools" should "hand every scheduler asked for to the one stepped scheduler" in {
    val stepped = new ManualScheduler(new MutableClock(java.time.Instant.EPOCH))
    val pools   = SpecThreadPools.scheduling(stepped)
    pools.scheduler("a") shouldBe theSameInstanceAs(stepped)
    pools.scheduler("b", threads = 3) shouldBe theSameInstanceAs(stepped)
  }
}
