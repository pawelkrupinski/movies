package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.schedule.MongoScheduledRunStore
import services.tasks.{MongoTaskQueue, TaskType}
import tools.ConcurrentInstances
import tools.ConcurrentInstances.{race, rounds, successes}

import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.locks.LockSupport
import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

/**
 * Two workers claiming from ONE task queue — each its own Mongo client, each with several claim
 * threads, the way `TaskWorker` runs its pool — with a lease reaper sweeping at the same time.
 * The queue is the only thing standing between two claimers and one task, so what it must hold
 * is exactly: a task runs once, never twice at once, and none is lost.
 */
class TaskClaimsAcrossWorkersIntegrationSpec extends AnyFlatSpec with Matchers {

  FoldFixture.requireThrowawayMongo()

  private val Now   = Instant.parse("2026-06-01T10:00:00Z")
  private val Lease = 5.minutes

  "two workers draining one queue while a reaper sweeps" should "run every task exactly once, never two at once" in
    ConcurrentInstances.withInstances(tools.IntegrationMongoTarget.from(_root_.settings.ProcessConfiguration.resolve()).get, "task-claims-two-workers") { instances =>
      val queues = instances.map(instance => new MongoTaskQueue(Some(instance.database)))
      rounds(4, tools.ConcurrentInstances.baseSeed(_root_.settings.ProcessConfiguration.resolve())) { round =>
        val keys = (1 to 30).map(i => s"scrape|round-${round.number}-$i")
        keys.foreach(key => queues(round.random.nextInt(2)).enqueue(TaskType.ScrapeCinema, key, submittedAt = Now))

        val running  = ConcurrentHashMap.newKeySet[String]()
        val overlaps = ConcurrentHashMap.newKeySet[String]()
        val runs     = new ConcurrentHashMap[String, AtomicInteger]()
        val work     = (0 until 64).map(_ => round.random.nextInt(300_000).toLong)   // seeded run times, ns
        def drain(queue: MongoTaskQueue, workerId: String): Unit =
          Iterator.continually(queue.claim(workerId, Lease, Now)).takeWhile(_.isDefined).flatten.foreach { task =>
            if (!running.add(task.id)) overlaps.add(task.dedupKey)
            runs.computeIfAbsent(task.dedupKey, _ => new AtomicInteger()).incrementAndGet()
            LockSupport.parkNanos(work(math.abs(task.dedupKey.hashCode) % work.size))
            running.remove(task.id)
            queue.complete(task.id, workerId)
          }
        // Leases run to Now + 5min; a sweep at Now + 1min must re-queue nothing that is being worked.
        val reaper = () => { (1 to 20).foreach(_ => queues(0).reapExpiredLeases(Now.plusSeconds(60))); () }
        val claimers = for { (queue, w) <- queues.zipWithIndex; thread <- 1 to 3 } yield () => drain(queue, s"worker-$w-$thread")
        successes(race(claimers :+ reaper, Some(round), joinTimeout = 60.seconds))

        withClue("a task two claimers held at once: ") { overlaps.asScala shouldBe empty }
        val ranTwice = runs.asScala.collect { case (key, n) if n.get != 1 => key -> n.get }
        withClue("a task run more than once: ") { ranTwice shouldBe empty }
        withClue("a task never run: ") { keys.filterNot(runs.containsKey) shouldBe empty }
        queues(1).countByState() shouldBe empty
      }
    }

  // A worker whose lease expired mid-run (a GC pause, a slow upstream) finishes after the reaper
  // handed its task to the other worker. Its late `complete` must not delete the task out from
  // under the worker now running it — that task would be lost if the second run then failed.
  "a late complete from a worker whose lease was reaped" should "not remove the task the other worker now holds" in
    ConcurrentInstances.withInstances(tools.IntegrationMongoTarget.from(_root_.settings.ProcessConfiguration.resolve()).get, "task-claims-reaped-lease") { instances =>
      val Seq(first, second) = instances.map(instance => new MongoTaskQueue(Some(instance.database)))
      first.enqueue(TaskType.ScrapeCinema, "scrape|reaped", submittedAt = Now)
      val stalled = first.claim("worker-1", 1.minute, Now).get
      second.reapExpiredLeases(Now.plusSeconds(120)) shouldBe 1
      val reclaimed = second.claim("worker-2", 1.minute, Now.plusSeconds(120)).get
      reclaimed.id shouldBe stalled.id

      first.complete(stalled.id, "worker-1")
      second.countByState().values.sum shouldBe 1L
      second.complete(reclaimed.id, "worker-2")
      second.countByState() shouldBe empty
    }

  // Every recurring sweep (the reapers, the backfills) runs on ONE machine per occurrence: each
  // worker claims the occurrence id, and only the first insert wins (7b83a67d8).
  "two workers claiming the same scheduled occurrences at once" should "each run a disjoint half, together all of them" in
    ConcurrentInstances.withInstances(tools.IntegrationMongoTarget.from(_root_.settings.ProcessConfiguration.resolve()).get, "scheduled-runs-two-workers") { instances =>
      val stores = instances.map(instance => new MongoScheduledRunStore(instance.database.getCollection("scheduledRuns")))
      rounds(4, tools.ConcurrentInstances.baseSeed(_root_.settings.ProcessConfiguration.resolve())) { round =>
        val occurrences = (1 to 25).map(i => s"sweep|round-${round.number}|$i")
        val claimed = successes(race(stores.map(store => () => occurrences.filter(store.claim)), Some(round)))
        claimed.flatten.sorted shouldBe occurrences.sorted
      }
    }
}
