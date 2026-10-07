package services.contracts

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.metrics.{MeteredTaskQueue, WorkerTaskMetrics}
import services.movies.InMemoryMovieRepository
import services.tasks.{EnqueueResult, InMemoryTaskQueue, TaskQueue, TaskType}
import tools.contracts.Implementations
import tools.IsolatedMongoDatabase

import java.time.Instant
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.*

/**
 * ONE behaviour suite for [[TaskQueue]], run against every implementation on the class path
 * — the in-memory fake, the Mongo queue, and each decorator — found by reflection, so a new
 * implementation joins the suite the day it is written.
 *
 * THE DRIFT THIS PINS. The in-memory and Mongo queues once disagreed on amending a waiting task
 * with fields it already carries (69831df05), and every spec of the resolve re-try metric ran
 * against the in-memory one. Both now answer `true` — Mongo from the update's MATCHED count —
 * because that re-try's request still runs; only a missing or claimed task answers `false`.
 */
class TaskQueueContractSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private lazy val isolatedDatabase = IsolatedMongoDatabase.open(mongoTarget, "task-queue-contract")

  private lazy val database = isolatedDatabase.database
  override protected def afterAll(): Unit = try isolatedDatabase.drop() finally super.afterAll()

  private val collections = new AtomicInteger
  private val t0          = Instant.parse("2026-09-24T10:00:00Z")

  /** Every implementation, each test on a fresh instance: its own Mongo collection, its own
   *  in-memory delegate under a decorator. */
  private def fresh(cls: Class[? <: TaskQueue]): TaskQueue =
    Implementations.construct(cls, _.getTypeName match {
      case "scala.Option<org.mongodb.scala.MongoDatabase>" => Some(Some(database))
      case "java.lang.String"                              => Some(s"tasks_${collections.incrementAndGet()}")
      case "services.tasks.TaskQueue"                      => Some(new InMemoryTaskQueue)
      case "services.metrics.WorkerTaskMetrics"            =>
        Some(new WorkerTaskMetrics("pl", new WorkerTaskMetrics.Series(poolSize = 1, countryCodes = Seq("pl"))))
      case _ => None
    }).fold(missing => fail(missing), identity)

  private val implementations =
    Implementations.of(classOf[TaskQueue], classOf[TaskQueue], classOf[MeteredTaskQueue], classOf[InMemoryMovieRepository])

  "the TaskQueue implementations" should "include the in-memory fake and the Mongo queue" in {
    implementations.map(_.getSimpleName) should contain allOf ("InMemoryTaskQueue", "MongoTaskQueue")
  }

  implementations.foreach { cls =>
    val name = cls.getSimpleName

    it should s"[$name] add a task, then report a second enqueue of its key as a duplicate" in {
      val queue = fresh(cls)
      queue.enqueue(TaskType.ResolveImdbId, "film|2026", Map("title" -> "Film"), t0) shouldBe EnqueueResult.Added
      queue.enqueue(TaskType.ResolveImdbId, "film|2026", Map("title" -> "Film"), t0) shouldBe EnqueueResult.Duplicate
    }

    it should s"[$name] amend a waiting task's payload, and hand the amended payload to its claimer" in {
      val queue = fresh(cls)
      queue.enqueue(TaskType.ResolveImdbId, "film|2026", Map("title" -> "Film"), t0)
      queue.amendWaiting("film|2026", Map("retryMiss" -> "true")) shouldBe true
      queue.claim("worker", 1.minute, t0).map(_.payload) shouldBe Some(Map("title" -> "Film", "retryMiss" -> "true"))
    }

    it should s"[$name] report amended when the waiting task already carries the fields" in {
      val queue = fresh(cls)
      queue.enqueue(TaskType.ResolveImdbId, "film|2026", Map("title" -> "Film", "retryMiss" -> "true"), t0)
      queue.amendWaiting("film|2026", Map("retryMiss" -> "true")) shouldBe true
    }

    it should s"[$name] leave a claimed task's payload alone" in {
      val queue = fresh(cls)
      queue.enqueue(TaskType.ResolveImdbId, "film|2026", Map("title" -> "Film"), t0)
      val claimed = queue.claim("worker", 1.minute, t0)
      claimed.map(_.dedupKey) shouldBe Some("film|2026")
      queue.amendWaiting("film|2026", Map("force" -> "true")) shouldBe false
      queue.release(claimed.get.id, "worker")
      queue.claim("worker", 1.minute, t0).map(_.payload) shouldBe Some(Map("title" -> "Film"))
    }

    it should s"[$name] report nothing amended under a key with no task" in {
      fresh(cls).amendWaiting("absent|2026", Map("force" -> "true")) shouldBe false
    }

    // A held task's insert finds nothing to claim and its hold ending writes nothing, so the pool times
    // its wake from the hold the push hands it. A queue that drops the hold leaves a staggered chunk
    // waiting out the pool's 30 s idle backstop once claimable.
    it should s"[$name] hand a watcher each enqueued task's hold, or none for a task claimable at once" in {
      val queue = fresh(cls)
      val holds = new java.util.concurrent.CopyOnWriteArrayList[Option[Instant]]()
      queue.watchWaiting(hold => { holds.add(hold); () }).foreach { handle =>
        try {
          // A change stream opens asynchronously: enqueue until the first push arrives.
          tools.Eventually.awaitStreamLive("an unheld task's insert", !holds.isEmpty) { pass =>
            queue.enqueue(TaskType.ResolveImdbId, s"live|$pass", Map.empty, t0); ()
          }
          holds.get(0) shouldBe None
          holds.clear()
          val until = t0.plusSeconds(90)
          queue.enqueue(TaskType.ScrapeChunk, "chunk|held", Map.empty, t0, notBefore = Some(until))
          org.scalatest.concurrent.Eventually.eventually(org.scalatest.concurrent.PatienceConfiguration.Timeout(
            org.scalatest.time.Span(tools.SpecTimeouts.Settle.toMillis, org.scalatest.time.Millis))) {
            scala.jdk.CollectionConverters.ListHasAsScala(holds).asScala.toList should contain (Some(until))
          }
        } finally handle.close()
      }
    }

    it should s"[$name] free a key once its task completes" in {
      val queue = fresh(cls)
      queue.enqueue(TaskType.ResolveImdbId, "film|2026", Map.empty, t0)
      val task = queue.claim("worker", 1.minute, t0).get
      queue.complete(task.id, "worker")
      queue.enqueue(TaskType.ResolveImdbId, "film|2026", Map.empty, t0) shouldBe EnqueueResult.Added
    }
  }
}
