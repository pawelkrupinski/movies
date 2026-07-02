package integration

import org.mongodb.scala.{MongoClient, SingleObservableFuture}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.MovieCodecs
import services.tasks.{EnqueueResult, MongoTaskQueue, TaskType}
import tools.Env

import java.time.Instant
import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Live test of `MongoTaskQueue` against real MongoDB. Requires MONGODB_URI;
 * skips otherwise so CI without secrets keeps passing. Uses a sentinel
 * collection so it never touches the production `tasks` collection, dropped in
 * `afterAll`. Exercises the actual Mongo upsert-dedup, findOneAndUpdate claim,
 * ownership-guarded complete, and lease reaping — the paths the in-memory fake
 * can't prove.
 */
class MongoTaskQueueIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")

  private val client = MongoClient(Env.get("MONGODB_URI").get)
  private val db = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    .withCodecRegistry(MovieCodecs.registry)
  private val collName = "__integration_test_tasks"
  private val queue = new MongoTaskQueue(Some(db), collName)

  override protected def afterAll(): Unit = try {
    Await.ready(db.getCollection(collName).drop().toFuture(), 10.seconds)
    client.close()
  } finally super.afterAll()

  private val t0 = Instant.parse("2026-06-07T12:00:00Z")

  "MongoTaskQueue" should "use a relaxed {w:1, j:false} write concern to keep journal-fsync waits off the shared Mongo" in {
    queue.collectionWriteConcern shouldBe Some(com.mongodb.WriteConcern.W1.withJournal(false))
  }

  it should "add a task then dedup a second with the same key while active" in {
    val key = s"scrape|it-dedup-${System.nanoTime()}"
    queue.enqueue(TaskType.ScrapeCinema, key, submittedAt = t0) shouldBe EnqueueResult.Added
    queue.enqueue(TaskType.ScrapeCinema, key, submittedAt = t0) shouldBe EnqueueResult.Duplicate
  }

  it should "claim the task once, carry its payload, and not hand it out twice" in {
    val key = s"imdb|it-claim-${System.nanoTime()}"
    queue.enqueue(TaskType.ImdbRating, key, Map("title" -> "Dune"), submittedAt = t0)
    val claimed = drainUntil(_.dedupKey == key, "w1") // now worked_on, owned by w1
    claimed.payload shouldBe Map("title" -> "Dune")
    // It's leased now, so a further claim can never hand the same task back out.
    queue.claim("w2", 5.minutes).foreach(t => t.dedupKey should not be key)
  }

  it should "remove on complete only for the holder, and allow re-enqueue after" in {
    val key = s"scrape|it-complete-${System.nanoTime()}"
    queue.enqueue(TaskType.ScrapeCinema, key, submittedAt = t0)
    val task = drainUntil(_.dedupKey == key, "w1")
    queue.complete(task.id, "intruder") // wrong owner — no-op
    queue.enqueue(TaskType.ScrapeCinema, key, submittedAt = t0) shouldBe EnqueueResult.Duplicate
    queue.complete(task.id, "w1") // real owner — removes the task outright
    queue.enqueue(TaskType.ScrapeCinema, key, submittedAt = t0) shouldBe EnqueueResult.Added
  }

  it should "time out a task stuck in processing past its lease" in {
    val key = s"detail|it-reap-${System.nanoTime()}"
    queue.enqueue(TaskType.EnrichDetails, key, submittedAt = t0)
    drainUntil(_.dedupKey == key, "w1", lease = 1.millis)
    Thread.sleep(10)
    queue.reapExpiredLeases(Instant.now()) should be >= 1
    // Back to waiting → claimable again.
    drainUntil(_.dedupKey == key, "w2").dedupKey shouldBe key
  }

  it should "expose a monitor snapshot listing live active tasks with their state" in {
    val workKey = s"imdb|it-mon-work-${System.nanoTime()}"
    queue.enqueue(TaskType.ImdbRating, workKey, Map("title" -> "Heat"), submittedAt = t0)
    val worked = drainUntil(_.dedupKey == workKey, "mon-w1") // workKey → worked_on, owned by mon-w1
    // Enqueue the waiting task only AFTER draining, with the newest submittedAt,
    // so `drainUntil` (which leases every task it passes) can never claim it.
    val waitKey = s"scrape|it-mon-wait-${System.nanoTime()}"
    queue.enqueue(TaskType.ScrapeCinema, waitKey, submittedAt = Instant.now())

    val snap = queue.monitor(500)
    val mine = snap.active.filter(t => t.dedupKey == waitKey || t.dedupKey == workKey)
    mine.map(_.dedupKey).toSet shouldBe Set(waitKey, workKey)

    val waiting = mine.find(_.dedupKey == waitKey).get
    waiting.state shouldBe services.tasks.TaskState.Waiting
    waiting.taskType shouldBe "ScrapeCinema"
    waiting.workerId shouldBe None

    val workedSummary = mine.find(_.dedupKey == workKey).get
    workedSummary.state shouldBe services.tasks.TaskState.WorkedOn
    workedSummary.workerId shouldBe Some("mon-w1")
    workedSummary.leaseExpiresAt should be (defined)
    workedSummary.id shouldBe worked.id

    // A completed task is removed — gone from the active listing.
    queue.complete(worked.id, "mon-w1")
    queue.monitor(500).active.map(_.dedupKey) should not contain workKey
  }

  // Claim repeatedly until the task matching `p` is handed out (other tests'
  // leftovers may be claimed first; harmless — they just lease and stay).
  private def drainUntil(p: services.tasks.Task => Boolean, worker: String, lease: FiniteDuration = 5.minutes) = {
    var found: Option[services.tasks.Task] = None
    var tries = 0
    while (found.isEmpty && tries < 50) {
      queue.claim(worker, lease) match {
        case Some(t) if p(t) => found = Some(t)
        case Some(_)         => () // someone else's task; leave it leased
        case None            => tries = 50
      }
      tries += 1
    }
    found.getOrElse(fail(s"task matching predicate never appeared"))
  }
}
