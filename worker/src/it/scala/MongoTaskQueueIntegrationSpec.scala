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
  // Never against a real cluster: these specs write + purge sentinels, and
  // `.env.local` aims MONGODB_URI at the prod tunnel. See `IntegrationMongo`.
  tools.IntegrationMongo.requireThrowaway()

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

  // `countByState` is a single `$group` rather than a `countDocuments` per state,
  // because it runs on the 60s heartbeat AND every /debug request and metrics
  // scrape. Only real Mongo can prove the pipeline shape — the in-memory queue
  // never executes it. The absent-when-zero contract matters: callers (and
  // several unit specs) rely on a state with no rows being MISSING, not 0.
  it should "count both states in one pass" in {
    // Order matters: `drainUntil` claims tasks until it finds its target, so
    // anything enqueued BEFORE it would be dragged to worked_on too. Enqueue the
    // one to claim first, drain it, and only then add the row that must stay
    // waiting.
    val worked = s"scrape|it-count-worked-${System.nanoTime()}"
    queue.enqueue(TaskType.ScrapeCinema, worked, submittedAt = t0)
    val claimed = drainUntil(_.dedupKey == worked, "counter")
    queue.enqueue(TaskType.ScrapeCinema, s"scrape|it-count-waiting-${System.nanoTime()}", submittedAt = t0)

    val counts = queue.countByState()
    counts.getOrElse(services.tasks.TaskState.Waiting, 0L) should be >= 1L
    counts.getOrElse(services.tasks.TaskState.WorkedOn, 0L) should be >= 1L

    queue.complete(claimed.id, "counter")
  }

  // The `$group` rewrite buckets whatever `state` value it finds, where the old
  // per-state `countDocuments` could only ever answer about the states we named.
  // Without the pipeline's `$match` this leaks an unrecognised state into the map,
  // and /debug + the metrics export render it as a real queue state. Fails on a
  // `$group` with no `$match`; passes on both the old and the guarded new version.
  it should "never report a state outside the two the queue defines" in {
    val doc = org.mongodb.scala.Document(
      "_id" -> s"it-bogus-${System.nanoTime()}",
      "dedupKey" -> s"bogus|${System.nanoTime()}",
      "taskType" -> TaskType.ScrapeCinema.name,
      "state" -> "not_a_real_state",
      "active" -> false)
    Await.result(db.getCollection(collName).insertOne(doc).toFuture(), 10.seconds)

    queue.countByState().keySet should contain theSameElementsAs
      queue.countByState().keySet.intersect(services.tasks.TaskState.all.toSet)
    queue.countByState() should not contain key ("not_a_real_state")
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

  // The `notBefore` enqueue path — `setOnInsert("nextEligibleAt")` gated by the same
  // `claim` filter the release-backoff uses. Only real Mongo proves the on-insert
  // write + the eligibility query agree; it's what the chunk-scrape spread relies on.
  it should "hold a task back from claim until its notBefore, then hand it out" in {
    val key        = s"chunk|it-notbefore-${System.nanoTime()}"
    val eligibleAt = Instant.now().plusSeconds(3600) // an hour out, so "now" is before it
    queue.enqueue(TaskType.ScrapeChunk, key, submittedAt = t0, notBefore = Some(eligibleAt)) shouldBe EnqueueResult.Added
    // Before its window opens the queue never hands this task out, even though it's
    // the oldest by submittedAt — the nextEligibleAt gate excludes it.
    claimableKeysAt(eligibleAt.minusSeconds(60)) should not contain key
    // Once `now` reaches notBefore it becomes claimable.
    claimableKeysAt(eligibleAt) should contain (key)
  }

  /** Every dedupKey the queue will hand out at `now` (drains the eligible set,
   *  leasing each — harmless, the sentinel collection is dropped in afterAll). */
  private def claimableKeysAt(now: Instant): Set[String] = {
    val keys = scala.collection.mutable.Set.empty[String]
    var next = queue.claim("nb-probe", 5.minutes, now)
    while (next.isDefined) { keys += next.get.dedupKey; next = queue.claim("nb-probe", 5.minutes, now) }
    keys.toSet
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
