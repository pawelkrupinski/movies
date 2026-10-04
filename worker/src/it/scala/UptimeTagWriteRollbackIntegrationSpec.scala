package integration

import org.mongodb.scala.model.{CreateCollectionOptions, Filters, ValidationOptions}
import com.mongodb.{ConnectionString, MongoClientSettings}
import org.mongodb.scala.{Document, MongoClient, MongoCollection, ObservableFuture, SingleObservableFuture}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.{ServiceTags, UptimeMonitor}
import tools.Eventually

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * A tag write that Mongo REFUSES must not be recorded in memory as if it had landed.
 *
 * `tagService` skips the write when the tags it is handed match what it already holds in
 * memory — the optimisation that took 35,882 no-op tag updates out of a two-day window. It
 * updates that map optimistically, before the write, so a rejected write would otherwise
 * leave the map claiming a value the collection never received: every later call would see
 * "unchanged", skip, and never retry, and Mongo would keep the stale tags until the process
 * restarted. The unconditional write this guard replaced could not go stale that way, so the
 * rollback is what keeps the optimisation honest.
 *
 * The failure is induced structurally rather than by breaking the connection: the collection
 * is created up front with a validator no document `tagService` writes can satisfy, so every
 * upsert is rejected by the server while the client, the database and the monitor's own
 * background threads all stay healthy.
 *
 * Requires MONGODB_URI; skips otherwise.
 */
class UptimeTagWriteRollbackIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private val isolatedDb = tools.IsolatedMongoDatabase.open(mongoTarget, "uptime-tag-rollback-spec")

  private val db = isolatedDb.database
  // Reject everything the monitor writes: it sets `service` and `tags`, never this field.
  Await.result(
    db.createCollection(
      "uptimeServiceTags",
      CreateCollectionOptions().validationOptions(ValidationOptions().validator(Filters.exists("__no_tag_write_may_satisfy_this__")))
    ).toFuture(),
    30.seconds
  )

  private val tagCollection: MongoCollection[Document] = db.getCollection("uptimeServiceTags")
  private val monitor  = new UptimeMonitor(Some(db), clock = _root_.tools.MongoTtlSpecClock.Pinned)
  private val service  = "__uptime-tag-rollback-sentinel__"

  /**
   * DROP UNTIL IT STAYS DROPPED. `close()` stops the scheduler but cannot join the constructor's
   * daemon init thread (`ensureIndexes` → `hydrate` → `loadTags`) or a flush already in flight,
   * and either one RECREATES this database simply by touching a collection in it — after the
   * drop has run. A single drop therefore leaks, silently and on every run: eighteen of these
   * had piled up on the local instance before anyone counted them.
   *
   * The loop is the fix rather than a sleep because there is no handle to wait on and the
   * window depends on how long index creation takes on the day.
   */
  override protected def afterAll(): Unit = {
    monitor.close()
    // NOTHING HERE MAY THROW. A cleanup that fails must not replace the suite's own result with
    // its own, and must not skip `super.afterAll()` on the way out — an `afterAll` that throws
    // reports as a suite-level abort, which reads exactly like a real failure and hides one. The
    // per-call timeouts are well inside the loop's budget so a single hung drop cannot overrun it.
    try Eventually.eventually({
      Await.result(db.drop().toFuture(), 5.seconds)
      Await.result(db.listCollectionNames().toFuture(), 5.seconds) shouldBe empty
    }, timeoutMs = 20000, pollMs = 250)
    catch {
      case _: Throwable =>
        // Say so rather than leaving a stray database for someone to find by counting.
        info(s"could not confirm ${db.name} was dropped; sweep kinowo_isolated_* if it lingers")
    }
    finally isolatedDb.drop()
    super.afterAll()
  }

  /** How long the rejection may take to arrive. It is asynchronous and unbounded — the write
   *  has no deadline of its own — and a loaded shared mongod (the 2026-10-03 itAll, where single
   *  commands took 11-15 s) answered it well past the 2 s `eventually` gives by default. */
  private val RejectionBudgetMs = 60000L

  "tagService" should "forget a tag whose write Mongo rejected, so the next call retries it" in {
    monitor.tagService(service, Set("custom:RejectedClient")) shouldBe true

    // The rejection arrives asynchronously; once it does, the in-memory claim is gone.
    Eventually.eventually(monitor.serviceTagsSnapshot().keySet should not contain service, timeoutMs = RejectionBudgetMs)

    // The write really was refused — nothing to reconcile against.
    Await.result(tagCollection.countDocuments(Filters.eq("service", service)).toFuture(), 30.seconds) shouldBe 0L

    // Which is the point: the same tags are attempted again instead of being skipped.
    monitor.tagService(service, Set("custom:RejectedClient")) shouldBe true
  }

  it should "forget it however late the rejection arrives" in {
    // A client with ONE connection, held by a query that sleeps on the server: the tag write
    // queues behind it, so Mongo's refusal arrives seconds later — as a loaded server's did.
    val slow = MongoClient(MongoClientSettings.builder().applyConnectionString(new ConnectionString(mongoTarget.uri.value))
      .codecRegistry(MongoClient.DEFAULT_CODEC_REGISTRY)
      .applyToConnectionPoolSettings(pool => { pool.maxSize(1); () }).build())
    try {
      val slowDb = slow.getDatabase(db.name)
      // A document for the sleeping query to visit (the tag collection accepts none).
      Await.result(db.getCollection[Document]("held").insertOne(Document("_id" -> 1)).toFuture(), 30.seconds)
      val holding = slowDb.getCollection[Document]("held").find(Filters.where("sleep(5000) || true")).toFuture()
      // Wait until it is running server-side, i.e. holds the client's one connection.
      Eventually.eventually({
        val active = Await.result(isolatedDb.client.getDatabase("admin").aggregate[Document](Seq(
          Document("$currentOp" -> Document()), Document("$match" -> Document("ns" -> s"${db.name}.held")))).toFuture(), 10.seconds)
        active should not be empty
      }, timeoutMs = 5000)
      val tags = new ServiceTags(Some(slowDb.getCollection[Document]("uptimeServiceTags")))
      tags.tagService(service, Set("custom:RejectedClient")) shouldBe true

      Eventually.eventually(tags.snapshot().keySet should not contain service, timeoutMs = RejectionBudgetMs)
      tags.tagService(service, Set("custom:RejectedClient")) shouldBe true
      Await.result(holding, 60.seconds)
    } finally slow.close()
  }
}
