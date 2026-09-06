package integration

import org.mongodb.scala.model.{CreateCollectionOptions, Filters, ValidationOptions}
import org.mongodb.scala.{Document, MongoCollection, ObservableFuture, SingleObservableFuture}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.UptimeMonitor
import tools.{Env, Eventually}

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
class UptimeTagWriteRollbackIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val db = tools.IsolatedMongoDatabase.open(Env.get("MONGODB_URI").get, "uptime-tag-rollback-spec")

  // Reject everything the monitor writes: it sets `service` and `tags`, never this field.
  Await.result(
    db.createCollection(
      "uptimeServiceTags",
      CreateCollectionOptions().validationOptions(ValidationOptions().validator(Filters.exists("__no_tag_write_may_satisfy_this__")))
    ).toFuture(),
    30.seconds
  )

  private val tagCollection: MongoCollection[Document] = db.getCollection("uptimeServiceTags")
  private val monitor  = new UptimeMonitor(Some(db))
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
    finally tools.IsolatedMongoDatabase.drop(db)
    super.afterAll()
  }

  "tagService" should "forget a tag whose write Mongo rejected, so the next call retries it" in {
    monitor.tagService(service, Set("custom:RejectedClient")) shouldBe true

    // The write really was refused — nothing to reconcile against.
    Await.result(tagCollection.countDocuments(Filters.eq("service", service)).toFuture(), 30.seconds) shouldBe 0L

    // The rejection arrives asynchronously; once it does, the in-memory claim is gone.
    Eventually.eventually(monitor.serviceTagsSnapshot().keySet should not contain service)

    // Which is the point: the same tags are attempted again instead of being skipped.
    monitor.tagService(service, Set("custom:RejectedClient")) shouldBe true
  }
}
