package integration

import org.mongodb.scala.model.{CreateCollectionOptions, Filters, ValidationOptions}
import org.mongodb.scala.{Document, MongoCollection, SingleObservableFuture}
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

  override protected def afterAll(): Unit = {
    monitor.close()
    tools.IsolatedMongoDatabase.drop(db)
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
