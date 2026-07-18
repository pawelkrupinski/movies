package integration

import org.mongodb.scala.{MongoClient, ObservableFuture, SingleObservableFuture}
import org.mongodb.scala.bson.collection.immutable.Document
import org.scalatest.BeforeAndAfterAll
import org.scalatest.concurrent.Eventually
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Millis, Seconds, Span}
import services.schedule.{MongoScheduledRunStore, ScheduledRunStore}
import tools.Env

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Live test of `MongoScheduledRunStore` against real MongoDB. Requires
 * MONGODB_URI; skips otherwise so CI without secrets keeps passing.
 *
 * Uses a sentinel collection (`__integration_test_scheduled_runs`) so it never
 * touches the production `scheduled_runs` collection; dropped in `afterAll`.
 */
class ScheduledRunStoreIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with Eventually {

  assume(Env.get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  // Never against a real cluster: these specs write + purge sentinels, and
  // `.env.local` aims MONGODB_URI at the prod tunnel. See `IntegrationMongo`.
  tools.IntegrationMongo.requireThrowaway()

  private val client = MongoClient(Env.get("MONGODB_URI").get)
  private val coll   = client.getDatabase(Env.get("MONGODB_DB").getOrElse("kinowo"))
    .getCollection[Document]("__integration_test_scheduled_runs")

  private val store: ScheduledRunStore = new MongoScheduledRunStore(coll)

  override protected def afterAll(): Unit = try {
    Await.ready(coll.drop().toFuture(), 10.seconds)
    client.close()
  } finally super.afterAll()

  "MongoScheduledRunStore.claim" should "grant a never-before-seen occurrence exactly once" in {
    val id = "it-claim-once-1"
    store.claim(id) shouldBe true
    store.claim(id) shouldBe false // same occurrence id → loser
  }

  it should "grant distinct occurrence ids independently" in {
    store.claim("it-distinct-a") shouldBe true
    store.claim("it-distinct-b") shouldBe true
  }

  it should "create a 48h TTL index on claimedAt so old claims self-expire" in {
    // The index is built fire-and-forget on a daemon thread at construction.
    eventually(timeout(Span(10, Seconds)), interval(Span(200, Millis))) {
      val indexes    = Await.result(coll.listIndexes().toFuture(), 5.seconds)
      val ttlIndex   = indexes.find(_.get("key").exists(_.asDocument().containsKey("claimedAt")))
      val ttlSeconds = ttlIndex.flatMap(_.get("expireAfterSeconds")).map(_.asNumber().longValue())
      ttlSeconds.shouldBe(Some(48L * 3600L))
    }
  }
}
