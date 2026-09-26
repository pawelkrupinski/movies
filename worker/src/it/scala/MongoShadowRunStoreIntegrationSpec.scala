package integration

import com.mongodb.{ConnectionString, MongoClientSettings}
import org.mongodb.scala.{MongoClient, MongoDatabase, SingleObservableFuture}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.TtlIndexMismatches
import services.identity.{MongoShadowRunBackend, ShadowRun, ShadowRunStore, ShadowRunStoreBehaviour}
import tools.{IntegrationCorpusDatabase, IntegrationMongoSuite, MutableClock}

import scala.concurrent.Await
import scala.concurrent.duration._

/** The shadow runs' rules over the Mongo backend — the cases `ShadowRunStoreSpec` runs in memory,
 *  against the real collections — and the split the wirings rely on: the worker's writer owns the
 *  indexes, and the web's reader sees what it wrote. */
class MongoShadowRunStoreIntegrationSpec extends AnyFlatSpec with Matchers with ShadowRunStoreBehaviour
    with BeforeAndAfterAll with IntegrationMongoSuite {

  private val client = MongoClient(MongoClientSettings.builder()
    .applyConnectionString(new ConnectionString(mongoTarget.uri.value))
    .codecRegistry(MongoClient.DEFAULT_CODEC_REGISTRY).build())
  private val database: MongoDatabase = client.getDatabase(IntegrationCorpusDatabase.named(mongoTarget, "shadow-runs"))
  private val mismatches = new TtlIndexMismatches

  private def fresh(): Unit =
    Seq(ShadowRunStore.DecisionsCollection, ShadowRunStore.DiffCollection)
      .foreach(c => Await.result(database.getCollection(c).drop().toFuture(), 30.seconds))

  "the Mongo shadow run store" should behave like shadowRunStore { clock =>
    fresh()
    new ShadowRunStore(MongoShadowRunBackend.writer(database, mismatches), clock)
  }

  "the admin view's reader" should "read the run the worker's writer persisted, and the writer own the TTL index" in {
    fresh()
    val clock  = new MutableClock(ShadowRunStoreBehaviour.T0)
    val writer = new ShadowRunStore(MongoShadowRunBackend.writer(database, mismatches), clock)
    val reader = new ShadowRunStore(MongoShadowRunBackend.reader(database), clock)
    val (cluster, family) = ShadowRunStoreBehaviour.sample
    val run = ShadowRun(ShadowRunStoreBehaviour.T0, Seq(cluster), Seq(family))
    writer.record(run, ShadowRunStoreBehaviour.Retention)
    reader.latestRun() shouldBe Some(run)
    reader.latest() shouldBe Seq(cluster.decision)
    mismatches.names shouldBe empty
  }

  override def afterAll(): Unit =
    try Await.ready(database.drop().toFuture(), 60.seconds) finally { client.close(); super.afterAll() }
}
