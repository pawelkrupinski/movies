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

  // A run's decisions went out as ONE insertMany: 2,233 documents, a 6-8 MB message on US every
  // shadow tick, which kept an 8 MB buffer in the driver's pool for good (live US dump, 09-29).
  "the shadow run writer" should "send a run's documents in bounded inserts" in {
    val inserts = new java.util.concurrent.ConcurrentLinkedQueue[Int]()
    val watched = MongoClient(MongoClientSettings.builder()
      .applyConnectionString(new ConnectionString(mongoTarget.uri.value))
      .codecRegistry(MongoClient.DEFAULT_CODEC_REGISTRY)
      .addCommandListener(new com.mongodb.event.CommandListener {
        override def commandStarted(event: com.mongodb.event.CommandStartedEvent): Unit =
          if (event.getCommandName == "insert") inserts.add(event.getCommand.getArray("documents", new org.bson.BsonArray()).size)
      }).build())
    try {
      val db = watched.getDatabase(IntegrationCorpusDatabase.named(mongoTarget, "shadow-run-batches"))
      val (cluster, family) = ShadowRunStoreBehaviour.sample
      val run = ShadowRun(ShadowRunStoreBehaviour.T0, Seq.fill(1200)(cluster), Seq.fill(10)(family))
      new ShadowRunStore(MongoShadowRunBackend.writer(db, mismatches), new MutableClock(ShadowRunStoreBehaviour.T0))
        .record(run, ShadowRunStoreBehaviour.Retention)
      import scala.jdk.CollectionConverters._
      withClue(s"insert sizes ${inserts.asScala.toSeq}: ")(inserts.asScala.max should be <= MongoShadowRunBackend.InsertBatch)
      inserts.asScala.sum shouldBe 1210
      Await.ready(db.drop().toFuture(), 60.seconds)
    } finally watched.close()
  }

  override def afterAll(): Unit =
    try Await.ready(database.drop().toFuture(), 60.seconds) finally { client.close(); super.afterAll() }
}
