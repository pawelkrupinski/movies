package integration

import com.mongodb.{ConnectionString, MongoClientSettings}
import org.mongodb.scala.{MongoClient, MongoDatabase, SingleObservableFuture}
import org.scalatest.BeforeAndAfterAll
import services.TtlIndexMismatches
import services.observations.{MongoObservationBackend, ObservationStore, ObservationStoreBehaviour}
import tools.{Env, IntegrationCorpusDatabase, MutableClock}

import scala.concurrent.Await
import scala.concurrent.duration._

/** The observation store's rules over the Mongo backend — the same cases `ObservationStoreSpec`
 *  runs in memory, against the shadow collections' real codecs, indexes and queries. */
class MongoObservationStoreIntegrationSpec extends ObservationStoreBehaviour with BeforeAndAfterAll {

  assume(Env.fromProcess().get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val client = MongoClient(MongoClientSettings.builder()
    .applyConnectionString(new ConnectionString(Env.fromProcess().get("MONGODB_URI").get))
    .codecRegistry(MongoClient.DEFAULT_CODEC_REGISTRY).build())
  private val database: MongoDatabase = client.getDatabase(IntegrationCorpusDatabase.named("observations"))
  private val mismatches = new TtlIndexMismatches

  protected def newStore(clock: MutableClock): ObservationStore = {
    Seq(ObservationStore.ListingsCollection, ObservationStore.LookupsCollection)
      .foreach(c => Await.result(database.getCollection(c).drop().toFuture(), 30.seconds))
    MongoObservationBackend.store(database, clock, mismatches)
  }

  "the shadow collections" should "carry the TTL index the retention relies on, with no mismatch reported" in {
    newStore(new MutableClock(java.time.Instant.parse("2026-09-26T10:00:00Z")))
      .observeLookup(services.observations.LookupQuery.of("GET", "https://x/"), services.observations.LookupAnswer.Body("y"))
    mismatches.names shouldBe empty
  }

  override def afterAll(): Unit =
    try Await.ready(database.drop().toFuture(), 60.seconds) finally { client.close(); super.afterAll() }
}
