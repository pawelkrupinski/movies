package integration

import com.mongodb.{ConnectionString, MongoClientSettings}
import org.mongodb.scala.{MongoClient, MongoDatabase, SingleObservableFuture}
import org.scalatest.BeforeAndAfterAll
import services.TtlIndexMismatches
import services.observations.{MongoObservationBackend, ObservationStore, ObservationStoreBehaviour}
import tools.{IntegrationCorpusDatabase, IntegrationMongoSuite, MutableClock}

import scala.concurrent.Await
import scala.concurrent.duration._

/** The observation store's rules over the Mongo backend — the same cases `ObservationStoreSpec`
 *  runs in memory, against the shadow collections' real codecs, indexes and queries. */
class MongoObservationStoreIntegrationSpec extends ObservationStoreBehaviour with BeforeAndAfterAll with IntegrationMongoSuite {

  private val client = MongoClient(MongoClientSettings.builder()
    .applyConnectionString(new ConnectionString(mongoTarget.uri.value))
    .codecRegistry(MongoClient.DEFAULT_CODEC_REGISTRY).build())
  private val database: MongoDatabase = client.getDatabase(IntegrationCorpusDatabase.named(mongoTarget, "observations"))
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

  "the purge of the unscoped capture" should "report without deleting by default, and under --apply delete every version of what is not identity evidence" in {
    import services.observations.{LookupAnswer, LookupQuery}
    import tools.PurgeNonIdentityObservations.{Mode, purge}
    val clock    = new MutableClock(java.time.Instant.parse("2026-09-26T10:00:00Z"))
    val store    = newStore(clock)
    val search   = LookupQuery.of("GET", "https://api.themoviedb.org/3/search/movie?query=Belle")
    val detail   = LookupQuery.venueDetail("Kino Muza", "/film/belle")
    val metacritic = LookupQuery.of("GET", "https://www.metacritic.com/movie/belle/")
    val imdb     = LookupQuery.of("POST", "https://caching.graphql.imdb.com/", Some("{rating}"))
    Seq(search, detail, metacritic, imdb).foreach(store.observeLookup(_, LookupAnswer.Body("first")))
    clock.advance(java.time.Duration.ofHours(1))
    store.observeLookup(metacritic, LookupAnswer.Body("second")) // a superseded version too
    val lookups = database.getCollection[org.mongodb.scala.Document](ObservationStore.LookupsCollection)

    // Pages of one key: the scan crosses page boundaries, and a key's versions straddle one.
    purge(lookups, Mode.DryRun, pageSize = 1) shouldBe Set(metacritic.key, imdb.key)
    store.currentLookups().map(_.query).toSet shouldBe Set(search, detail, metacritic, imdb)

    purge(lookups, Mode.Apply, pageSize = 1) shouldBe Set(metacritic.key, imdb.key)
    store.currentLookups().map(_.query).toSet shouldBe Set(search, detail)
    store.lookupHistory(metacritic) shouldBe empty
    purge(lookups, Mode.Apply) shouldBe empty
  }

  override def afterAll(): Unit =
    try Await.ready(database.drop().toFuture(), 60.seconds) finally { client.close(); super.afterAll() }
}
