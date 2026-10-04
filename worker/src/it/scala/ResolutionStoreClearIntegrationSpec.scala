package integration

import org.mongodb.scala.MongoClient
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.resolution.MongoResolutionStore

/** An operator's "forget every resolution" against real MongoDB: a clear that could not run throws,
 *  so the bulk refresh behind it reports a failure. It answered "0 forgotten", and the refresh
 *  reported itself dispatched while every stored resolution stayed to be replayed. */
class ResolutionStoreClearIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private val isolated    = tools.IsolatedMongoDatabase.open(mongoTarget, "resolution-clear")
  private val unreachable = MongoClient("mongodb://127.0.0.1:1/?serverSelectionTimeoutMS=200&connectTimeoutMS=200")

  override protected def afterAll(): Unit = try { unreachable.close(); isolated.drop() } finally super.afterAll()

  "MongoResolutionStore.removeAll" should "forget every stored resolution" in {
    val store = new MongoResolutionStore(Some(isolated.database), "resolve_test", normalizer = services.movies.SingleCountryNormalizer.titleNormalizer, ttlMismatches = new services.TtlIndexMismatches, clock = _root_.tools.MongoTtlSpecClock.Pinned)
    store.put("anora|2024", "1064213")   // fire-and-forget: wait for it to land
    _root_.tools.Eventually.poll(pollMs = 20)(store.get("anora|2024").isDefined) shouldBe true
    store.removeAll() shouldBe 1
    store.get("anora|2024") shouldBe None
  }

  it should "throw, not answer zero forgotten, when it cannot reach the store" in {
    val blind = new MongoResolutionStore(Some(unreachable.getDatabase("resolution-clear")), "resolve_test",
      normalizer = services.movies.SingleCountryNormalizer.titleNormalizer, ttlMismatches = new services.TtlIndexMismatches, clock = _root_.tools.MongoTtlSpecClock.Pinned)
    an[Exception] should be thrownBy blind.removeAll()
  }
}
