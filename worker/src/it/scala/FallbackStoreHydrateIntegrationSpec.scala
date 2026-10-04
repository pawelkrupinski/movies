package integration

import org.mongodb.scala.MongoClient
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.fallback.{FallbackState, MongoFallbackStore}

import java.time.Instant

/**
 * The Filmweb-fallback store's mirror against real MongoDB. A boot hydrate that FAILED left the
 * mirror empty and every read answered "no cinema is on fallback": the scraper rebuilt each state
 * from nothing and wrote it over the stored one, losing its failure streak, its history and
 * whether it had already paged. A read now hydrates again, or throws.
 */
class FallbackStoreHydrateIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private val isolated = tools.IsolatedMongoDatabase.open(mongoTarget, "fallback-hydrate")
  // Nothing listens on port 1: every read fails once server selection gives up.
  private val unreachable = MongoClient("mongodb://127.0.0.1:1/?serverSelectionTimeoutMS=200&connectTimeoutMS=200")

  override protected def afterAll(): Unit = try { unreachable.close(); isolated.drop() } finally super.afterAll()

  "MongoFallbackStore" should "read back a stored state through a fresh store's hydrate" in {
    val at    = Instant.parse("2026-10-01T00:00:00Z")
    val state = FallbackState(cinema = "Kino Test", active = true, fallbackSource = "Filmweb", fallbackRef = None,
      since = Some(at), lastReason = None, consecutiveFailures = 2, lastPrimaryProbeAt = None, nextPrimaryProbeAt = None,
      updatedAt = at, history = Nil)
    new MongoFallbackStore(Some(isolated.database), _root_.tools.SpecClock.Pinned).put(state)
    new MongoFallbackStore(Some(isolated.database), _root_.tools.SpecClock.Pinned).get("Kino Test").map(_.active) shouldBe Some(true)
  }

  it should "throw, not answer 'not on fallback', while its hydrate cannot read the collection" in {
    val blind = new MongoFallbackStore(Some(unreachable.getDatabase("fallback-hydrate")), _root_.tools.SpecClock.Pinned)
    an[IllegalStateException] should be thrownBy blind.get("Kino Test")
    an[IllegalStateException] should be thrownBy blind.findAll()
  }

  it should "throw at once between hydrate retries, not re-read on every call" in {
    val clock = new tools.MutableClock(Instant.parse("2026-10-01T00:00:00Z"))
    val blind = new MongoFallbackStore(Some(unreachable.getDatabase("fallback-hydrate")), clock)
    (1 to 5).foreach(_ => an[IllegalStateException] should be thrownBy blind.get("Kino Test"))
    blind.hydrateAttempts shouldBe 1
    clock.advance(java.time.Duration.ofMillis(MongoFallbackStore.HydrateRetry.toMillis))
    an[IllegalStateException] should be thrownBy blind.findAll()
    blind.hydrateAttempts shouldBe 2
  }
}
