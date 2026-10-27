package integration

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{MongoPinStore, Pin, PinClaim}
import services.movies.ListingKey
import org.mongodb.scala.SingleObservableFuture
import tools.IsolatedMongoDatabase

import java.time.Instant

/** The pin store against REAL Mongo: a pin written is read back whole, a re-asserted pin stays one
 *  document, and a removed one is gone. */
class MongoPinStoreIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private val isolated = IsolatedMongoDatabase.open(mongoTarget, "identity-pins")
  // Prod's collection already exists; creating it here, under the spec's own I/O timeout, keeps a fresh database's
  // first-write collection creation (seconds under a loaded machine) out of the store's 10s request budget.
  scala.concurrent.Await.result(isolated.database.createCollection(MongoPinStore.Collection).toFuture(), tools.SpecTimeouts.Io)
  private val store    = new MongoPinStore(Some(isolated.database))
  private val At       = Instant.parse("2026-09-26T12:00:00Z")

  override protected def afterAll(): Unit = try isolated.drop() finally super.afterAll()

  private val first  = ListingKey.Published("Kino A", "Film | klasyka w 4k", None, Nil)
  private val second = ListingKey.Native("Kino B", "https://b/film", "Film")

  "a pin" should "be read back whole, stay one document when re-asserted, and be gone once removed" in {
    val pin = Pin(Seq(first, second).sorted, PinClaim.SameFilm, "admin@example.com", "one film", At)
    store.insert(pin)
    store.all() shouldBe Seq(pin)
    store.insert(Pin(Seq(first, second).sorted, PinClaim.SameFilm, "admin@example.com", "again", At))
    store.all() should have size 1
    store.delete(pin.id) shouldBe true
    store.delete(pin.id) shouldBe false
    store.all() shouldBe empty
  }
}
