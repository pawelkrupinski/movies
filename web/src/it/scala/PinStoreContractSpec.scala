package integration

import java.time.Instant

import models.ListingKey
import org.mongodb.scala.SingleObservableFuture
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.vocabulary.{Pin, PinClaim, PinStore}
import services.identity.{InMemoryPinStore, MongoPinStore}
import tools.IsolatedMongoDatabase

/** The pin store, as both stores keep it: a pin written is read back whole, a re-asserted pin stays one document, a
 *  removed one is gone, and one without a database holds nothing and refuses a write. Mongo's half runs on REAL Mongo. */
class PinStoreContractSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private val isolated = IsolatedMongoDatabase.open(mongoTarget, "identity-pins")
  // Prod's collection already exists; creating it here, under the spec's own I/O timeout, keeps a fresh database's
  // first-write collection creation (seconds under a loaded machine) out of the store's 10s request budget.
  scala.concurrent.Await.result(isolated.database.createCollection(MongoPinStore.Collection).toFuture(), tools.SpecTimeouts.Io)
  private val At = Instant.parse("2026-09-26T12:00:00Z")

  override protected def afterAll(): Unit = try isolated.drop() finally super.afterAll()

  private val first  = ListingKey.Published("Kino A", "Film | klasyka w 4k", None, Nil)
  private val second = ListingKey.Native("Kino B", "https://b/film", "Film")

  private val implementations: Seq[(String, () => PinStore)] = Seq(
    "InMemoryPinStore" -> (() => new InMemoryPinStore),
    "MongoPinStore"    -> (() => { isolated.freshCollection(MongoPinStore.Collection); new MongoPinStore(Some(isolated.database)) }))

  implementations.foreach { case (name, fresh) =>
    s"[$name] a pin" should "be read back whole, stay one document when re-asserted, and be gone once removed" in {
      val store = fresh()
      val pin = Pin(Seq(first, second).sorted, PinClaim.SameFilm, "admin@example.com", "one film", At)
      store.insert(pin)
      store.all() shouldBe Seq(pin)
      store.insert(Pin(Seq(first, second).sorted, PinClaim.SameFilm, "admin@example.com", "again", At))
      store.all() should have size 1
      store.delete(pin.id) shouldBe true
      store.delete(pin.id) shouldBe false
      store.all() shouldBe empty
    }

    it should "keep every claim apart" in {
      val store = fresh()
      val pins  = Seq(PinClaim.IsFilm(1), PinClaim.NeverFilm(2), PinClaim.SameFilm).zip(Seq(Seq(first), Seq(second), Seq(first, second).sorted))
        .map { case (claim, listings) => Pin(listings, claim, "admin@example.com", "why", At) }
      pins.foreach(store.insert)
      store.all().toSet shouldBe pins.toSet
    }
  }

  "a pin store without a database" should "hold nothing, delete nothing and refuse a pin" in {
    val store = new MongoPinStore(None)
    store.all() shouldBe empty
    store.delete("any") shouldBe false
    an[IllegalStateException] should be thrownBy store.insert(Pin(Seq(first), PinClaim.SameFilm, "admin@example.com", "why", At))
  }
}
