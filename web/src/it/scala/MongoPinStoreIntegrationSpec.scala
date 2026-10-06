package integration

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{MongoPinStore, PinClaim, Pins}
import services.movies.ListingKey
import org.mongodb.scala.SingleObservableFuture
import tools.IsolatedMongoDatabase

import java.time.{Clock, Instant, ZoneOffset}

/** The admin pin store against REAL Mongo, through the pin rules the admin page uses: a pin
 *  written is read back whole, a re-asserted pin stays one document, and a removed one is gone. */
class MongoPinStoreIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll with tools.IntegrationMongoSuite {

  private val isolated = IsolatedMongoDatabase.open(mongoTarget, "identity-pins")
  // Prod's collection already exists; creating it here, under the spec's own I/O timeout, keeps a fresh database's
  // first-write collection creation (seconds under a loaded machine) out of the store's 10s request budget.
  scala.concurrent.Await.result(isolated.database.createCollection(MongoPinStore.Collection).toFuture(), tools.SpecTimeouts.Io)
  private val pins     = new Pins(new MongoPinStore(Some(isolated.database)),
    Clock.fixed(Instant.parse("2026-09-26T12:00:00Z"), ZoneOffset.UTC))

  override protected def afterAll(): Unit = try isolated.drop() finally super.afterAll()

  private val first  = ListingKey.Published("Kino A", "Film | klasyka w 4k", None, Nil)
  private val second = ListingKey.Native("Kino B", "https://b/film", "Film")

  "a pin" should "be read back whole, refused a second time, and gone once removed" in {
    val pin = pins.add(Seq(first, second), PinClaim.SameFilm, "admin@example.com", "one film").toOption.get
    pins.all() shouldBe Seq(pin)
    pins.add(Seq(second, first), PinClaim.SameFilm, "admin@example.com", "again").isLeft shouldBe true
    pins.all() should have size 1
    pins.remove(pin.id) shouldBe true
    pins.all() shouldBe empty
  }
}
