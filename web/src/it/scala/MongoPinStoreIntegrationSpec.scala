package integration

import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.{MongoPinStore, PinClaim, Pins}
import services.movies.ListingKey
import tools.{Env, IsolatedMongoDatabase}

import java.time.{Clock, Instant, ZoneOffset}

/** The admin pin store against REAL Mongo, through the pin rules the admin page uses: a pin
 *  written is read back whole, a re-asserted pin stays one document, and a removed one is gone. */
class MongoPinStoreIntegrationSpec extends AnyFlatSpec with Matchers with BeforeAndAfterAll {

  assume(Env.fromProcess().get("MONGODB_URI").isDefined, "MONGODB_URI not set")
  tools.IntegrationMongo.requireThrowaway()

  private val isolated = IsolatedMongoDatabase.open(Env.fromProcess().get("MONGODB_URI").get, "identity-pins")
  private val pins     = new Pins(new MongoPinStore(Some(isolated.database)),
    Clock.fixed(Instant.parse("2026-09-26T12:00:00Z"), ZoneOffset.UTC))

  override protected def afterAll(): Unit = try isolated.drop() finally super.afterAll()

  private val a = ListingKey.Published("Kino A", "Film | klasyka w 4k", None, Nil)
  private val b = ListingKey.Native("Kino B", "https://b/film", "Film")

  "a pin" should "be read back whole, refused a second time, and gone once removed" in {
    val pin = pins.add(Seq(a, b), PinClaim.SameFilm, "admin@example.com", "one film").toOption.get
    pins.all() shouldBe Seq(pin)
    pins.add(Seq(b, a), PinClaim.SameFilm, "admin@example.com", "again").isLeft shouldBe true
    pins.all() should have size 1
    pins.remove(pin.id) shouldBe true
    pins.all() shouldBe empty
  }
}
