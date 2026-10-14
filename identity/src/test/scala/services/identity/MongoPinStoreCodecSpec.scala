package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A pin survives the `identity_pins` document shape whole: every claim, both listing-key shapes
 *  (a Published key with and without its year and directors). */
class MongoPinStoreCodecSpec extends AnyFlatSpec with Matchers {

  "the pin document" should "round-trip every fixture pin, keyed by the pin's id" in {
    KnownCasePins.all.foreach { pin =>
      val doc = MongoPinStore.encode(pin)
      doc.get("_id").map(_.asString.getValue) shouldBe Some(pin.id)
      MongoPinStore.decode(doc) shouldBe Some(pin)
    }
  }

  it should "come back under the id it was stored under, so a re-asserted pin is refused and a removal finds it" in {
    // The decoder builds a key's directors as whatever collection it reads them into; an id hashed from the
    // collection's toString ("List()" stored, "Vector()" read) refused nothing and removed nothing (CI run 37758508401).
    KnownCasePins.all.foreach { pin =>
      val doc = MongoPinStore.encode(pin)
      MongoPinStore.decode(doc).map(_.id) shouldBe doc.get("_id").map(_.asString.getValue)
    }
  }

  it should "decode an unreadable document as nothing rather than throwing" in {
    MongoPinStore.decode(org.mongodb.scala.bson.collection.immutable.Document("_id" -> "x", "kind" -> "is-film")) shouldBe None
  }
}
