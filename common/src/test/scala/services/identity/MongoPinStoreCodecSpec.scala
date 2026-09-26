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

  it should "decode an unreadable document as nothing rather than throwing" in {
    MongoPinStore.decode(org.mongodb.scala.bson.collection.immutable.Document("_id" -> "x", "kind" -> "is-film")) shouldBe None
  }
}
