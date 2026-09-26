package services.identity

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.identity.KnownCasePins._

import java.time.{Clock, ZoneOffset}

/** The pin service's rules, above the store seam: what a pin must carry, and which pins are
 *  refused because they contradict the ones already held. */
class PinsSpec extends AnyFlatSpec with Matchers {

  private def pins() = new Pins(new InMemoryPinStore, Clock.fixed(At, ZoneOffset.UTC))

  "adding a pin" should "stamp it with its author, reason and the clock's time, and hold it" in {
    val p = pins()
    val pin = p.add(Seq(opetanie), PinClaim.IsFilm(Possession1981), "admin@example.com", "the strand is the 1981 film")
    pin.map(_.createdAt) shouldBe Right(At)
    p.all() shouldBe pin.toSeq
    p.constraints().resolvedFilm(opetanie, None) shouldBe Some(Possession1981)
  }

  it should "refuse a pin with no listing, a one-listing same-film pin, a bad film id, or no author or reason" in {
    val p = pins()
    p.add(Nil, PinClaim.IsFilm(1), "a", "r") shouldBe a[Left[?, ?]]
    p.add(Seq(opetanie), PinClaim.SameFilm, "a", "r") shouldBe a[Left[?, ?]]
    p.add(Seq(opetanie), PinClaim.IsFilm(0), "a", "r") shouldBe a[Left[?, ?]]
    p.add(Seq(opetanie), PinClaim.IsFilm(1), " ", "r") shouldBe a[Left[?, ?]]
    p.add(Seq(opetanie), PinClaim.IsFilm(1), "a", "") shouldBe a[Left[?, ?]]
    p.all() shouldBe empty
  }

  it should "refuse a pin that contradicts one already held, naming the conflict" in {
    val p = pins()
    p.add(Seq(opetanie), PinClaim.IsFilm(Possession1981), "a", "r").isRight shouldBe true
    val refused = p.add(Seq(opetanie), PinClaim.NeverFilm(Possession1981), "a", "r")
    refused.left.toOption.get should include(Possession1981.toString)
    p.all() should have size 1
  }

  it should "refuse the same claim on the same listings twice" in {
    val p = pins()
    p.add(lalka, PinClaim.SameFilm, "a", "r").isRight shouldBe true
    p.add(lalka.reverse, PinClaim.SameFilm, "b", "again") shouldBe a[Left[?, ?]]
  }

  "removing a pin" should "drop it and its constraints" in {
    val p = pins()
    val pin = p.add(lalka, PinClaim.SameFilm, "a", "r").toOption.get
    p.remove(pin.id) shouldBe true
    p.remove(pin.id) shouldBe false
    p.constraints().mustLinks shouldBe empty
  }
}
