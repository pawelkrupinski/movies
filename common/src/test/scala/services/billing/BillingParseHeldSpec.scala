package services.billing

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A text is parsed once while its parse is held: every reader of the same text reads the one parse, its parts read
 *  once, and a parse made after the held ones were dropped reads exactly as the dropped one did. */
class BillingParseHeldSpec extends AnyFlatSpec with Matchers {
  private val billing = "Unrestricted View Horror Film Festival 2026: Opening Night"

  "A billing's parse" should "be the one already made of the same text" in {
    BillingParse(billing) should be theSameInstanceAs BillingParse(new String(billing.toCharArray))
    BillingParse(billing).programmeSlot shouldBe BillingPieces.programmeSlot(billing)
    BillingParse("Triple: Alien + Aliens + Alien 3").marker shouldBe BillJoins.headingBanner("Triple: Alien + Aliens + Alien 3")
  }

  it should "read as a fresh one does once the held parses are dropped" in {
    val first = BillingParse(billing)
    val parts = (first.programmeSlot, first.uncertified, first.instalment, first.marker)
    (0 to BillingParse.Held).foreach(i => BillingParse(s"filler $i"))
    val again = BillingParse(billing)
    again should not be theSameInstanceAs(first)
    (again.programmeSlot, again.uncertified, again.instalment, again.marker) shouldBe parts
  }
}
