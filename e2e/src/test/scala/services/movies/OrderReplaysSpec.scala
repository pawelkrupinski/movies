package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** A recording's replays fetch live, so beside the boot they quadrupled its TMDB rate and drew 429s until the breaker
 *  opened and the replays resolved no film (Germany, recording run 37719613743). */
class OrderReplaysSpec extends AnyFlatSpec with Matchers {

  "the order replays" should "start beside the boot only in a hermetic run that includes their test" in {
    OrderReplays.besideTheBoot(hermetic = true, included = true) shouldBe true
    OrderReplays.besideTheBoot(hermetic = false, included = true) shouldBe false
    OrderReplays.besideTheBoot(hermetic = true, included = false) shouldBe false
  }
}
