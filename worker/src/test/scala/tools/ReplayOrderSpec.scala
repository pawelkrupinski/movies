package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The one replay world's randomness (fix 3): an order by the seed and each item's own key -- one item more or fewer moves
 *  no other's place, as a shared stream of draws did. */
class ReplayOrderSpec extends AnyFlatSpec with Matchers {
  private val venues = (1 to 40).map(i => s"venue-$i")

  "a replay order" should "place each item by the seed and its own key alone" in {
    val order = ReplayOrder.of(venues, 7L)(identity)
    order.toSet shouldBe venues.toSet
    ReplayOrder.of(venues.reverse, 7L)(identity) shouldBe order                    // whatever order they came in
    ReplayOrder.of(venues :+ "venue-new", 7L)(identity).filterNot(_ == "venue-new") shouldBe order
    ReplayOrder.of(venues.filterNot(_ == "venue-3"), 7L)(identity) shouldBe order.filterNot(_ == "venue-3")
    ReplayOrder.of(venues, 8L)(identity) should not be order                       // another seed, another order
  }
}
