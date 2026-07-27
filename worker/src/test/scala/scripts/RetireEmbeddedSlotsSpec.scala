package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The whole safety of the retirement pass is one predicate: strip a film's embedded
 * copy ONLY when `movie_slots` demonstrably holds everything that copy had. Get it
 * wrong and the film has no cinemas in either place, which nothing downstream can
 * recover from — so the predicate is pinned rather than trusted.
 */
class RetireEmbeddedSlotsSpec extends AnyFlatSpec with Matchers {
  import RetireEmbeddedSlots.coversEmbedded

  "coversEmbedded" should "allow the strip when the stored rows hold every embedded slot" in {
    coversEmbedded(Set("cinemaA", "cinemaB"), Set("cinemaA", "cinemaB")) shouldBe true
  }

  it should "allow it when the stored rows hold MORE than the embedded map" in {
    // the film gained a cinema since the copy was written — the copy is stale, not richer
    coversEmbedded(Set("cinemaA"), Set("cinemaA", "cinemaB")) shouldBe true
  }

  it should "REFUSE when a stored slot is missing" in {
    coversEmbedded(Set("cinemaA", "cinemaB"), Set("cinemaA")) shouldBe false
  }

  it should "REFUSE when there are no stored rows at all" in {
    coversEmbedded(Set("cinemaA"), Set.empty) shouldBe false
  }

  // An empty embedded map means there is nothing to retire; treating it as "covered"
  // would report phantom work and, worse, invite a strip of a film that has no slots
  // recorded anywhere.
  it should "REFUSE an empty embedded map rather than counting it as done" in {
    coversEmbedded(Set.empty, Set("cinemaA")) shouldBe false
    coversEmbedded(Set.empty, Set.empty)      shouldBe false
  }
}
