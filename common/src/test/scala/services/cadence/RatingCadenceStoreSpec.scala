package services.cadence

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant

class RatingCadenceStoreSpec extends AnyFlatSpec with Matchers {

  private val t0 = Instant.parse("2026-06-27T10:00:00Z")

  "InMemoryRatingCadenceStore" should "be empty for an unseen key" in {
    new InMemoryRatingCadenceStore().statsFor("mc|tmdb:1") shouldBe None
  }

  it should "accumulate the streak across no-change refreshes and reset on change" in {
    val store = new InMemoryRatingCadenceStore()
    store.record("mc|tmdb:1", reportedValue = None, t0).unchangedStreak             shouldBe 1
    store.record("mc|tmdb:1", reportedValue = None, t0.plusSeconds(7200)).unchangedStreak shouldBe 2
    store.statsFor("mc|tmdb:1").map(_.unchangedStreak)                          shouldBe Some(2)
    store.record("mc|tmdb:1", reportedValue = Some("85"), t0.plusSeconds(9000)).unchangedStreak shouldBe 0
  }

  it should "key sources independently" in {
    val store = new InMemoryRatingCadenceStore()
    store.record("mc|tmdb:1", reportedValue = None, t0)
    store.record("imdb|tmdb:1", reportedValue = Some("85"), t0)
    store.statsFor("mc|tmdb:1").map(_.unchangedStreak)   shouldBe Some(1)
    store.statsFor("imdb|tmdb:1").map(_.unchangedStreak) shouldBe Some(0)
  }
}
