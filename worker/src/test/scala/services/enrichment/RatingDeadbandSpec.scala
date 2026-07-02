package services.enrichment

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import services.enrichment.RatingDeadband.{Decision, Pending}

/**
 * Unit tests for the pure confirmation-deadband fold. The interesting property
 * is that a single-refresh excursion that reverts on the next fetch (the A→B→A
 * rounding-boundary rating flap) yields ZERO commits, while a value that
 * persists commits once it's been seen `confirmations` times in a row.
 */
class RatingDeadbandSpec extends AnyFlatSpec with Matchers {

  private def decide(stored: Option[String], fresh: Option[String], pending: Option[Pending], confirmations: Int) =
    RatingDeadband.decide(stored, fresh, pending, confirmations)

  "decide" should "commit the first value ever seen regardless of the confirmation count" in {
    decide(stored = None, fresh = Some("7.3"), pending = None, confirmations = 3) shouldBe
      Decision(commit = true, pending = None)
  }

  it should "never write, and clear any candidate, when the fresh value equals what's shown" in {
    decide(Some("7.3"), Some("7.3"), pending = Some(Pending("7.4", 1)), confirmations = 2) shouldBe
      Decision(commit = false, pending = None)
  }

  it should "not write and preserve the candidate when nothing was fetched" in {
    val pending = Some(Pending("7.4", 1))
    decide(Some("7.3"), fresh = None, pending, confirmations = 2) shouldBe
      Decision(commit = false, pending = pending)
  }

  it should "commit a new value on first sight when the deadband is off (confirmations = 1)" in {
    decide(Some("7.3"), Some("7.4"), pending = None, confirmations = RatingDeadband.Off) shouldBe
      Decision(commit = true, pending = None)
  }

  it should "hold a single-fetch blip, then drop it silently when the value reverts (A→B→A ⇒ no commit)" in {
    // A→B: B is a candidate, held (seen 1 of 2).
    val afterB = decide(stored = Some("7.3"), fresh = Some("7.4"), pending = None, confirmations = 2)
    afterB shouldBe Decision(commit = false, pending = Some(Pending("7.4", 1)))
    // B→A: fresh equals the stored value again — clear the candidate, still no write.
    decide(stored = Some("7.3"), fresh = Some("7.3"), pending = afterB.pending, confirmations = 2) shouldBe
      Decision(commit = false, pending = None)
  }

  it should "commit a change once two consecutive refreshes confirm it (A→B→B)" in {
    val afterB1 = decide(Some("7.3"), Some("7.4"), pending = None, confirmations = 2)
    afterB1 shouldBe Decision(commit = false, pending = Some(Pending("7.4", 1)))
    decide(Some("7.3"), Some("7.4"), pending = afterB1.pending, confirmations = 2) shouldBe
      Decision(commit = true, pending = None)
  }

  it should "require three consecutive sightings when confirmations = 3" in {
    val s1 = decide(Some("66%"), Some("67%"), None, 3)
    s1 shouldBe Decision(commit = false, pending = Some(Pending("67%", 1)))
    val s2 = decide(Some("66%"), Some("67%"), s1.pending, 3)
    s2 shouldBe Decision(commit = false, pending = Some(Pending("67%", 2)))
    decide(Some("66%"), Some("67%"), s2.pending, 3) shouldBe Decision(commit = true, pending = None)
  }

  it should "restart the count when a different new candidate appears mid-confirmation" in {
    val afterB = decide(Some("7.3"), Some("7.4"), None, 2)
    afterB.pending shouldBe Some(Pending("7.4", 1))
    // A third distinct value resets the candidate to itself, seen once.
    decide(Some("7.3"), Some("7.5"), afterB.pending, 2) shouldBe
      Decision(commit = false, pending = Some(Pending("7.5", 1)))
  }
}
