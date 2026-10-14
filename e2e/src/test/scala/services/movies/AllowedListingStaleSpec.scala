package services.movies

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AllowedListingStaleSpec extends AnyFlatSpec with Matchers {
  private val muza    = AllowedListing("Kino Muza", "Międzynarodowy Dzień Animacji")
  private val jubilee = AllowedListing("Pictureville", "Jubilee (1978)")
  private val allow   = Set(muza, jubilee)

  // Run 37223407761 re-recorded PL past the Muza parser fix: the listing is carried and now passes. That is the
  // fix arriving, not a stale entry — it must not turn the convergence lane red.
  "a re-recorded corpus" should "not fail an entry awaiting its re-record once it passes" in {
    AllowedListing.stale(allow, carried = allow, stillBreaking = Set(jubilee), awaitingReRecord = Set(muza)) shouldBe empty
  }

  it should "still fail an ordinary entry that passes" in {
    AllowedListing.stale(allow, carried = allow, stillBreaking = Set.empty, awaitingReRecord = Set(muza)) shouldBe Set(jubilee)
  }

  it should "say nothing about an entry whose listing the corpus does not carry" in {
    AllowedListing.stale(allow, carried = Set(jubilee), stillBreaking = Set(jubilee), awaitingReRecord = Set.empty) shouldBe empty
  }

  // Cineamo billed "…außergewöhnliche  Freundschaft von A" (two spaces) when it was allowlisted, Astor Film Lounge
  // "…außergewöhnliche Freundschaft von A" a recording later: one billing, which a venue's spacing does not make anew.
  "an entry" should "hold the listing spelled alike but for its runs of spaces" in {
    AllowedListing.anywhere("Freundschaft  von A") shouldBe AllowedListing.anywhere("Freundschaft von A ")
    AllowedListing("Kino  Muza", "a\tb") shouldBe AllowedListing("Kino Muza", "a b")
  }
}
