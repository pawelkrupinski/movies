package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** What a pinned slice's listing is held to ([[IdentitySlice.Expected]]): its right film, or — set by hand in the case's
 *  expected.tsv — no film (`none`), or any film but a wrong one (`not:<film>`). */
class IdentitySliceExpectedSpec extends AnyFlatSpec with Matchers {

  private def expected(film: String) = IdentitySlice.Expected("IL KINO", "Ukrainian Film Festival Berlin: CAMOUFLAGE", film, "")

  "A pinned listing's expectation" should "hold its right film, and only that" in {
    expected("tmdb:1735950").holds(Some("tmdb:1735950")) shouldBe true
    expected("tmdb:1735950").holds(Some("tmdb:56085")) shouldBe false
    expected("tmdb:1735950").holds(None) shouldBe false
  }

  it should "hold no film when it says none" in {
    expected("none").holds(Some("none")) shouldBe true
    expected("none").holds(Some("tmdb:56085")) shouldBe false
  }

  it should "hold any take but the wrong film it names" in {
    expected("not:tmdb:56085").holds(Some("none")) shouldBe true
    expected("not:tmdb:56085").holds(Some("tmdb:1735950")) shouldBe true
    expected("not:tmdb:56085").holds(Some("tmdb:56085")) shouldBe false
  }

  it should "be pending, its expectation the same, while prefixed pending:" in {
    expected("pending:not:tmdb:56085").pending shouldBe true
    expected("pending:not:tmdb:56085").holds(Some("tmdb:56085")) shouldBe false
    expected("pending:not:tmdb:56085").holds(Some("none")) shouldBe true
    expected("not:tmdb:56085").pending shouldBe false
  }
}
