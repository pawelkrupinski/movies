package scripts

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.ListingKey
import tools.UnmatchedClusters.Label

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

  private val nosferatu = ListingKey.Published("Babylon Hagen", "Tim Brownlow - Nosferatu: The Journal of a Broken Man", None, Nil)
  private def label(venue: String, film: String, right: Boolean) =
    Label("de", venue, "Tim Brownlow - Nosferatu: The Journal of a Broken Man", film, right, s"$venue $film")

  "A case pinned from the judgements" should "expect the film they call right" in {
    IdentitySlice.expectationsOf(nosferatu, Country.Germany, Seq(label("*", "tmdb:653", right = false), label("*", "tmdb:1", right = true)))
      .map(_.film) shouldBe Seq("tmdb:1")
  }

  it should "expect none of the films they call wrong when they call none right" in {
    IdentitySlice.expectationsOf(nosferatu, Country.Germany, Seq(label("*", "tmdb:653", right = false), label("Babylon Hagen", "tmdb:2", right = false)))
      .map(_.film) shouldBe Seq("not:tmdb:2", "not:tmdb:653")
  }

  it should "read a film's label at the venue before every venue's" in {
    IdentitySlice.expectationsOf(nosferatu, Country.Germany, Seq(label("*", "tmdb:653", right = false), label("Babylon Hagen", "tmdb:653", right = true)))
      .map(_.film) shouldBe Seq("tmdb:653")
    IdentitySlice.expectationsOf(nosferatu, Country.Germany, Seq(label("Other", "tmdb:653", right = false))) shouldBe empty
  }
}
