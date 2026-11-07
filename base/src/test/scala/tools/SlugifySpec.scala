package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SlugifySpec extends AnyFlatSpec with Matchers {

  "Slugify" should "lowercase and hyphenate a plain title" in {
    Slugify("Top Gun: Maverick") shouldBe "top-gun-maverick"
  }

  it should "fold Polish diacritics rather than dropping the letters" in {
    // The whole point of routing through `deburr`: a naive `[^a-z0-9]+` filter
    // would turn "Chłopi" into "ch-opi" and lose the ł entirely.
    Slugify("Chłopi") shouldBe "chlopi"
    Slugify("Zimna wojna — Zażółć gęślą jaźń") shouldBe "zimna-wojna-zazolc-gesla-jazn"
  }

  it should "fold German umlauts and ß" in {
    Slugify("Über den Wolken") shouldBe "uber-den-wolken"
    // NFD leaves ß intact, so without the explicit fold this would read
    // "groe-freiheit" — a slug that reads as a typo.
    Slugify("Große Freiheit") shouldBe "grosse-freiheit"
  }

  it should "romanize Cyrillic instead of folding it away to nothing" in {
    Slugify("Ваяна") shouldBe "vaiana"
  }

  it should "collapse punctuation runs and trim the edges" in {
    Slugify("\"Kultowe wakacje\" - Ghost in the Shell (1995)") shouldBe
      "kultowe-wakacje-ghost-in-the-shell-1995"
    Slugify("...And Justice for All...") shouldBe "and-justice-for-all"
    Slugify("A  //  B") shouldBe "a-b"
  }

  it should "keep digits, including a year suffix" in {
    Slugify("Blade Runner 2049") shouldBe "blade-runner-2049"
  }

  it should "return an empty slug when nothing survives the fold" in {
    // Callers must handle this — `FilmHref` falls back to the query form so a
    // title like this still gets a working link.
    Slugify("!!!") shouldBe ""
    Slugify("") shouldBe ""
  }

  it should "be stable under repeated application" in {
    // A slug fed back through the fold must not shift, or a canonical URL
    // built from an already-slugged value would drift.
    val slug = Slugify("Zażółć gęślą jaźń: Część II")
    Slugify(slug) shouldBe slug
  }
}
