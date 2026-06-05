package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ScrapeCitiesSpec extends AnyFlatSpec with Matchers {

  private val default = Set("poznan")

  "ScrapeCities.enabled" should "fall back to the default when the override is unset" in {
    ScrapeCities.enabled(None, default) shouldBe Set("poznan")
  }

  it should "fall back to the default when the override is blank or has no tokens" in {
    ScrapeCities.enabled(Some(""), default)    shouldBe Set("poznan")
    ScrapeCities.enabled(Some("  , "), default) shouldBe Set("poznan")
  }

  it should "parse a comma-separated list, trimmed and lowercased" in {
    ScrapeCities.enabled(Some(" Poznan , wroclaw ,"), default) shouldBe Set("poznan", "wroclaw")
  }

  it should "enable all three when listed" in {
    ScrapeCities.enabled(Some("poznan,wroclaw,warszawa"), default) shouldBe
      Set("poznan", "wroclaw", "warszawa")
  }

  it should "let the override REPLACE the default (not merge) — e.g. wroclaw only" in {
    ScrapeCities.enabled(Some("wroclaw"), default) shouldBe Set("wroclaw")
  }
}
