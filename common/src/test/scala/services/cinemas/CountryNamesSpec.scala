package services.cinemas

import java.util.Locale

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CountryNamesSpec extends AnyFlatSpec with Matchers {

  private val polish  = Locale.forLanguageTag("pl-PL")
  private val english = Locale.forLanguageTag("en-GB")
  private val german  = Locale.forLanguageTag("de-DE")

  "canonical" should "map English country names to Polish" in {
    CountryNames.canonical("Luxembourg") shouldBe "Luksemburg"
    CountryNames.canonical("Germany") shouldBe "Niemcy"
    CountryNames.canonical("France") shouldBe "Francja"
    CountryNames.canonical("United States") shouldBe "USA"
    CountryNames.canonical("United Kingdom") shouldBe "Wielka Brytania"
  }

  it should "be case-insensitive" in {
    CountryNames.canonical("luxembourg") shouldBe "Luksemburg"
    CountryNames.canonical("LUXEMBOURG") shouldBe "Luksemburg"
    CountryNames.canonical("usa") shouldBe "USA"
  }

  it should "pass through already-canonical Polish names unchanged" in {
    CountryNames.canonical("Polska") shouldBe "Polska"
    CountryNames.canonical("Luksemburg") shouldBe "Luksemburg"
    CountryNames.canonical("Francja") shouldBe "Francja"
  }

  it should "pass through unknown countries verbatim" in {
    CountryNames.canonical("Atlantyda") shouldBe "Atlantyda"
  }

  it should "recognise the 'Wlk. Brytania' abbreviation and Irak" in {
    CountryNames.canonical("Wlk. Brytania") shouldBe "Wielka Brytania"
    CountryNames.isPolish("Wlk. Brytania") shouldBe true
    CountryNames.isPolish("Irak") shouldBe true
  }

  "canonical(raw, language)" should "fold to the Polish name for a Polish deployment" in {
    CountryNames.canonical("United Kingdom", polish) shouldBe "Wielka Brytania"
    CountryNames.canonical("United States", polish) shouldBe "USA"
    CountryNames.canonical("Germany", polish) shouldBe "Niemcy"
  }

  it should "localise the country name to a non-Polish deployment's language" in {
    // A UK/German deployment must NOT be folded to a Polish name.
    CountryNames.canonical("United Kingdom", english) shouldBe "United Kingdom"
    CountryNames.canonical("Germany", german) shouldBe "Deutschland"
    CountryNames.canonical("USA", german) shouldBe "Vereinigte Staaten"
  }

  it should "fold every spelling variant of a country into ONE name for a non-Polish deployment" in {
    // Regression: the UK film page listed "USA" and "United States" side by
    // side. All variants must collapse to a single localised name.
    val us = CountryNames.canonical("United States", english)
    us shouldBe "United States"
    CountryNames.canonical("USA", english) shouldBe us
    CountryNames.canonical("United States of America", english) shouldBe us
    CountryNames.canonical("U.S.", english) shouldBe us
  }

  it should "trim and pass through an unknown country outside Poland" in {
    CountryNames.canonical("  United Kingdom  ", english) shouldBe "United Kingdom"
    CountryNames.canonical("Atlantyda", english) shouldBe "Atlantyda"
  }

  "isPolish" should "recognise canonical names and their aliases" in {
    CountryNames.isPolish("Luksemburg") shouldBe true
    CountryNames.isPolish("Luxembourg") shouldBe true
    CountryNames.isPolish("Polska") shouldBe true
    CountryNames.isPolish("USA") shouldBe true
  }

  it should "reject unknown strings" in {
    CountryNames.isPolish("Atlantyda") shouldBe false
    CountryNames.isPolish("2026") shouldBe false
  }
}
