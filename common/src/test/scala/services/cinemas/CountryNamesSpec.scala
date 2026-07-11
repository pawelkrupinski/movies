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

  it should "keep the source's already-localised name for a non-Polish deployment" in {
    // TMDB/IMDb return the country in the requested language, so a UK/German
    // deployment must NOT have it folded to a Polish name.
    CountryNames.canonical("United Kingdom", english) shouldBe "United Kingdom"
    CountryNames.canonical("United States of America", english) shouldBe "United States of America"
    CountryNames.canonical("Deutschland", german) shouldBe "Deutschland"
  }

  it should "trim but not translate outside Poland" in {
    CountryNames.canonical("  United Kingdom  ", english) shouldBe "United Kingdom"
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
