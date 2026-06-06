package services.cinemas

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CountryNamesSpec extends AnyFlatSpec with Matchers {

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
