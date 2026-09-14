package views

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import testsupport.TestMessages

// The dynamic picker's country pills (`landing.scala.html`'s
// `renderPickerCountries`) used to read `country.name` straight off
// `KINOWO_CATALOG`, i.e. `Country.displayName` — each country's own FIXED
// native/English label, baked once into the boot-time `/api/catalog` JSON
// and so incapable of varying with a visitor's chosen UI language (Germany's
// pill always said "Deutschland", everywhere). `COUNTRY_NAMES` is the fix: a
// `country.<code>` lookup embedded per-request from `Messages`, mirroring the
// STATIC fallback list's existing `messages("country." + c.code)` (see
// `LandingApexSpec`).
class LandingCountryNamesLocalizationSpec extends AnyFlatSpec with Matchers {

  private def render(lang: String): String =
    views.html.landing(Country.default, isApex = false)(using TestMessages.forLang(lang)).body

  "the dynamic picker's COUNTRY_NAMES" should
    "carry Germany's name in the visitor's chosen language, not its own native name" in {
    render("de") should include (""""de":"Deutschland"""")
    render("en") should include (""""de":"Germany"""")
    render("pl") should include (""""de":"Niemcy"""")
  }

  it should "carry every switchable country, keyed by its localized name in that language" in {
    val html = render("es")
    Country.switchable.foreach { c =>
      html should include (s""""${c.code}":"${TestMessages.forLang("es")("country." + c.code)}"""")
    }
  }
}
