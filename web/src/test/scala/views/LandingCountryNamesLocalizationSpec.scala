package views

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import testsupport.TestMessages

// The dynamic picker's country pills (`landing.scala.html`'s
// `renderPickerCountries`) used to read `country.name` straight off
// `KINOWO_CATALOG`, i.e. `Country.displayName` — each country's own FIXED
// native/English label, baked once into the boot-time `/api/catalog` JSON and
// so incapable of varying with a visitor's chosen UI language (Germany's pill
// always said "Deutschland", everywhere).
//
// The fix is now client-side: `countryName(code)` (`landing.scala.html`) calls
// `t('country.' + code)` against the embedded language pack (`#i18n-packs`,
// `controllers.I18nPacks`) at RUNTIME, driven by whatever the visitor last
// picked (`localStorage`) — not a server-baked, request-language-scoped
// object. So every page embeds EVERY language's country names regardless of
// which `Messages` rendered it (unlike the old `COUNTRY_NAMES`, which varied
// per render); this spec asserts the pack itself carries the right mapping in
// every language and that the picker's own lookup function is present.
class LandingCountryNamesLocalizationSpec extends AnyFlatSpec with Matchers {

  // The embedded pack doesn't depend on which `Messages` renders the page
  // any more — picking Polish here (Poland's deployment default) is arbitrary.
  private val html = views.html.landing(Country.default, isApex = false)(using TestMessages.forLang("pl")).body

  "the embedded language pack" should "carry Germany's name in every language, not just its native one" in {
    html should include (""""country.de":"Deutschland"""")
    html should include (""""country.de":"Germany"""")
    html should include (""""country.de":"Niemcy"""")
  }

  it should "carry every switchable country, keyed by its localized name in Spanish" in {
    Country.switchable.foreach { c =>
      html should include (s""""country.${c.code}":"${TestMessages.forLang("es")("country." + c.code)}"""")
    }
  }

  it should "expose the client-side lookup the dynamic picker calls per pill" in {
    html should include ("function countryName(code)")
    html should include ("t('country.' + code)")
  }
}
