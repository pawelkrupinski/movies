package views

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The navbar's country switcher translates per the resolved UI language
 * (`messages("country." + code)`), unlike a city's own name, which never
 * translates — see `messages.en`'s `country.*` keys.
 */
class TranslatedCountryNamesSpec extends AnyFlatSpec with Matchers {

  private implicit val city: models.City = models.Poznan

  private def render(lang: String): String = {
    given play.api.i18n.Messages = testsupport.TestMessages.forLang(lang)
    views.html._navbar(devMode = false, oauthProviders = Set.empty).body
  }

  "the country switcher" should "show each country's name translated into the resolved language" in {
    val english = render("en")
    val german  = render("de")
    models.Country.switchable.foreach { c =>
      english should include (testsupport.TestMessages.forLang("en")("country." + c.code))
      german  should include (testsupport.TestMessages.forLang("de")("country." + c.code))
    }
    // Poland's name actually differs between the two bundles — the case that
    // would catch a navbar still hardcoded to `Country.displayName`.
    english should include ("Poland")
    german  should include ("Polen")
    german  should not include ">Poland<"
  }

  it should "leave the city's own name untranslated" in {
    // Poznań is a proper noun — it doesn't have an English or German form in
    // this app, unlike a country's name, and must render identically no
    // matter which language the rest of the chrome is in.
    render("en") should include (models.Poznan.labels.nominative)
    render("de") should include (models.Poznan.labels.nominative)
  }
}
