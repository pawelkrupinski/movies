package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import testsupport.TestMessages

import java.time.LocalDate
import java.util.Locale

/**
 * Guards the §4 web-i18n wiring: message keys resolve to the right language
 * under `Lang("pl")` / `Lang("en")`, the JS locale payload carries the correct
 * per-language plural rule + showtime forms (the data the shared.js plural
 * function selects with), and `DateFormatter` keeps Polish byte-identical while
 * rendering other languages via their locale.
 *
 * Builds a real `MessagesApi` off the checked-in `conf/messages` + `messages.en`
 * (no running app needed) so the assertions exercise the actual bundles.
 */
class WebI18nSpec extends AnyFlatSpec with Matchers {

  private val pl = TestMessages.forLang("pl")
  private val en = TestMessages.forLang("en")

  "message keys" should "resolve to Polish under Lang(pl)" in {
    pl("nav.sort")          shouldBe "Sortuj"
    pl("day.today")         shouldBe "Dziś"
    pl("empty.repertoire")  shouldBe "Brak repertuaru."
    pl("poster.missing")    shouldBe "Brak plakatu"
    pl("brand.title", "Poznań") shouldBe "Repertuar kinowy Poznań"
    pl("film.titleSuffix", "Poznań", "Kinowo") shouldBe "– godziny seansów Poznań | Kinowo"
  }

  it should "resolve to English under Lang(en)" in {
    en("nav.sort")          shouldBe "Sort"
    en("day.today")         shouldBe "Today"
    en("empty.repertoire")  shouldBe "No listings."
    en("poster.missing")    shouldBe "No poster"
    en("brand.title", "London") shouldBe "Cinema listings London"
    en("film.titleSuffix", "London", "Showtimes") shouldBe "– showtimes London | Showtimes"
  }

  /** Every country's landing asks for a CITY again: the US's places are metros
   *  now (Los Angeles, Houston), grouped under their state rather than being
   *  one. The state-flavoured copy that shipped for the fortnight the US listed
   *  states has no reader left. */
  "the landing copy" should "ask for a city in every bundle, and nowhere for a state" in {
    pl("landing.chooseCity")   shouldBe "Wybierz miasto"
    pl("landing.searchCity")   shouldBe "Szukaj miasta…"
    pl("landing.noCity")       shouldBe "Brak miasta o tej nazwie."
    pl("landing.noNearby")     shouldBe "Brak obsługiwanego miasta w pobliżu — wybierz z listy."
    en("landing.chooseCity")   shouldBe "Choose your city"
    TestMessages.forLang("de")("landing.chooseCity") shouldBe "Wähle deine Stadt"
    for (messages <- Seq(pl, en, TestMessages.forLang("de"));
         key <- Seq("landing.chooseCity.state", "areas.chooseArea", "areas.cinemaCount")) {
      withClue(s"${messages.lang.code} / $key: ")(messages(key) shouldBe key)
    }
  }

  "JsLocale" should "carry the Polish 3-form showtime plural rule" in {
    val json = JsLocale.json(pl)
    json should include("\"plural\":\"pl\"")
    json should include("\"one\":\"seans\"")
    json should include("\"few\":\"seanse\"")
    json should include("\"many\":\"seansów\"")
    json should include("\"emptyRepertoire\":\"Brak repertuaru.\"")
    json should include("Nie")   // Polish short weekday
  }

  it should "carry the English 2-form showtime plural rule" in {
    val json = JsLocale.json(en)
    json should include("\"plural\":\"en\"")
    json should include("\"one\":\"showing\"")
    json should include("\"other\":\"showings\"")
    json should include("\"emptyRepertoire\":\"No listings.\"")
  }

  // German and Spanish share the English one/other RULE but not its words — a
  // German card counting "3 showings" was the English fallback showing through.
  it should "carry the German word forms on the two-form rule" in {
    val json = JsLocale.json(TestMessages.forLang("de"))
    json should include("\"plural\":\"en\"")
    json should include("\"one\":\"Vorstellung\"")
    json should include("\"other\":\"Vorstellungen\"")
    json should not include "showings"
  }

  // The Polish plural rule shared.js implements, mirrored here so the category
  // boundaries (one / few / many) are pinned as a regression on the data that
  // drives it.
  private def plCategory(n: Int): String = {
    val mod10 = n % 10; val mod100 = n % 100
    if (n == 1) "one"
    else if (mod10 >= 2 && mod10 <= 4 && (mod100 < 12 || mod100 > 14)) "few"
    else "many"
  }

  "the Polish showtime plural rule" should "pick one/few/many per Polish grammar" in {
    plCategory(1)  shouldBe "one"
    plCategory(2)  shouldBe "few"   // seanse
    plCategory(4)  shouldBe "few"
    plCategory(5)  shouldBe "many"  // seansów
    plCategory(12) shouldBe "many"
    plCategory(22) shouldBe "few"
    plCategory(25) shouldBe "many"
  }

  /** {0} is the country's `brandName` ("Kinowo" in PL, "Showtimes" elsewhere) —
   *  the app banner substitutes it the same way `brand.title` does above, so a
   *  bundle that dropped the placeholder or mistranslated the fixed half would
   *  surface here rather than only at render time. */
  "the app promotion banner" should "substitute the brand name per bundle" in {
    pl("appBanner.headline", "Kinowo")    shouldBe "Kinowo — aplikacja mobilna"
    en("appBanner.headline", "Showtimes") shouldBe "Showtimes — mobile app"
    TestMessages.forLang("de")("appBanner.headline", "Showtimes") shouldBe "Showtimes — Mobile App"
    TestMessages.forLang("es")("appBanner.headline", "Showtimes") shouldBe "Showtimes — app móvil"
  }

  it should "carry a store-badge line pair in every bundle, none of it hardcoded caps" in {
    for (messages <- Seq(pl, en, TestMessages.forLang("de"), TestMessages.forLang("es"))) withClue(s"${messages.lang.code}: ") {
      val appStoreLine1 = messages("appBanner.appStoreLine1")
      val playLine1     = messages("appBanner.playLine1")
      appStoreLine1 should not be "appBanner.appStoreLine1"   // key actually resolved
      playLine1     should not be "appBanner.playLine1"
      // Sentence case, not shouting — e.g. "Get it on", never "GET IT ON".
      appStoreLine1 should not be appStoreLine1.toUpperCase(messages.lang.locale)
      playLine1     should not be playLine1.toUpperCase(messages.lang.locale)
    }
  }

  "DateFormatter" should "keep Polish byte-identical (genitive month)" in {
    val d = LocalDate.of(2026, 6, 4)
    DateFormatter.format(d, Locale.forLanguageTag("pl-PL")) shouldBe "Czwartek 4 czerwca"
  }

  it should "render other languages via their locale" in {
    val d = LocalDate.of(2026, 6, 4)
    DateFormatter.format(d, Locale.ENGLISH) shouldBe "Thursday 4 June"
  }
}
