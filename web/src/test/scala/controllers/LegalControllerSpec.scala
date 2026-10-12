package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}

/** `/privacy-policy?lang=…` serves the policy in the language the LINK asks for,
 *  not the one the deployment happens to run in. The App Store and Play listings
 *  point one URL per locale at this endpoint, and all three resolve against the
 *  same deployment — so a German store listing has to be able to get German
 *  prose out of a Polish-configured host.
 */
class LegalControllerSpec extends AnyFlatSpec with Matchers {

  private val controller = new LegalController(Helpers.stubControllerComponents(), models.Country.Poland)

  private def policy(lang: Option[String]): String =
    contentAsString(controller.privacy(lang).apply(FakeRequest("GET", "/privacy-policy")))

  "the privacy policy" should "render Polish when asked for pl" in {
    val html = policy(Some("pl"))
    html should include ("""<html lang="pl"""")
    html should include ("Polityka prywatności")
    html should include ("Administratorem danych")
  }

  it should "render English when asked for en" in {
    val html = policy(Some("en"))
    html should include ("""<html lang="en"""")
    html should include ("Privacy policy")
    html should include ("data controller")
  }

  it should "render German when asked for de" in {
    val html = policy(Some("de"))
    html should include ("""<html lang="de"""")
    html should include ("Datenschutzerklärung")
    html should include ("Verantwortlicher")
  }

  it should "render Spanish when asked for es" in {
    val html = policy(Some("es"))
    html should include ("""<html lang="es"""")
    html should include ("Política de privacidad")
    html should include ("responsable del tratamiento")
  }

  // Every language the site offers (the navbar picker's `LanguageNames.Codes`)
  // gets a COMPLETE policy in that language — adding a language to the picker
  // without translating the policy fails here, instead of the new language's
  // store listings quietly getting English. The English page also used to be a
  // stub that said "the full text is Polish only", which is not something an
  // app store will accept as a privacy policy.
  it should "carry the full policy in every language the site offers, not a stub or a fallback" in {
    for (lang <- LanguageNames.Codes) withClue(s"lang=$lang: ") {
      val html = policy(Some(lang))
      html should include (s"""<html lang="$lang"""")
      // 8 numbered sections, a contact address and a last-updated date.
      for (section <- 1 to 8) html should include (s">$section.")
      html should include ("pawel.krupinski@gmail.com")
      html should not include "Polish only"
    }
  }

  // A bare /privacy-policy (an old link, or a person typing it) still has to
  // answer — with whatever language this deployment serves.
  it should "fall back to the deployment's own language when no lang is given" in {
    val german = new LegalController(Helpers.stubControllerComponents(), models.Country.Germany)
    val html = contentAsString(german.privacy(None)(FakeRequest(GET, "/privacy-policy")))
    html should include ("""<html lang="de"""")
    html should include ("Showtimes")
  }

  it should "answer a bare link on the Spanish deployment in Spanish" in {
    val spain = new LegalController(Helpers.stubControllerComponents(), models.Country.Spain)
    contentAsString(spain.privacy(None)(FakeRequest(GET, "/privacy-policy"))) should include ("""<html lang="es"""")
  }

  it should "fall back for a language we don't publish rather than 404" in {
    val html = policy(Some("fr"))
    html should include ("""<html lang=""")
    html should include ("pawel.krupinski@gmail.com")
  }

  // Retired 2026-10-12: no registration points here any more — Meta's dashboard
  // and Google's consent screen carry /privacy-policy (or nothing), and the
  // only hits in six weeks of access logs were 95 crawler requests. With the
  // route gone the path falls to the `/:city` catch-all, which 404s an unknown
  // slug (see CityTrailingSlashRedirectSpec).
  "the routes file" should "no longer serve the pre-i18n /polityka-prywatnosci path" in {
    val source = scala.io.Source.fromInputStream(getClass.getResourceAsStream("/routes"))
    val paths  = try source.getLines().filter(_.startsWith("GET ")).map(_.split("\\s+")(1)).toList finally source.close()
    paths should contain ("/privacy-policy")
    paths should not contain "/polityka-prywatnosci"
  }
}
