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

  private val controller = new LegalController(Helpers.stubControllerComponents())

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

  // Every language is a COMPLETE policy — the English page used to be a stub
  // that said "the full text is Polish only", which is not something an app
  // store will accept as a privacy policy.
  it should "carry the full section set in every language, not a stub" in {
    for (lang <- Seq("pl", "en", "de")) withClue(s"lang=$lang: ") {
      val html = policy(Some(lang))
      // 8 numbered sections, a contact address and a last-updated date.
      for (section <- 1 to 8) html should include (s">$section.")
      html should include ("pawel.krupinski@gmail.com")
      html should not include "Polish only"
    }
  }

  // A bare /privacy-policy (an old link, or a person typing it) still has to
  // answer — with whatever language this deployment serves.
  it should "fall back to the deployment's own language when no lang is given" in {
    val html = policy(None)
    val expected = models.Country.fromEnv.language.getLanguage
    html should include (s"""<html lang="$expected"""")
  }

  it should "fall back for a language we don't publish rather than 404" in {
    val html = policy(Some("fr"))
    html should include ("""<html lang=""")
    html should include ("pawel.krupinski@gmail.com")
  }
}
