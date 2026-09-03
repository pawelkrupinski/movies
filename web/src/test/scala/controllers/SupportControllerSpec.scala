package controllers

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.test.Helpers._
import play.api.test.{FakeRequest, Helpers}

/** `/support?lang=…` is the App Store's required support URL — one per listing
 *  locale, all resolving against whichever deployment serves that country. It
 *  therefore has to hand back Spanish prose from a Polish-configured host, the
 *  same way `/privacy-policy` does, and every language has to be a real page
 *  with a contact route on it: Apple rejects a support URL that only shows
 *  product marketing.
 */
class SupportControllerSpec extends AnyFlatSpec with Matchers {

  private val controller = new SupportController(Helpers.stubControllerComponents())

  private def support(lang: Option[String]): String =
    contentAsString(controller.support(lang).apply(FakeRequest("GET", "/support")))

  private val published = Seq("pl", "en", "de", "es")

  it should "render Polish when asked for pl" in {
    val html = support(Some("pl"))
    html should include ("""<html lang="pl"""")
    html should include ("Pomoc")
  }

  it should "render English when asked for en" in {
    val html = support(Some("en"))
    html should include ("""<html lang="en"""")
    html should include ("Support")
  }

  it should "render German when asked for de" in {
    val html = support(Some("de"))
    html should include ("""<html lang="de"""")
    html should include ("Hilfe")
  }

  // Spain became an App Store territory alongside Poland, Germany, the UK and
  // the US, so es is a PUBLISHED language here, not an English fallback.
  it should "render Spanish when asked for es" in {
    val html = support(Some("es"))
    html should include ("""<html lang="es"""")
    html should include ("Ayuda")
  }

  // The whole point of the page: a reviewer (and a user) must find a way to
  // reach a human on it, in the language they opened it in.
  it should "carry a reachable contact address in every language" in {
    for (lang <- published) withClue(s"lang=$lang: ") {
      val html = support(Some(lang))
      html should include (s"mailto:${LegalContact.Email}")
      html should include (LegalContact.Email)
    }
  }

  // Apple's support URL is checked by a human who expects answers, not a link
  // back to the listing. Every language carries the same question set.
  it should "answer the common questions in every language, not just link out" in {
    for (lang <- published) withClue(s"lang=$lang: ") {
      val html = support(Some(lang))
      for (section <- 1 to 6) html should include (s">$section.")
    }
  }

  it should "point every language at the privacy policy in that same language" in {
    for (lang <- published) withClue(s"lang=$lang: ") {
      support(Some(lang)) should include (s"/privacy-policy?lang=$lang")
    }
  }

  // A bare /support — an old link, or someone typing it — still has to answer.
  it should "fall back to the deployment's own language when no lang is given" in {
    val html = support(None)
    val expected = models.Country.fromEnv.language.getLanguage
    html should include (s"""<html lang="$expected"""")
  }

  it should "fall back for a language we don't publish rather than 404" in {
    val html = support(Some("fr"))
    html should include ("""<html lang=""")
    html should include (LegalContact.Email)
  }
}
