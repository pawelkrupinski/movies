package views

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class PrivacyViewSpec extends AnyFlatSpec with Matchers {

  private val html = views.html.privacy().body

  "the privacy policy page" should "render as a Polish HTML document" in {
    html should include ("""<html lang="pl">""")
    html should include ("Polityka prywatności")
  }

  it should "name the data controller and a contact email (required by RODO and Meta)" in {
    html should include ("Paweł Krupiński")
    html should include ("pawel.krupinski@gmail.com")
  }

  it should "describe the data deletion path (the second Meta go-live requirement)" in {
    html should include ("usunąć swoje konto")
    html should include ("Po usunięciu konta")
  }

  it should "list the core RODO data-subject rights" in {
    html should include ("RODO")
    html should include ("PUODO")
  }
}
