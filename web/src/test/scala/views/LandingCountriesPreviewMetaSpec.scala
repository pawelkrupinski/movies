package views

import testsupport.TestMessages

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// The `showtimes.cc` bare apex (the brand front door — see `LandingController`)
// is pasted into Facebook/Slack/X like any other landing URL, but it lists every
// country rather than belonging to one. Its share card must not default to
// Poland's Polish-language `og-home.jpg`: pin it at the US card instead, since
// English + US posters is the closest thing this brand-neutral page has to a
// representative image.
class LandingCountriesPreviewMetaSpec extends AnyFlatSpec with Matchers {

  private def render(): String =
    views.html.landing(Country.default, isApex = true)(using TestMessages.forLang("en")).body

  "the front-door preview" should "point og:image + twitter:image at the US home card, not Poland's" in {
    val html = render()
    html should include ("""<meta property="og:image"       content="https://showtimes.cc/assets/img/og-home-us.jpg">""")
    html should include ("""<meta name="twitter:image"       content="https://showtimes.cc/assets/img/og-home-us.jpg">""")
    html should not include "og-home.jpg\""
  }

  it should "advertise the brand in English, not Polish" in {
    val html = render()
    html should include ("""<html lang="en"""")
  }
}
