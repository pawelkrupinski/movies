package views

import testsupport.TestMessages.given

import models.City
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// The bare `/` landing is the URL people actually paste into Facebook /
// Messenger / Slack / X. The page itself is a city picker, but its share
// preview must sell the product (repertoire + ratings), not show "Wybierz
// miasto". This spec pins the Open Graph / Twitter card it emits — in
// particular that og:image points at the national `og-home.png` repertoire
// card ("…w Twoim mieście"), as opposed to a city index's own `og-{slug}.png`.
class LandingPreviewMetaSpec extends AnyFlatSpec with Matchers {

  private def render(): String = views.html.landing(City.allSorted).body

  "the landing preview" should "point og:image + twitter:image at the dedicated home card" in {
    val html = render()
    html should include ("""<meta property="og:image"       content="https://kinowo.fly.dev/assets/img/og-home.png">""")
    html should include ("""<meta name="twitter:image"       content="https://kinowo.fly.dev/assets/img/og-home.png">""")
  }

  it should "use the large-image twitter card declared as 1200×630" in {
    val html = render()
    html should include ("""<meta name="twitter:card"        content="summary_large_image">""")
    html should include ("""<meta property="og:image:width"  content="1200">""")
    html should include ("""<meta property="og:image:height" content="630">""")
  }

  it should "advertise the product in og:title, not the city picker" in {
    val html = render()
    // The page body still reads "Wybierz miasto" (it IS the picker) — but the
    // share card's title must sell the repertoire instead.
    html should include ("""<meta property="og:title"       content="Kinowo — repertuar kin w Twoim mieście">""")
    html should not include """content="Wybierz miasto"""
  }

  it should "carry the canonical landing URL so Facebook keeps the card" in {
    val html = render()
    html should include ("""<meta property="og:url"         content="https://kinowo.fly.dev/">""")
  }
}
