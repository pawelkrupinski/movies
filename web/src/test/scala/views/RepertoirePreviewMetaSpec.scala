package views

import models.{Poznan, Wroclaw}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import testsupport.TestMessages.given

// The `/{city}/` index is the URL people share for a city. Its preview card is
// a per-city, server-generated `og-{slug}.png` ("Repertuar kin w {locative}",
// see `tools.OgCardGenerator`) — NOT the generic national `og-home.png`. This
// spec pins that each city index points og:image / twitter:image at its own
// slug's card.
class RepertoirePreviewMetaSpec extends AnyFlatSpec with Matchers {

  private def render(city: models.City): String = {
    implicit val c: models.City = city
    views.html.repertoire(
      films = Nil, allCinemas = Nil, cinemaPills = Map.empty,
      devMode = false, currentUser = None, oauthProviders = Set.empty,
    ).body
  }

  "the city index preview" should "point og:image + twitter:image at the city's own card" in {
    val html = render(Poznan)
    html should include ("""content="https://kinowo.fly.dev/assets/img/og-poznan.png"""")
    // og:image AND twitter:image both carry it.
    html.sliding("og-poznan.png".length).count(_ == "og-poznan.png") should be >= 2
  }

  it should "use a different card per city (not a shared national image)" in {
    render(Poznan) should include ("og-poznan.png")
    render(Wroclaw) should include ("og-wroclaw.png")
    render(Poznan) should not include "og-home.png"
    render(Poznan) should not include "og-image.png"
  }
}
