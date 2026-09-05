package views

import models.{Poznan, Wroclaw}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import testsupport.TestMessages.given

// The `/{city}/` index is the URL people share for a city. Its preview card is
// a per-city, server-generated `og-{slug}.jpg` ("Repertuar kin w {locative}",
// see `tools.OgCardGenerator`) — NOT the generic national `og-home.jpg`. This
// spec pins that each city index points og:image / twitter:image at its own
// slug's card.
class RepertoirePreviewMetaSpec extends AnyFlatSpec with Matchers {

  private def render(city: models.City): String = {
    implicit val c: models.City = city
    views.html.repertoire(
      films = Nil, allCinemas = Nil, cinemaPills = Map.empty,
      devMode = false, oauthProviders = Set.empty,
      renderedAt = java.time.LocalDateTime.of(2026, 6, 8, 0, 0),
    ).body
  }

  "the city index preview" should "point og:image + twitter:image at the city's own card" in {
    val html = render(Poznan)
    html should include ("""content="https://kinowo.net/assets/img/og-poznan.jpg"""")
    // og:image AND twitter:image both carry it.
    html.sliding("og-poznan.jpg".length).count(_ == "og-poznan.jpg") should be >= 2
  }

  it should "use a different card per city (not a shared national image)" in {
    render(Poznan) should include ("og-poznan.jpg")
    render(Wroclaw) should include ("og-wroclaw.jpg")
    render(Poznan) should not include "og-home.jpg"
    render(Poznan) should not include "og-image.png"
  }
}
