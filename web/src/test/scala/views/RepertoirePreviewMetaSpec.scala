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

  private def render(city: models.City, pinnedToday: Option[java.time.LocalDate] = None): String = {
    implicit val c: models.City = city
    views.html.repertoire(
      films = Nil, allCinemas = Nil, cinemaPills = Map.empty,
      devMode = false, minifier = tools.Minify, oauthProviders = Set.empty,
      renderedAt = java.time.LocalDateTime.of(2026, 6, 8, 0, 0), pinnedToday = pinnedToday,
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

  // The client's "today" is pinned only by a render that is HANDED a date — a fixture render
  // passing its corpus's capture day. Production passes none, whatever any JVM property says.
  "the page's client-side today" should "be pinned only to a date the render is handed" in {
    render(Poznan, pinnedToday = Some(java.time.LocalDate.of(2026, 6, 8))) should include (
      """window.KINOWO_PINNED_TODAY = "2026-06-08";""")
    render(Poznan) should not include "KINOWO_PINNED_TODAY"
  }
}
