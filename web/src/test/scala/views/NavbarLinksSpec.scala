package views

import testsupport.TestMessages.given

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The navbar's links: no tab back to the film listing, and the filter panel's way to another city. */
class NavbarLinksSpec extends AnyFlatSpec with Matchers {

  private implicit val city: models.City = models.Poznan

  private def render(): String =
    views.html._navbar(oauthProviders = Set.empty, showImaxFilter = true).body

  "navbar partial" should "not render a Filmy nav link" in {
    // The Filmy pill was dropped from the navbar. The logo stays, but
    // there is no longer a tab linking back to the city's film listing.
    render() should not include ">Filmy<"
  }

  "filter panel" should "navigate to the unified `/` picker rather than a country select or an in-panel city select" in {
    // The old `#country-select` dropdown is gone entirely — picking a
    // country is now a step inside the `/` picker itself (`landing.scala.html`),
    // not a separate navbar control.
    val html = render()
    html should not include ("""id="country-select"""")
    html should include ("""onclick="goToCityPicker()"""")
    html should not include ("""id="city-select"""")
    html should not include ("""id="locate-city-btn"""")
  }
}
