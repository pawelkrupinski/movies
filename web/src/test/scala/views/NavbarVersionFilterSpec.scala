package views

import testsupport.TestMessages.given

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The Filtry panel's "version" radios filter on a LITERAL `Showtime.format`
 * token, and the token a subtitled or dubbed screening carries is the country's
 * own: `NAP`/`DUB` in Poland, `VOSE`/`DOB` in Spain, `OmU`/`DF` in Germany.
 *
 * They were hardcoded to Poland's pair for every deployment, so Spain and
 * Germany shipped two radios that could only ever match nothing — and the
 * English-speaking countries, whose films all screen in one language and whose
 * scrapers mark neither version, got a whole section that filtered to an empty
 * page.
 */
class NavbarVersionFilterSpec extends AnyFlatSpec with Matchers {

  private def render(city: models.City): String = {
    given models.City = city
    views.html._navbar(activePage = "films", devMode = false,
      currentUser = None, oauthProviders = Set.empty).body
  }

  private def cityIn(country: models.Country): models.City = country.cities.head

  "the version radios" should "filter on Poland's own subtitled/dubbed tokens" in {
    val html = render(models.Poznan)
    html should include ("""name="format-lang" value="NAP"""")
    html should include ("""name="format-lang" value="DUB"""")
  }

  it should "filter on Spain's tokens under a Spanish city" in {
    val html = render(cityIn(models.Country.Spain))
    html should include ("""name="format-lang" value="VOSE"""")
    html should include ("""name="format-lang" value="DOB"""")
    html should not include """value="NAP""""
  }

  it should "filter on Germany's tokens under a German city" in {
    val html = render(cityIn(models.Country.Germany))
    html should include ("""name="format-lang" value="OmU"""")
    html should include ("""name="format-lang" value="DF"""")
    html should not include """value="NAP""""
  }

  // Nothing marks a version in the UK or the US, so the row is left out rather
  // than offering a filter with no matching badge anywhere on the page.
  it should "be left out entirely for a country that marks neither version" in {
    for (country <- Seq(models.Country.UnitedKingdom, models.Country.UnitedStates)) {
      val html = render(cityIn(country))
      html should not include "format-lang"
      // …while the axes that DO apply everywhere are untouched.
      html should include ("""name="format-dim"""")
      html should include ("""id="format-imax"""")
    }
  }
}
