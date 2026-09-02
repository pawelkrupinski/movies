package views

import testsupport.TestMessages.given

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The Filtry panel's "version" radios filter on a LITERAL `Showtime.format`
 * token, and the token a subtitled or dubbed screening carries is the country's
 * own: `NAP`/`DUB` in Poland, `VOSE`/`DOB` in Spain, `OmU`/`DF` in Germany.
 *
 * They were hardcoded to Poland's pair for every deployment, so every country
 * but Poland shipped two radios that could only ever match nothing.
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

  // Britain and America subtitle rather than dub, and their chains' labels
  // normalise to `SUB`/`DUB` — 13,341 subtitled screenings across the two on
  // 2026-09-02, so the row belongs there too.
  it should "filter on SUB/DUB in the English-speaking countries" in {
    for (country <- Seq(models.Country.UnitedKingdom, models.Country.UnitedStates)) {
      val html = render(cityIn(country))
      withClue(s"${country.code}: ") {
        html should include ("""name="format-lang" value="SUB"""")
        html should include ("""name="format-lang" value="DUB"""")
        html should not include """value="NAP""""
      }
    }
  }

  // The row is conditional, not unconditional: a country that marked neither
  // version would get no section at all rather than a filter with no matching
  // badge anywhere on the page. No country is in that position today, so the
  // branch is exercised through the model.
  "a country that marks neither version" should "render no version row" in {
    models.Country.all.filter(_.versionTokens.isEmpty) shouldBe empty
    render(models.Poznan) should include ("format-lang")
  }
}
