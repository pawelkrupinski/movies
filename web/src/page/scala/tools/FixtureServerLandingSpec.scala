package tools

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The contract the Playwright suite depends on and could not state: which places
 * each of the fixture server's landing paths actually offers.
 *
 * WHY IT IS WORTH A SPEC OF ITS OWN. On 2026-08-30 `FixtureServerMain`'s `/` was
 * narrowed as part of an unrelated (and correct) change. It compiled, every Scala
 * layer stayed green — the contract was asserted nowhere in this repository
 * except a browser spec several CI shards away — and it surfaced as
 * `Test timeout of 30000ms exceeded` waiting to click a link that no longer
 * existed. Nothing in that message points at a fixture server.
 *
 * The spec written to prevent that was then defeated by the same class of drift:
 * it called a `renderLanding()` helper that only IT called, while the route
 * built its own string. Narrowing the route left the spec green over a page
 * nothing served. So this now reads `FixtureServerMain.landings()` — the same map
 * the route table indexes — and there is no second copy left to diverge from.
 *
 * No browser, no server: just the strings the harness would serve.
 */
class FixtureServerLandingSpec extends AnyFlatSpec with Matchers {

  private val landings = FixtureServerMain.landings()

  /** The place slugs a landing offers, in render order. */
  private def rows(path: String): List[String] = {
    val html = landings.getOrElse(path, fail(s"the fixture server serves no $path"))
    """<a href="/([^/"]+)/">""".r.findAllMatchIn(html).map(_.group(1)).toList
  }

  "the fixture server's `/`" should "offer the default country's own list, exactly as a deployment does" in {
    // Poland's 41, and NOT the union across countries. `city-select.spec.ts`
    // counts them, and the geolocation redirect it also drives is a claim about
    // where the visitor IS — offered every country's places, a Poznań fix would
    // answer with whichever of five happened to be nearest.
    rows("/") should contain theSameElementsAs Country.default.cities.map(_.slug)
    rows("/") should not contain "london"
    rows("/") should not contain "los-angeles"
  }

  it should "serve a landing for each country a browser spec picks a place on" in {
    // Slugs, not labels: the label is the half a copy change legitimately moves.
    rows("/landing-us") should contain theSameElementsAs Country.UnitedStates.cities.map(_.slug)
    rows("/landing-uk") should contain theSameElementsAs Country.UnitedKingdom.cities.map(_.slug)
    rows("/landing-de") should contain theSameElementsAs Country.Germany.cities.map(_.slug)

    // The ones the Playwright specs actually click, named so a failure here says
    // which page lost them rather than timing out in a browser.
    rows("/landing-us") should contain allOf ("los-angeles", "delaware")
    rows("/landing-uk") should contain allOf ("cheshire", "birmingham", "liverpool")
    rows("/landing-de") should contain allOf ("koeln", "muenchen", "hamburg")
  }

  it should "keep each landing to ONE country, so a place is never offered by the wrong one" in {
    // A grouped landing that leaked another country's places would let a spec
    // click a row whose `/{slug}/` the deployment under test does not serve.
    rows("/landing-us") should not contain "poznan"
    rows("/landing-uk") should not contain "los-angeles"
    rows("/landing-de") should not contain "london"
  }

  it should "offer a GROUPED country every place it has, not just the ones a heading collapsed onto" in {
    // The nesting is what this could silently lose: a bug in `_cityPickerGroup`'s
    // recursion, or a group that stopped being rendered, drops rows the count
    // catches and the spot-checks above would not.
    rows("/landing-uk") should have size Country.UnitedKingdom.cities.size
    rows("/landing-de") should have size Country.Germany.cities.size
    rows("/landing-us") should have size Country.UnitedStates.cities.size
    // …and none of them twice, which a mis-recursion would do.
    rows("/landing-uk").distinct should have size rows("/landing-uk").size
    rows("/landing-us").distinct should have size rows("/landing-us").size
  }

  it should "still read as the default country, which is where its copy comes from" in {
    // The specs read Polish nouns off the pages they land on ("133 kin", not
    // "133 cinemas"), so a harness that also switched country would break them in
    // a second, quieter way.
    landings("/") should include(s"""<html lang="${Country.default.language.getLanguage}"""")
    landings("/") should include(Country.default.brandName)
  }
}
