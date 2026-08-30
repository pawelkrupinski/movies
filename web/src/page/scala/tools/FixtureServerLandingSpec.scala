package tools

import models.{City, Country}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * The one contract the Playwright suite depends on and could not state: the
 * fixture server's `/` lists EVERY city, across every country.
 *
 * No deployment renders that page. A Polish visitor is offered Polish cities, a
 * US visitor US states, and `landing.scala.html` defaults to exactly that. But
 * `page-tests-playwright` runs ONE fixture server for specs written against
 * four countries — `city-select.spec.ts` clicks Poznań, München and California
 * on this single page — so the harness passes the union explicitly, and three
 * quarters of that suite has nothing to click the moment it stops.
 *
 * WHY IT IS WORTH A SPEC OF ITS OWN. That is precisely what happened on
 * 2026-08-30: `FixtureServerMain` was narrowed from `City.all` to one country as
 * part of an unrelated (and correct) change to the landing's copy. It compiled,
 * and every Scala layer stayed green — the union was asserted nowhere in this
 * repository except a browser spec several CI shards away, which failed as
 * `Test timeout of 30000ms exceeded` waiting to click a `California` link that
 * no longer existed. Nothing in that message points at a fixture server.
 *
 * So this asserts the contract where it is cheap: no browser, no server, just
 * the string the harness would serve.
 */
class FixtureServerLandingSpec extends AnyFlatSpec with Matchers {

  private val html = FixtureServerMain.landingHtml

  private val rows = """<li><a href="/([^/"]+)/">""".r
    .findAllMatchIn(html).map(_.group(1)).toList

  "the fixture server's landing" should "offer every city of every country, not just the default one's" in {
    rows should contain theSameElementsAs City.all.map(_.slug)
    // Said twice on purpose: the count is what a reader compares against the
    // number in city-select.spec.ts, and the set is what actually holds.
    rows.size shouldBe City.all.size
    rows.size should be > Country.default.allSorted.size
  }

  it should "reach a city from each country a browser spec clicks on" in {
    // One per country the Playwright suite navigates from this page. Slugs, not
    // labels, because the label is the half that the copy change legitimately
    // moves around.
    rows should contain allOf ("poznan", "london", "berlin", "california")
  }

  it should "still read as the default country, which is where its copy comes from" in {
    // The seam the harness overrides is the LIST and nothing else. The specs
    // read Polish nouns off the pages they land on ("133 kin", not "133
    // cinemas"), so a fixture server that also switched country would break
    // them in a second, quieter way.
    html should include(s"""<html lang="${Country.default.language.getLanguage}"""")
    html should include(Country.default.brandName)
  }
}
