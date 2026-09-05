package views

import testsupport.TestMessages.given

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LandingViewSpec extends AnyFlatSpec with Matchers {

  private val html = views.html.landing(models.Country.Poland).body

  "the city-selection landing page" should "render as a Polish HTML document listing the cities" in {
    html should include ("""<html lang="pl">""")
    html should include ("Wybierz miasto")
    models.Country.Poland.cities.foreach { c =>
      html should include (s"/${c.slug}/")
    }
  }

  it should "be crawlable — no robots noindex (it's the site's public entry point)" in {
    // `/` was once blocked from indexing by a
    // `<meta name="robots" content="noindex">`. The landing page is the
    // homepage; it must be indexable.
    html.toLowerCase should not include "noindex"
    html should not include """name="robots""""
  }

  it should "carry a meta description for search-result snippets" in {
    // The landing gets its description via _ogTagsApp, the same partial as the
    // other pages — without it search results have no snippet.
    html should include ("""<meta name="description"""")
    html should include ("Repertuar wszystkich kin w jednym miejscu")
  }

  it should "carry the Google Search Console verification tag" in {
    // `/` (this landing page) is what GSC fetches for the URL-prefix property
    // https://kinowo.net/. The meta tag predates owning the domain (fly.dev's
    // DNS belonged to Fly, so TXT verification was impossible); it is kept
    // because removing a verification a property still rests on un-verifies it.
    html should include (
      """<meta name="google-site-verification" content="GHV7eYMZc7PnJlXt03b8TU5ZsLib0pSDYOgIr08ifTE" />"""
    )
  }

  it should "declare a favicon so the browser doesn't 404 on /favicon.ico" in {
    html should include ("""rel="icon"""")
    html should include ("img/favicon.svg")
  }

  /** The US picks a METRO here — Los Angeles, Houston — found under its state's
   *  heading. The page renders under the English bundle that host serves. */
  private val usHtml =
    views.html.landing(models.Country.UnitedStates)(using testsupport.TestMessages.forLang("en")).body

  "the US landing page" should "list every metro, each under its state" in {
    models.Country.UnitedStates.cities.foreach(c => usHtml should include (s"/${c.slug}/"))
    usHtml should include ("""<details class="city-group">""")
    usHtml should include ("""<summary class="city-group-label">California</summary>""")
    usHtml should include ("""<a href="/los-angeles/">Los Angeles</a>""")
    // The state is a heading, never a link: `/california/` is not a page.
    usHtml should not include """href="/california/""""
    usHtml should not include "/poznan/"
  }

  it should "start every group SHUT — a <details> with no `open`, so 468 metros aren't the first screen" in {
    // The grouping only earns its keep closed. Rendered open, the page is the
    // A-to-Z of 468 metros the states were introduced to break up, with 55
    // headings added to it.
    usHtml should not include "<details class=\"city-group\" open"
    usHtml should not include "<details open"
  }

  it should "link a state that IS a place straight through, rather than heading a list of one" in {
    // Seven states and territories are too small to cut into metros, so the
    // state's own venue list is its page. See `CityGroup.soleCity`.
    usHtml should include ("""<li><a href="/delaware/">Delaware</a></li>""")
    usHtml should not include """<summary class="city-group-label">Delaware</summary>"""
  }

  /** The UK picks a COUNTY — Cheshire, Kent — or one of the cities big enough to
   *  be a region of its own, found under its nation's heading. */
  private val ukHtml =
    views.html.landing(models.Country.UnitedKingdom)(using testsupport.TestMessages.forLang("en")).body

  "the UK landing page" should "list every county, each under its nation" in {
    models.Country.UnitedKingdom.cities.foreach(c => ukHtml should include (s"/${c.slug}/"))
    ukHtml should include ("""<summary class="city-group-label">England</summary>""")
    ukHtml should include ("""<summary class="city-group-label">Scotland</summary>""")
    ukHtml should include ("""<summary class="city-group-label">Northern Ireland</summary>""")
    ukHtml should include ("""<a href="/cheshire/">Cheshire</a>""")
    ukHtml should include ("""<a href="/glasgow/">Glasgow</a>""")
    // A nation is a heading, never a link: `/scotland/` is not a page.
    ukHtml should not include """href="/scotland/""""
    ukHtml should not include """href="/england/""""
  }

  it should "ask for a city, like every other country" in {
    // A metro IS a city. The state-flavoured copy that shipped while the US
    // listed states has no reader left.
    usHtml should include ("Choose your city")
    usHtml should not include "Choose your state"
  }

  it should "stay one flat list for a country with no groups" in {
    html should not include """<details class="city-group">"""
    html should include ("""<li><a href="/poznan/">Poznań</a></li>""")
  }
}
