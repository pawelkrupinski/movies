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
    html should include ("img/favicon.png")
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

  it should "start every group SHUT — a <details> with no `open`, so 468 places aren't the first screen" in {
    // The grouping only earns its keep closed. Rendered open, the page is the
    // A-to-Z of 468 places the states were introduced to break up, with 55
    // headings added to it.
    // No `<details>` anywhere on the page carries `open` — matched as an
    // ATTRIBUTE rather than by a substring, so it cannot be satisfied or
    // defeated by the word appearing in a comment.
    """<details\b[^>]*\bopen\b[^>]*>""".r.findFirstIn(usHtml) shouldBe None
    // …and the disclosures really are there to be shut: one per state that did
    // not collapse onto its single place.
    """<details class="city-group">""".r.findAllIn(usHtml).size shouldBe
      models.Country.UnitedStates.cityGroups.count(_.soleCity.isEmpty)
  }

  it should "link a state that IS a place straight through, rather than heading a list of one" in {
    // Seven states and territories are too small to cut into metros, so the
    // state's own venue list is its page. See `CityGroup.soleCity`.
    usHtml should include ("""<li class="city-direct"><a href="/delaware/">Delaware</a></li>""")
    usHtml should not include """<summary class="city-group-label">Delaware</summary>"""
  }

  /** The UK picks a COUNTY — Cheshire, Kent — or one of the cities big enough to
   *  be a region of its own, found under its nation's heading. */
  private val ukHtml =
    views.html.landing(models.Country.UnitedKingdom)(using testsupport.TestMessages.forLang("en")).body

  "the UK landing page" should "list every county under its nation, and its places under the county" in {
    models.Country.UnitedKingdom.cities.foreach(c => ukHtml should include (s"/${c.slug}/"))
    ukHtml should include ("""<summary class="city-group-label">England</summary>""")
    ukHtml should include ("""<summary class="city-group-label">Scotland</summary>""")
    ukHtml should include ("""<summary class="city-group-label">Northern Ireland</summary>""")
    // The second level: a county heading, with its places inside it.
    ukHtml should include ("""<summary class="city-group-label">West Midlands</summary>""")
    ukHtml should include ("""<a href="/birmingham/">Birmingham</a>""")
    // Neither level is a link: `/scotland/` and `/west-midlands/` are not pages.
    ukHtml should not include """href="/scotland/""""
    ukHtml should not include """href="/england/""""
    ukHtml should not include """href="/west-midlands/""""
  }

  it should "pull a county's single place up a level, so most of the list stays one tap deep" in {
    // A heading you open to find one row underneath is a tap that buys nothing.
    // A Flicks region already IS Cheshire, so those two names agree; Merseyside
    // and Greater Manchester collapse onto names that differ. Only the counties
    // really arranging several cost a second tap — which is what keeps two
    // levels readable. See `CityGroup.soleCity`.
    ukHtml should include ("""<li class="city-direct"><a href="/cheshire/">Cheshire</a></li>""")
    ukHtml should not include """<summary class="city-group-label">Cheshire</summary>"""
    ukHtml should not include """<summary class="city-group-label">Greater Manchester</summary>"""
    ukHtml should not include """<summary class="city-group-label">Merseyside</summary>"""

    // …and the county's own name rides along so the search still finds the row
    // by it: dropping the heading saves a tap, it does not hide the county.
    ukHtml should include (
      """<li class="city-direct" data-alias="Merseyside"><a href="/liverpool/">Liverpool</a></li>""")
    ukHtml should include (
      """<li class="city-direct" data-alias="Greater Manchester"><a href="/manchester/">Manchester</a></li>""")
    // Nothing to remember where the two names already agree.
    ukHtml should not include """data-alias="Cheshire""""

    // West Midlands really groups three, so it keeps its heading.
    ukHtml should include ("""<summary class="city-group-label">West Midlands</summary>""")
  }

  /** Germany picks a REGION — Köln, München — found under its Bundesland. */
  private val deHtml =
    views.html.landing(models.Country.Germany)(using testsupport.TestMessages.forLang("en")).body

  "the German landing page" should "list every region under its Bundesland" in {
    models.Country.Germany.cities.foreach(c => deHtml should include (s"/${c.slug}/"))
    deHtml should include ("""<summary class="city-group-label">Bayern</summary>""")
    deHtml should include ("""<summary class="city-group-label">Nordrhein-Westfalen</summary>""")
    deHtml should include ("""<a href="/koeln/">Köln</a>""")
    // A Land is a heading, never a link.
    deHtml should not include """href="/bayern/""""
    // …except a Land holding one region, which is pulled up: Hamburg shares its
    // region's name, Saarland does not and keeps "Saarland" searchable.
    deHtml should include ("""<li class="city-direct"><a href="/hamburg/">Hamburg</a></li>""")
    deHtml should include (
      """<li class="city-direct" data-alias="Saarland"><a href="/saarbruecken/">Saarbrücken</a></li>""")
    deHtml should not include """<summary class="city-group-label">Saarland</summary>"""
    // One level, not the UK's two.
    deHtml should not include """<summary class="city-group-label">Saarbrücken</summary>"""
  }

  "a collapsed group's search alias" should "be HTML-escaped, not pasted into the attribute" in {
    // `data-alias` carries the group's own name, and a Bundesland's comes out of
    // the scraped `regions.json` rather than being hand-authored — so nothing
    // upstream guarantees it holds no quote. Built as raw `Html(...)` the first
    // one to do so would close the attribute and inject whatever followed.
    val hostile = models.CityGroup(
      """Ruck" onmouseover=alert(1) x="""",
      "hostile",
      Seq(models.Country.UnitedKingdom.cities.head))
    val rendered = views.html._cityPickerGroup(hostile).body
    rendered should include ("""data-alias="Ruck&quot; onmouseover=alert(1) x=&quot;"""")
    rendered should not include "onmouseover=alert(1) x=\"\">"
    // The attribute is still one attribute: exactly one unescaped quote pair
    // around it, so nothing after the label is read as markup.
    rendered.split("data-alias=").last.takeWhile(_ != '>') should not include "\" "
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
