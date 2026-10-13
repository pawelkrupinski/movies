package views

import testsupport.TestMessages.given

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LandingViewSpec extends AnyFlatSpec with Matchers {

  private val picker = controllers.PickerFragments.rendered()

  private val html = views.html.landing(models.Country.Poland, isApex = false, picker).body

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
    views.html.landing(models.Country.UnitedStates, isApex = false, picker)(using testsupport.TestMessages.forLang("en")).body

  /** The opening tag of `slug`'s place link. */
  private def placeLink(html: String, slug: String): String =
    s"""<a class="picker-item" href="[^"]*/$slug/" data-slug="$slug"[^>]*>""".r.findFirstIn(html)
      .getOrElse(fail(s"no place link for $slug"))

  /** The heading row that drills into `label`'s level. */
  private def heading(label: String): String =
    s"""data-region="$label"><span class="picker-item-label">$label</span>"""

  "the US landing page" should "list every page, each under its state, as a real link to its page" in {
    models.Country.UnitedStates.cities.foreach(c => placeLink(usHtml, c.slug) should include (s"""href="https://showtimes.cc/us/${c.slug}/""""))
    usHtml should include (heading("California"))
    usHtml should include ("""<div class="picker-level" id="pick-us-california" data-region="California">""")
    placeLink(usHtml, "los-angeles")
    usHtml should include ("""<span class="picker-item-label">Los Angeles</span>""")
    // The state is a heading linking DOWN the page to its level, never a page of its own.
    usHtml should include ("""href="#pick-us-california" data-region="California"""")
    usHtml should not include """/texas/""""
    usHtml should not include "/poznan/"
  }

  it should "carry one level per state that holds several places, plus the root — and no other country's" in {
    // One screen of the drill-down each; the script shows one at a time. A state
    // that collapsed onto its single place is a row, not a level.
    """<div class="picker-level"""".r.findAllIn(usHtml).size shouldBe
      1 + models.Country.UnitedStates.cityGroups.count(_.soleCity.isEmpty)
    """data-country="([a-z]+)" data-base-url""".r.findAllMatchIn(usHtml).map(_.group(1)).toList shouldBe List("us")
  }

  it should "give every place what the picker's script reads: its coordinates" in {
    val la = models.Country.UnitedStates.cities.find(_.slug == "los-angeles").get
    placeLink(usHtml, "los-angeles") should include (s"""data-lat="${la.lat}" data-lon="${la.lon}"""")
  }

  it should "link a state that IS a place straight through, rather than heading a list of one" in {
    // The District of Columbia has one page, Washington: the picker links it
    // straight through, keeping the district's name searchable. See `CityGroup.soleCity`.
    placeLink(usHtml, "washington") should include ("""data-alias="District of Columbia"""")
    usHtml should not include heading("District of Columbia")
  }

  /** The UK picks a town page — London, Leeds, "Penrith area" — found under its
   *  county under its nation. */
  private val ukHtml =
    views.html.landing(models.Country.UnitedKingdom, isApex = false, picker)(using testsupport.TestMessages.forLang("en")).body

  "the UK landing page" should "list every county under its nation, and its places under the county" in {
    models.Country.UnitedKingdom.cities.foreach(c => placeLink(ukHtml, c.slug))
    ukHtml should include (heading("England"))
    ukHtml should include (heading("Scotland"))
    ukHtml should include (heading("Northern Ireland"))
    // The second level: a county heading inside its nation's level, with its own
    // level below, titled by both.
    ukHtml should include ("""data-region="Wales" data-subregion="Glamorgan"><span class="picker-item-label">Glamorgan</span>""")
    ukHtml should include ("""<p class="picker-level-title">Wales — Glamorgan</p>""")
    placeLink(ukHtml, "cardiff")
    // Neither level is a page.
    ukHtml should not include "/scotland/\""
    ukHtml should not include "/england/\""
    ukHtml should not include "/west-midlands/\""
  }

  it should "pull a county's single place up a level, so most of the list stays one tap deep" in {
    // A heading you open to find one row underneath is a tap that buys nothing.
    // Shetland holds Lerwick alone, Greater London London: only the counties
    // really arranging several cost a second tap. See `CityGroup.soleCity`.
    ukHtml should not include """data-subregion="Shetland""""
    ukHtml should not include """data-subregion="Greater London""""
    placeLink(ukHtml, "bristol") should not include "data-alias"   // Bristol over Bristol: nothing to add

    // …and the county's own name rides along so the search still finds the row
    // by it: dropping the heading saves a tap, it does not hide the county.
    placeLink(ukHtml, "lerwick") should include ("""data-alias="Shetland"""")
    placeLink(ukHtml, "london") should include ("""data-alias="Greater London"""")

    // West Midlands really groups several, so it keeps its heading.
    ukHtml should include ("""data-subregion="West Midlands"""")
  }

  it should "find a page by the name of every retired region that redirects to it" in {
    // The 76 Flicks regions retired into town pages (2026-10), and the three
    // merged before that, keep their names on the row their slug now 301s to —
    // inside a county that did NOT collapse, which is the case the collapsed
    // alias alone never reached.
    placeLink(ukHtml, "west-bromwich") should include ("""data-alias="Sandwell"""")
    placeLink(ukHtml, "inverness-highland") should include ("""data-alias="Highlands and Islands"""")
    placeLink(ukHtml, "lisburn") should include ("""data-alias="Lisburn and Castlereagh, Antrim, Armagh, Down"""")
    placeLink(ukHtml, "edinburgh") should include ("""data-alias="Edinburgh &amp; Lothians"""")
    // A collapsed county keeps its own name beside them.
    placeLink(ukHtml, "lerwick") should include ("""data-alias="Shetland"""")
  }

  /** Germany picks a town page — Köln, München, "Bayreuth und Umgebung" — found under its Bundesland. */
  private val deHtml =
    views.html.landing(models.Country.Germany, isApex = false, picker)(using testsupport.TestMessages.forLang("en")).body

  "the German landing page" should "list every page under its Bundesland" in {
    models.Country.Germany.cities.foreach(c => placeLink(deHtml, c.slug))
    deHtml should include (heading("Bayern"))
    deHtml should include (heading("Nordrhein-Westfalen"))
    deHtml should include ("""<span class="picker-item-label">Köln</span>""")
    placeLink(deHtml, "bayreuth")
    deHtml should include ("""<span class="picker-item-label">Bayreuth und Umgebung</span>""")
    // A Land is a heading, never a page.
    deHtml should not include "/bayern/\""
    // …except a city-state holding one page, which is pulled up.
    placeLink(deHtml, "hamburg")
    deHtml should not include heading("Hamburg")
    deHtml should include (heading("Saarland"))
    // One level, not the UK's two.
    deHtml should not include "data-subregion=\""
  }

  "a collapsed group's search alias" should "be HTML-escaped, not pasted into the attribute" in {
    // `data-alias` carries the group's own name, and a Bundesland's comes out of
    // the generated roster rather than being hand-authored — so nothing
    // upstream guarantees it holds no quote. Built as raw `Html(...)` the first
    // one to do so would close the attribute and inject whatever followed.
    val hostile = models.CityGroup(
      """Ruck" onmouseover=alert(1) x="""",
      "hostile",
      Seq(models.Country.UnitedKingdom.cities.head))
    val rendered = views.html._pickerPane("uk", "https://example.org", Seq(hostile), Nil).body
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

  it should "group Spain by province, a major city beside the towns around it" in {
    val esHtml = views.html.landing(models.Country.Spain, isApex = false, picker).body
    esHtml should include ("""<div class="picker-level" id="pick-es-""")
    placeLink(esHtml, "barcelona")
    esHtml should include ("""<span class="picker-item-label">Sitges y alrededores</span>""")
  }

  it should "group Poland by voivodeship, a major city beside the towns around it" in {
    html should include (heading("Wielkopolskie"))
    placeLink(html, "poznan") should include ("""href="https://kinowo.net/poznan/"""")
    html should include ("""<span class="picker-item-label">Turek i okolice</span>""")
  }

  // ── Every other country arrives after the page ─────────────────────────────

  "the landing" should "embed only the country it opens on, and link every other country's pane by its content version" in {
    // Poland's own pane is in the HTML; the US's 1,995 places are not.
    html should include ("""data-country="pl" data-base-url""")
    html should not include """data-country="us" data-base-url"""
    html should not include "/los-angeles/"
    models.Country.switchable.filterNot(_ == models.Country.Poland).foreach { c =>
      html should include (s""""${c.code}":"${picker.url(c.code)}"""")
    }
    picker.url("us") should startWith ("/api/picker/us?v=")
  }

  it should "no longer embed the whole catalog" in {
    html should not include "KINOWO_CATALOG"
    html should not include """"cities":["""
  }

  // ── The share preview ──────────────────────────────────────────────────────
  //
  // The bare `/` landing is the URL people actually paste into Facebook /
  // Messenger / Slack / X. The page itself is a city picker, but its share
  // preview must sell the product (repertoire + ratings), not show "Wybierz
  // miasto". These pin the Open Graph / Twitter card it emits — in particular
  // that og:image points at the national `og-home.jpg` repertoire card ("…w
  // Twoim mieście"), as opposed to a city index's own `og-{slug}.jpg`.

  "the landing preview" should "point og:image + twitter:image at the dedicated home card" in {
    html should include ("""<meta property="og:image"       content="https://kinowo.net/assets/img/og-home.jpg">""")
    html should include ("""<meta name="twitter:image"       content="https://kinowo.net/assets/img/og-home.jpg">""")
  }

  it should "use the large-image twitter card declared as 1200×630" in {
    html should include ("""<meta name="twitter:card"        content="summary_large_image">""")
    html should include ("""<meta property="og:image:width"  content="1200">""")
    html should include ("""<meta property="og:image:height" content="630">""")
  }

  it should "advertise the product in og:title, not the city picker" in {
    // The page body still reads "Wybierz miasto" (it IS the picker) — but the
    // share card's title must sell the repertoire instead.
    html should include ("""<meta property="og:title"       content="Kinowo — repertuar kin w Twoim mieście">""")
    html should not include """content="Wybierz miasto"""
  }

  it should "carry the canonical landing URL so Facebook keeps the card" in {
    html should include ("""<meta property="og:url"         content="https://kinowo.net/">""")
  }

  // The `showtimes.cc` bare apex (the brand front door — see `LandingController`)
  // is pasted into Facebook/Slack/X like any other landing URL, but it lists every
  // country rather than belonging to one. Its share card must not default to
  // Poland's Polish-language `og-home.jpg`: pin it at the US card instead, since
  // English + US posters is the closest thing this brand-neutral page has to a
  // representative image.
  private val apexHtml =
    views.html.landing(models.Country.default, isApex = true, picker)(using testsupport.TestMessages.forLang("en")).body

  "the front-door preview" should "point og:image + twitter:image at the US home card, not Poland's" in {
    apexHtml should include ("""<meta property="og:image"       content="https://showtimes.cc/assets/img/og-home-us.jpg">""")
    apexHtml should include ("""<meta name="twitter:image"       content="https://showtimes.cc/assets/img/og-home-us.jpg">""")
    apexHtml should not include "og-home.jpg\""
  }

  it should "advertise the brand in English, not Polish" in {
    apexHtml should include ("""<html lang="en"""")
  }

  // ── The picker's country names ─────────────────────────────────────────────
  //
  // The country pills are server-rendered links carrying `data-i18n="country.<code>"`,
  // so `applyLanguage` names them in the VISITOR's chosen language at runtime from
  // the embedded pack (`#i18n-packs`, `controllers.I18nPacks`) — never each
  // country's fixed native label (`Country.displayName`), which once made Germany's
  // pill say "Deutschland" everywhere. These assert the pack carries the right
  // mapping in every language and that every pill is wired to it. (The pack doesn't
  // depend on the rendering `Messages`, so `html`'s Polish render serves.)

  "the embedded language pack" should "carry Germany's name in every language, not just its native one" in {
    html should include (""""country.de":"Deutschland"""")
    html should include (""""country.de":"Germany"""")
    html should include (""""country.de":"Niemcy"""")
  }

  it should "carry every switchable country, keyed by its localized name in Spanish" in {
    models.Country.switchable.foreach { c =>
      html should include (s""""country.${c.code}":"${testsupport.TestMessages.forLang("es")("country." + c.code)}"""")
    }
  }

  "the country pills" should "each link to that country's own picker and relabel with the visitor's language" in {
    models.Country.switchable.foreach { c =>
      html should include (
        s"""href="${c.webUrl.get}/${controllers.LandingController.PickCityQuery}" data-country="${c.code}" data-i18n="country.${c.code}">""")
    }
    html should include ("""<a class="picker-country-pill active" href="https://kinowo.net/?pick=city" data-country="pl"""")
  }
}
