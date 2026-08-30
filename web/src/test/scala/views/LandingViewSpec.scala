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

  /** The US picks a STATE on this screen — its `City` objects are California,
   *  Texas, … — so every "city" in the copy was a small lie told to every US
   *  visitor. The page renders under the English bundle that host serves. */
  private val usHtml =
    views.html.landing(models.Country.UnitedStates)(using testsupport.TestMessages.forLang("en")).body

  "the US landing page" should "ask for a state, never a city" in {
    usHtml should include ("Choose your state or territory")
    usHtml should include ("Search for a state…")
    usHtml should include ("Search for a state or territory")   // the input's aria-label
    usHtml should include ("No state or territory by that name.")
    usHtml should include ("No supported state nearby")
    usHtml should include ("<title>Showtimes — cinema listings in your state</title>")
    // The exact strings this page used to show a Texan.
    usHtml should not include "Choose your city"
    usHtml should not include "Search for a city"
    usHtml should not include "No city by that name."
    usHtml should not include "No supported city nearby"
  }

  it should "list the states themselves, not another country's cities" in {
    models.Country.UnitedStates.cities.foreach(c => usHtml should include (s"/${c.slug}/"))
    usHtml should not include "/poznan/"
  }
}
