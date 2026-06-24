package views

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LandingViewSpec extends AnyFlatSpec with Matchers {

  private val html = views.html.landing(models.City.all).body

  "the city-selection landing page" should "render as a Polish HTML document listing the cities" in {
    html should include ("""<html lang="pl">""")
    html should include ("Wybierz miasto")
    models.City.all.foreach { c =>
      html should include (s"/${c.slug}/")
    }
  }

  it should "be crawlable — no robots noindex (it's the site's public entry point)" in {
    // Lighthouse SEO flagged `/` as blocked from indexing because of a
    // `<meta name="robots" content="noindex">`. The landing page is the
    // homepage; it must be indexable.
    html.toLowerCase should not include "noindex"
    html should not include """name="robots""""
  }

  it should "carry a meta description for search-result snippets" in {
    // The landing now gets its description via _ogTagsApp (same partial as the
    // other pages), which also covers the Lighthouse SEO "missing description".
    html should include ("""<meta name="description"""")
    html should include ("Repertuar wszystkich kin w jednym miejscu")
  }

  it should "carry the Google Search Console verification tag" in {
    // `/` (this landing page) is what GSC fetches for the URL-prefix property
    // https://kinowo.fly.dev/ — DNS TXT verification is impossible because
    // fly.dev's DNS is owned by Fly.io, so we verify via this HTML meta tag.
    html should include (
      """<meta name="google-site-verification" content="GHV7eYMZc7PnJlXt03b8TU5ZsLib0pSDYOgIr08ifTE" />"""
    )
  }

  it should "declare a favicon so the browser doesn't 404 on /favicon.ico" in {
    html should include ("""rel="icon"""")
    html should include ("img/favicon.svg")
  }
}
