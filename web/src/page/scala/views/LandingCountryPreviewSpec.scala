package views

import testsupport.TestMessages

import models.Country
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.i18n.Messages

/**
 * The `/` landing is the URL people paste into Facebook / Messenger / Slack, so
 * a non-Polish deployment's share preview must sell the RIGHT product: an
 * English card, the country's own host, and its English-poster home montage —
 * not Poland's `kinowo.net` / `og-home.jpg`. This pins the UK variant; the
 * default (Poland) variant is covered by `LandingPreviewMetaSpec`.
 *
 * The template and every partial it renders (`_ogTagsApp` included) take the
 * deployment's country as a parameter, so the spec simply hands it the UK — the
 * process's `KINOWO_COUNTRY` is read only by `AppLoader` and plays no part here.
 */
class LandingCountryPreviewSpec extends AnyFlatSpec with Matchers {

  private def renderUk(): String = {
    given Messages = TestMessages.forLang("en")
    views.html.landing(Country.UnitedKingdom, isApex = false).body
  }

  // Every page (including this one) embeds ALL FOUR languages' packs inline
  // (`#i18n-packs`, `controllers.I18nPacks`) for the client-side language
  // switch — inert JSON data, never rendered as visible text. A "this brand-
  // inappropriate word never appears" check has to look past that blob, or
  // the UK page would fail it just for carrying Poland's own Polish copy
  // (which legitimately names Filmweb) in the inert pack.
  private def visibleBody(html: String): String =
    html.replaceAll("(?s)<script id=\"i18n-packs\".*?</script>", "")

  "the UK landing preview" should "point og:image + twitter:image at the English home card on the UK host" in {
    val html = renderUk()
    html should include ("""<meta property="og:image"       content="https://showtimes.cc/uk/assets/img/og-home-uk.jpg">""")
    html should include ("""<meta name="twitter:image"       content="https://showtimes.cc/uk/assets/img/og-home-uk.jpg">""")
  }

  it should "carry the UK host as og:url and the Showtimes brand, in English" in {
    val html = renderUk()
    html should include ("""<meta property="og:url"         content="https://showtimes.cc/uk/">""")
    html should include ("""<meta property="og:site_name"   content="Showtimes">""")
    html should include ("""<meta property="og:title"       content="Showtimes — cinema listings in your city">""")
  }

  it should "describe the product without naming Filmweb (a Polish-only service the UK deployment lacks)" in {
    val html = renderUk()
    html should include ("IMDb, Rotten Tomatoes and Metacritic")
    visibleBody(html) should not include "Filmweb"
  }

  /** The UK's places are counties and regions, not cities — but they are WORDED
   *  as cities today (`PlaceKind.City`) and giving the US its own wording must
   *  not have moved a byte of theirs. */
  it should "keep exactly the city wording the UK ships today" in {
    val html = renderUk()
    html should include ("Choose your city")
    html should include ("Search for a city…")
    html should not include "Choose your state"
  }
}
