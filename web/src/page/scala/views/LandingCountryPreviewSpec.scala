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
 * not Poland's `kinowo.fly.dev` / `og-home.png`. This pins the UK variant
 * (`KINOWO_COUNTRY=uk`); the default (Poland) variant is covered by
 * `LandingPreviewMetaSpec`.
 *
 * Lives in `PageTest` (not the main unit `Test`) on purpose: forcing
 * `KINOWO_COUNTRY` mutates a process-global that `Country.fromEnv` reads, which
 * would race parallel unit suites — but `PageTest` runs unforked and
 * `parallelExecution := false`, so the set/clear window can't leak into a
 * concurrent reader.
 */
class LandingCountryPreviewSpec extends AnyFlatSpec with Matchers {

  private def renderUk(): String = {
    given Messages = TestMessages.forLang("en")
    // System property is the second source Env consults (after the real env
    // var), so it only takes effect when nothing already exports KINOWO_COUNTRY.
    val prev = System.getProperty("KINOWO_COUNTRY")
    try {
      System.setProperty("KINOWO_COUNTRY", "uk")
      Country.fromEnv shouldBe Country.UnitedKingdom   // guard: a stray env var didn't win
      views.html.landing(Country.UnitedKingdom.allSorted).body
    } finally {
      if (prev == null) System.clearProperty("KINOWO_COUNTRY") else System.setProperty("KINOWO_COUNTRY", prev)
    }
  }

  // A real KINOWO_COUNTRY env var (e.g. a dev shell that exports one) wins over
  // the property, so skip rather than assert against the wrong country.
  private val envCountryForced = System.getenv("KINOWO_COUNTRY") != null

  "the UK landing preview" should "point og:image + twitter:image at the English home card on the UK host" in {
    if (envCountryForced) cancel("KINOWO_COUNTRY is set in the environment; property override can't take effect")
    val html = renderUk()
    html should include ("""<meta property="og:image"       content="https://showtimes-uk.fly.dev/assets/img/og-home-uk.png">""")
    html should include ("""<meta name="twitter:image"       content="https://showtimes-uk.fly.dev/assets/img/og-home-uk.png">""")
  }

  it should "carry the UK host as og:url and the Showtimes brand, in English" in {
    if (envCountryForced) cancel("KINOWO_COUNTRY is set in the environment; property override can't take effect")
    val html = renderUk()
    html should include ("""<meta property="og:url"         content="https://showtimes-uk.fly.dev/">""")
    html should include ("""<meta property="og:site_name"   content="Showtimes">""")
    html should include ("""<meta property="og:title"       content="Showtimes — cinema listings in your city">""")
  }

  it should "describe the product without naming Filmweb (a Polish-only service the UK deployment lacks)" in {
    if (envCountryForced) cancel("KINOWO_COUNTRY is set in the environment; property override can't take effect")
    val html = renderUk()
    html should include ("IMDb, Rotten Tomatoes and Metacritic")
    html should not include "Filmweb"
  }
}
