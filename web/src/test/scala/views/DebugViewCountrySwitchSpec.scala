package views

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.movies.SingleCountryNormalizer.titleNormalizer

/**
 * Every debug view is implicitly scoped to the ONE country this deployment
 * serves (its own Mongo db). The debug navbar surfaces that dimension: it names
 * the current country and — since more than one country is deployed
 * (`Country.switchable`) — offers a switch to the SAME debug page on the other
 * country's host. Mirrors the main navbar's country switcher.
 */
class DebugViewCountrySwitchSpec extends AnyFlatSpec with Matchers {

  // The debug page under test is served under Poznań (a Polish city).
  private implicit val city: models.City = models.Poznan

  "debug navbar" should "offer a switch to another country's debug page on that country's host" in {
    val html = views.html.debug(Seq.empty, titleNormalizer).body
    html should include ("""class="debug-nav-country"""")
    // The UK deployment's corpus debug page, on its own host.
    html should include ("""value="https://uk.showtimes.cc/debug"""")
  }

  it should "mark this deployment's own country as the selected option" in {
    val html = views.html.debug(Seq.empty, titleNormalizer).body
    // KINOWO_COUNTRY unset in tests → Poland; its option is pre-selected.
    html should include ("""value="https://kinowo.net/debug" selected""")
  }

  "cadence navbar" should "keep the switcher pointed at the cadence page, not the corpus page" in {
    val html = views.html.cadence(Seq.empty, java.time.Instant.EPOCH).body
    html should include ("""value="https://uk.showtimes.cc/debug/cadence"""")
  }

  // Locally in Dev the wiring builds per-country debug stacks and passes
  // `sameOrigin = true`: the switcher then stays on THIS origin (`?country=xx`)
  // so it switches the served db in-process instead of navigating to the other
  // country's production host (which serves prod mode and 404s /debug).
  "debug navbar (Dev, switch wired)" should "emit same-origin ?country= links, not production hosts" in {
    val html = views.html.debug(Seq.empty, titleNormalizer, current = models.Country.UnitedKingdom, sameOrigin = true).body
    html should include ("""value="/debug?country=uk" selected""") // the switched-to country, selected
    html should include ("""value="/debug?country=pl"""")
    // Never a cross-host jump to production. Named explicitly rather than by a
    // shared suffix: the two brands live on different registrable domains, so
    // there is no one string that catches both.
    html should not include ("kinowo.net")
    html should not include ("showtimes.cc")
  }
}
