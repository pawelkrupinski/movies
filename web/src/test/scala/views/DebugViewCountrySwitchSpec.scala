package views

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

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
    val html = views.html.debug(Seq.empty).body
    html should include ("""class="debug-nav-country"""")
    // The UK deployment's corpus debug page, on its own host.
    html should include ("""value="https://showtimes-uk.fly.dev/debug"""")
  }

  it should "mark this deployment's own country as the selected option" in {
    val html = views.html.debug(Seq.empty).body
    // KINOWO_COUNTRY unset in tests → Poland; its option is pre-selected.
    html should include ("""value="https://kinowo.fly.dev/debug" selected""")
  }

  "cadence navbar" should "keep the switcher pointed at the cadence page, not the corpus page" in {
    val html = views.html.cadence(Seq.empty, java.time.Instant.EPOCH).body
    html should include ("""value="https://showtimes-uk.fly.dev/debug/cadence"""")
  }
}
