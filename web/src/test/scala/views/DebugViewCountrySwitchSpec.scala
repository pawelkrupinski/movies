package views

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
 * Every debug view is implicitly scoped to the ONE country this deployment
 * serves (its own Mongo db). The debug navbar surfaces that dimension: it always
 * NAMES the current country, and offers a switch to the same debug page on
 * another country's host only while more than one country is deployed
 * (`Country.switchable`). Since the UK and German deployments were stopped on
 * 2026-08-02 that list holds Poland alone, so the switcher collapses to its
 * label branch — the same rule the main navbar applies.
 */
class DebugViewCountrySwitchSpec extends AnyFlatSpec with Matchers {

  // The debug page under test is served under Poznań (a Polish city).
  private implicit val city: models.City = models.Poznan

  "debug navbar" should "name the served country without offering a dead switch" in {
    val html = views.html.debug(Seq.empty).body
    // KINOWO_COUNTRY unset in tests → Poland.
    html should include ("""class="debug-nav-country-label"""")
    html should include (">Polska<")
    html should not include ("""class="debug-nav-country"""")
  }

  it should "not link to a stopped deployment's debug page" in {
    val html = views.html.debug(Seq.empty).body
    html should not include ("showtimes-uk.fly.dev")
    html should not include ("showtimes-de.fly.dev")
  }

  "cadence navbar" should "carry the same country label as the corpus page" in {
    val html = views.html.cadence(Seq.empty, java.time.Instant.EPOCH).body
    html should include ("""class="debug-nav-country-label"""")
    html should not include ("showtimes-uk.fly.dev")
  }

  // Locally in Dev the wiring can build per-country debug stacks and pass
  // `sameOrigin = true`, which switches the served db in-process via `?country=`
  // rather than hopping hosts. That path is gated on the same `Country.switchable`
  // list, so with one deployed country it too renders as the label — and the
  // wiring builds no extra stacks to switch between in the first place.
  "debug navbar (Dev, switch wired)" should "still emit no switcher when only one country is deployed" in {
    val html = views.html.debug(Seq.empty, current = models.Country.UnitedKingdom, sameOrigin = true).body
    html should include ("""class="debug-nav-country-label"""")
    html should include (">United Kingdom<")   // whichever db is being viewed is still named
    html should not include ("?country=")
    html should not include ("fly.dev")         // never a cross-host jump to production
  }
}
