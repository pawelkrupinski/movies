package views

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.MirrorFreshness
import services.movies.SingleCountryNormalizer.titleNormalizer

import java.time.Instant
import scala.concurrent.duration._

/**
 * Every `/debug` page reads the local read-mirror, and a mirror whose sync has
 * stopped serves a snapshot that renders exactly like live data — on 2026-08-30
 * a day-old copy read as a rating-cadence bug, because the frozen US corpus
 * showed only the 2h base interval. The navbar's age badge is what makes that
 * impossible to miss, so it is asserted on every debug view rather than only the
 * one it was noticed on.
 */
class DebugViewMirrorFreshnessSpec extends AnyFlatSpec with Matchers {

  private implicit val city: models.City = models.Poznan

  private def age(behind: FiniteDuration) =
    MirrorFreshness.describe(Some(Instant.EPOCH), Instant.EPOCH.plusMillis(behind.toMillis))

  "debug navbar" should "say how far behind the mirror is" in {
    val html = views.html.debug(Seq.empty, titleNormalizer, mirror = age(12.seconds)).body
    html should include ("mirror 12s behind")
    // The stylesheet names both classes whatever the state, so the assertions
    // below are about the rendered ELEMENT's class list, never the class string.
    html should not include ("""debug-nav-mirror is-stale""")
    html should not include ("⚠")
  }

  // THE case: a sync that stopped yesterday, on the page that reads its data.
  it should "mark a mirror that has stopped syncing, in a way that cannot be read past" in {
    val html = views.html.debug(Seq.empty, titleNormalizer, mirror = age(26.hours)).body
    html should include ("mirror 26h behind")
    html should include ("""debug-nav-mirror is-stale""")
    html should include ("⚠")
  }

  it should "render no age in prod, where the pages read the source" in {
    val html = views.html.debug(Seq.empty, titleNormalizer).body
    html should not include ("""<span class="debug-nav-mirror""")
    html should not include ("behind")
  }

  // The cadence page is the one whose staleness was mistaken for a bug, and the
  // read-model dump is the other page reading the same copy.
  "cadence page" should "carry the same badge" in {
    val html = views.html.cadence(Seq.empty, Instant.EPOCH, mirror = age(26.hours)).body
    html should include ("mirror 26h behind")
    html should include ("""debug-nav-mirror is-stale""")
  }

  "read-model page" should "carry the same badge" in {
    val html = views.html.debugReadModel(Seq.empty, Map.empty, Instant.EPOCH, mirror = age(26.hours)).body
    html should include ("mirror 26h behind")
    html should include ("""debug-nav-mirror is-stale""")
  }
}
