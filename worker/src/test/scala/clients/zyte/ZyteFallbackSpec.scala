package clients.zyte

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{FallbackHttpFetch, GetOnlyHttpFetch, HttpFetch, HttpOutcome, HttpOutcomeRecorder, HttpStatusException}

import scala.collection.mutable
import services.cinemas.common.ZyteFallback

/**
 * `ZyteFallback.fetchFor` composes the proxy chain at the composition root.
 * The key is injected (not read from the ambient env) so both branches are
 * deterministic regardless of whether CI has `ZYTE_API_KEY` set.
 *
 *   - no key → the chain collapses to `direct` (local dev, fixture replay).
 *   - key set → Zyte fronts `direct` in a `FallbackHttpFetch`.
 */
class ZyteFallbackSpec extends AnyFlatSpec with Matchers {

  private val direct: HttpFetch = new GetOnlyHttpFetch {
    override def get(url: String): String = "direct-body"
  }

  "fetchFor without a Zyte key" should "return direct unchanged — no proxy in front" in {
    ZyteFallback.fetchFor(direct, apiKey = None) should be theSameInstanceAs direct
  }

  it should "treat a blank key as no key" in {
    ZyteFallback.fetchFor(direct, apiKey = Some("")) should be theSameInstanceAs direct
  }

  "fetchFor with a Zyte key" should "front direct with a Zyte fallback chain" in {
    ZyteFallback.fetchFor(direct, apiKey = Some("test-key")) shouldBe a[FallbackHttpFetch]
  }

  // Odeon's Zyte fallback paid for a 401 on every request for as long as its
  // Authorization header was being dropped, and nothing counted it: the Zyte leg
  // was metered nowhere. The meter sees every Zyte attempt — and ONLY Zyte's, not
  // the free direct leg behind it.
  "the Zyte chain" should "meter each Zyte attempt's outcome, and not the direct leg's" in {
    val outcomes = mutable.ListBuffer.empty[String]
    val meter: HttpOutcomeRecorder = (outcome: String) => outcomes += outcome
    val rejecting = new GetOnlyHttpFetch {
      override def get(url: String): String = throw new HttpStatusException(401, "GET", url, None)
    }
    val chain = ZyteFallback.chain(Some(rejecting), direct, meter)

    chain.get("https://vwc.odeon.co.uk/a") shouldBe "direct-body"
    chain.get("https://vwc.odeon.co.uk/b") shouldBe "direct-body"
    outcomes.toList shouldBe List(HttpOutcome.Http401, HttpOutcome.Http401)
  }

  it should "record a success when Zyte answers" in {
    val outcomes = mutable.ListBuffer.empty[String]
    val zyte = new GetOnlyHttpFetch { override def get(url: String): String = "zyte-body" }
    ZyteFallback.chain(Some(zyte), direct, (o: String) => outcomes += o).get("https://x/") shouldBe "zyte-body"
    outcomes.toList shouldBe List(HttpOutcome.Success)
  }
}
