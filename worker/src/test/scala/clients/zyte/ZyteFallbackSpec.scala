package clients.zyte

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{FallbackHttpFetch, GetOnlyHttpFetch, HttpFetch}
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
}
