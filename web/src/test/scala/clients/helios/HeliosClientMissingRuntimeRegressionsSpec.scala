package clients.helios

import clients.tools.FakeHttpFetch
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.HeliosClient

class HeliosClientMissingRuntimeRegressionsSpec extends AnyFlatSpec with Matchers {
  private val fakeHttp = new FakeHttpFetch("helios/missing-runtime")
  private val client   = new HeliosClient(fakeHttp)

  private def fetch() = client.fetch()

  // ── Smoke test ────────────────────────────────────────────────────────────

  "HeliosClient.fetch" should "return results with runtime always set" in {
    val result = fetch()
    result                    should not be empty
    result.size               should be >= 5
    result.find(_.movie.runtimeMinutes.isEmpty) shouldBe empty
  }
}
