package scripts

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The prod enqueue runs only in a JVM whose environment was started for its own call: a kept sbt server keeps the
 *  environment it started with — another call's database — and must be refused, never trusted. */
class EnqueueSourceRefreshSpec extends AnyFlatSpec with Matchers {
  import EnqueueSourceRefresh.refusal

  "the enqueue" should "run in the environment its own call started" in {
    refusal(Some("a1"), Some("a1")) shouldBe None
  }

  it should "refuse a kept server started for another call, or for none" in {
    refusal(Some("b2"), Some("a1")).get should include("carries b2")
    refusal(None, Some("a1")).get should include("carries none")
  }

  it should "refuse a run started without scripts/source-refresh" in {
    refusal(Some("a1"), None).get should include("scripts/source-refresh")
  }
}
