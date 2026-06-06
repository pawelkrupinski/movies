package clients.zyte

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.cinemas.ZyteFallback
import tools.{FallbackHttpFetch, GetOnlyHttpFetch, HttpFetch}

/**
 * `ZyteFallback.fetchFor` composes the proxy chain at the composition root.
 * `Env.get("ZYTE_API_KEY")` reads a JVM system property (one of its layers),
 * so we toggle the key per-test without a real environment variable.
 *
 *   - key unset  → the chain collapses to `direct` (local dev, fixture replay).
 *   - key set    → Zyte fronts `direct` in a `FallbackHttpFetch`.
 */
class ZyteFallbackSpec extends AnyFlatSpec with Matchers {

  private val direct: HttpFetch = new GetOnlyHttpFetch {
    override def get(url: String): String = "direct-body"
  }

  private def withKey[A](key: Option[String])(body: => A): A = {
    val prev = Option(System.getProperty("ZYTE_API_KEY"))
    key match {
      case Some(k) => System.setProperty("ZYTE_API_KEY", k)
      case None    => System.clearProperty("ZYTE_API_KEY")
    }
    try body
    finally prev match {
      case Some(p) => System.setProperty("ZYTE_API_KEY", p)
      case None    => System.clearProperty("ZYTE_API_KEY")
    }
  }

  "fetchFor without ZYTE_API_KEY" should "return direct unchanged — no proxy in front" in {
    withKey(None) {
      ZyteFallback.fetchFor(direct) should be theSameInstanceAs direct
    }
  }

  "fetchFor with ZYTE_API_KEY" should "front direct with a Zyte fallback chain" in {
    withKey(Some("test-key")) {
      ZyteFallback.fetchFor(direct) shouldBe a[FallbackHttpFetch]
    }
  }
}
