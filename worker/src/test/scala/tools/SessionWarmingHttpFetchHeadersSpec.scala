package tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** The warming wrapper is the other delegating fetch on the residential-proxy path,
 *  and it carried the same silent header loss as [[FallbackHttpFetch]] — see
 *  [[FallbackHttpFetchHeadersSpec]] for the outage that surfaced the class of bug. */
class SessionWarmingHttpFetchHeadersSpec extends AnyFlatSpec with Matchers {

  private class RecordingFetch extends GetOnlyHttpFetch {
    val seen: mutable.Buffer[(String, Map[String, String])] = mutable.Buffer.empty
    override def get(url: String): String = get(url, Map.empty)
    override def get(url: String, headers: Map[String, String]): String = {
      seen += (url -> headers)
      "body"
    }
  }

  private val Bearer = Map("Authorization" -> "Bearer jwt-token")

  "SessionWarmingHttpFetch" should "carry request headers through to the delegate" in {
    val delegate = new RecordingFetch
    new SessionWarmingHttpFetch(delegate, "https://www.multikino.pl/")
      .get("https://www.multikino.pl/api/x", Bearer) shouldBe "body"
    delegate.seen shouldBe Seq("https://www.multikino.pl/api/x" -> Bearer)
  }
}
