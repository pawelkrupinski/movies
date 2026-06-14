package clients.tools

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import tools.{FallbackHttpFetch, HttpFetch}

import scala.collection.mutable

class FallbackHttpFetchSpec extends AnyFlatSpec with Matchers {

  // In-memory backends parameterised by what they do on get/post —
  // either return a stub body or throw a named exception. Lets each
  // test arrange the failure shape it cares about without mocking
  // HTTP at all.
  private def ok(body: String): HttpFetch = new HttpFetch {
    override def get(url: String): String = body
    override def post(url: String, body: String, contentType: String): String = this.get(url)
  }
  // `boom` not `fail` — `Assertions.fail` is inherited by every ScalaTest
  // suite, so a `private def fail` here would clash with the override.
  private def boom(message: String): HttpFetch = new HttpFetch {
    override def get(url: String): String = throw new RuntimeException(message)
    override def post(url: String, body: String, contentType: String): String = this.get(url)
  }

  "FallbackHttpFetch" should "return the first backend's body when it succeeds" in {
    val visited = mutable.ListBuffer.empty[String]
    val tracking = new HttpFetch {
      override def get(url: String): String = { visited += "primary"; "primary-body" }
      override def post(url: String, body: String, contentType: String): String = this.get(url)
    }
    val chain = new FallbackHttpFetch(Seq("primary" -> tracking, "secondary" -> ok("secondary-body")))
    chain.get("https://example") shouldBe "primary-body"
    visited shouldBe Seq("primary")  // secondary never consulted
  }

  it should "roll over to the next backend when the first throws" in {
    val chain = new FallbackHttpFetch(Seq("primary" -> boom("zyte boom"), "secondary" -> ok("from-secondary")))
    chain.get("https://example") shouldBe "from-secondary"
  }

  it should "keep rolling through every failing backend until one succeeds" in {
    val chain = new FallbackHttpFetch(Seq(
      "zyte"   -> boom("zyte 5xx"),
      "direct" -> ok("direct-finally-worked")
    ))
    chain.get("https://example") shouldBe "direct-finally-worked"
  }

  it should "throw with all named failures aggregated when every backend fails" in {
    val chain = new FallbackHttpFetch(Seq(
      "zyte"   -> boom("z-failure"),
      "direct" -> boom("d-failure")
    ))
    val exception = intercept[RuntimeException](chain.get("https://example"))
    exception.getMessage should include ("All 2 backends failed")
    exception.getMessage should include ("zyte:")
    exception.getMessage should include ("z-failure")
    exception.getMessage should include ("direct:")
    exception.getMessage should include ("d-failure")
  }

  it should "exercise the same fallback for post as for get" in {
    val visited = mutable.ListBuffer.empty[String]
    val chain = new FallbackHttpFetch(Seq(
      "primary"   -> boom("503"),
      "secondary" -> new HttpFetch {
        override def get(url: String): String = "get-body"
        override def post(url: String, body: String, contentType: String): String = {
          visited += s"$url|$body|$contentType"
          "post-body"
        }
      }
    ))
    chain.post("https://x", "payload", "text/plain") shouldBe "post-body"
    visited shouldBe Seq("https://x|payload|text/plain")
  }

  it should "refuse to construct with an empty backend list — wiring bug, not runtime fallback" in {
    intercept[IllegalArgumentException] { new FallbackHttpFetch(Seq.empty) }
  }
}
