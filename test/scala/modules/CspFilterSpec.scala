package modules

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.mvc.{RequestHeader, Result, Results}
import play.api.test.FakeRequest

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

class CspFilterSpec extends AnyFlatSpec with Matchers {

  // One shared actor system / materializer for the whole spec — Play's
  // Filter trait needs a Materializer in scope; we never push enough
  // traffic to care about isolating it.
  private implicit val sys: ActorSystem     = ActorSystem("csp-filter-spec")
  private implicit val mat: Materializer    = Materializer(sys)
  private implicit val ec: ExecutionContext = sys.dispatcher

  private val filter = new CspFilter()

  private def run(req: RequestHeader = FakeRequest(), upstream: Result = Results.Ok("ok")): Result =
    Await.result(filter.apply(_ => Future.successful(upstream))(req), 2.seconds)

  "CspFilter" should "attach a Content-Security-Policy header to every response" in {
    val result = run()
    val csp    = result.header.headers.getOrElse("Content-Security-Policy", "")
    csp should not be empty
    csp should include("default-src 'self'")
    csp should include("frame-ancestors 'none'")
  }

  it should "allow Sentry, GTM and GA hosts in script-src" in {
    val csp = run().header.headers("Content-Security-Policy")
    csp should include("https://*.sentry-cdn.com")
    csp should include("https://www.googletagmanager.com")
    csp should include("https://www.google-analytics.com")
  }

  it should "allow YouTube as the only frame-src" in {
    // YouTube is the trailer iframe target on /film. If the policy ever
    // drops it, trailer playback breaks silently — the iframe just
    // fails to load with a console warning. This assertion is the
    // tripwire for that regression.
    val csp = run().header.headers("Content-Security-Policy")
    csp should include("frame-src https://www.youtube.com https://www.youtube-nocookie.com")
  }

  it should "leave img-src wide because cinema CDNs shift over time" in {
    val csp = run().header.headers("Content-Security-Policy")
    csp should include("img-src 'self' data: https:")
  }

  it should "set X-Content-Type-Options, Referrer-Policy and Permissions-Policy" in {
    val h = run().header.headers
    h.get("X-Content-Type-Options") shouldBe Some("nosniff")
    h.get("Referrer-Policy")        shouldBe Some("strict-origin-when-cross-origin")
    h.get("Permissions-Policy")     shouldBe defined
    h("Permissions-Policy") should include("interest-cohort=()")
  }

  it should "not overwrite a CSP set by the upstream controller" in {
    // A controller serving an embed/iframe page might set its own
    // looser CSP. The filter must defer rather than clobber.
    val custom = Results.Ok("ok").withHeaders("Content-Security-Policy" -> "default-src *")
    run(upstream = custom).header.headers("Content-Security-Policy") shouldBe "default-src *"
  }
}
