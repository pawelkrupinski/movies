package controllers

import tools.SpecTimeouts

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.stream.Materializer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import play.api.mvc.{RequestHeader, Result, Results}
import play.api.test.FakeRequest

import scala.concurrent.{Await, ExecutionContext, Future}

class CspFilterSpec extends AnyFlatSpec with Matchers {

  // One shared actor system / materializer for the whole spec — Play's
  // Filter trait needs a Materializer in scope; we never push enough
  // traffic to care about isolating it.
  private implicit val sys: ActorSystem     = ActorSystem("csp-filter-spec")
  private implicit val mat: Materializer    = Materializer(sys)
  private implicit val executionContext: ExecutionContext = sys.dispatcher

  private val filter = new CspFilter()

  private def run(request: RequestHeader = FakeRequest(), upstream: Result = Results.Ok("ok")): Result =
    Await.result(filter.apply(_ => Future.successful(upstream))(request), SpecTimeouts.Io)

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

  it should "frame every trailer embed the film page can render, and nothing else" in {
    // The trailer iframe on /movie loads whatever `TrailerEmbed.embedUrlFor`
    // made of a cinema's link — YouTube or Vimeo. A host the policy leaves out
    // breaks playback silently: the iframe just fails to load with a console
    // warning. Vimeo's was left out from the start.
    val frameSrc = run().header.headers("Content-Security-Policy").split("; ").find(_.startsWith("frame-src "))
    val embeds = Seq("https://youtu.be/dQw4w9WgXcQ", "https://vimeo.com/76979871").flatMap(models.TrailerEmbed.embedUrlFor)
    embeds should have size 2
    embeds.foreach(embed => frameSrc.toSeq.flatMap(_.split(" ")) should contain(ForwardedUrl.originOf(embed)))
    frameSrc shouldBe Some("frame-src https://www.youtube.com https://www.youtube-nocookie.com https://player.vimeo.com")
  }

  it should "leave img-src wide because cinema CDNs shift over time" in {
    val csp = run().header.headers("Content-Security-Policy")
    csp should include("img-src 'self' data: https:")
  }

  it should "keep Google Fonts off the public site, and allow them only in the review pages' policy" in {
    val csp = run().header.headers("Content-Security-Policy")
    csp should not include "fonts.googleapis.com"
    csp should not include "fonts.gstatic.com"
    CspFilter.WithGoogleFonts should include("style-src 'self' 'unsafe-inline' https://fonts.googleapis.com")
    CspFilter.WithGoogleFonts should include("font-src 'self' data: https://fonts.gstatic.com")
  }

  it should "set X-Content-Type-Options, Referrer-Policy and Permissions-Policy" in {
    val h = run().header.headers
    h.get("X-Content-Type-Options") shouldBe Some("nosniff")
    h.get("Referrer-Policy")        shouldBe Some("strict-origin-when-cross-origin")
    h.get("Permissions-Policy")     shouldBe defined
    h("Permissions-Policy") should include("interest-cohort=()")
  }

  it should "allow geolocation for same-origin so the landing page can auto-detect the city" in {
    // The `/` landing page calls navigator.geolocation to redirect to the
    // nearest supported city (landing.scala.html). A blanket `geolocation=()`
    // blocks that same-origin call outright, silently breaking the feature
    // and logging a permissions-policy violation to the console. `(self)`
    // permits the origin's own scripts while still denying every third party.
    val pp = run().header.headers("Permissions-Policy")
    pp should include("geolocation=(self)")
    pp should not include "geolocation=()"
  }

  it should "not overwrite a CSP set by the upstream controller" in {
    // A controller serving an embed/iframe page might set its own
    // looser CSP. The filter must defer rather than clobber.
    val custom = Results.Ok("ok").withHeaders("Content-Security-Policy" -> "default-src *")
    run(upstream = custom).header.headers("Content-Security-Policy") shouldBe "default-src *"
  }
}
