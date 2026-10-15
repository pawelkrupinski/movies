package services.sharecards

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import services.facebook.AppUsage
import tools.AnsweringHttpServer.answering

import java.net.URI

/** The Graph client over real HTTP: Facebook's answer read with its `X-App-Usage`, which the
 *  drain paces the fleet on. */
class HttpFacebookGraphSpec extends AnyFlatSpec with Matchers {

  private def graph(endpoint: URI) = new HttpFacebookGraph(settings.FacebookAppId("app"), settings.FacebookAppSecret("secret"),
    javax.net.ssl.SSLContext.getDefault, endpoint)

  "A re-scrape Facebook accepts" should "carry the app usage Facebook reports with it" in {
    answering(200, """{"id":"https://kinowo.net/"}""", Map("X-App-Usage" -> """{"call_count":91,"total_time":12,"total_cputime":8}""")) { endpoint =>
      graph(endpoint).scrape("https://kinowo.net/") shouldBe FacebookReply(FacebookScrape.Accepted, Some(AppUsage(91)))
    }
  }

  "A refusal" should "carry the usage too: Facebook reports it on its errors" in {
    answering(403, """{"error":{"code":4,"message":"(#4) Application request limit reached"}}""", Map("x-app-usage" -> """{"call_count":104}""")) { endpoint =>
      graph(endpoint).scrape("https://kinowo.net/") shouldBe
        FacebookReply(FacebookScrape.RateLimited("HTTP 403 code 4 (#4) Application request limit reached"), Some(AppUsage(104)))
    }
  }

  "An answer without the header" should "carry no usage" in {
    answering(200, "{}", Map.empty) { endpoint =>
      graph(endpoint).scrape("https://kinowo.net/") shouldBe FacebookReply(FacebookScrape.Accepted, None)
    }
  }
}
