package services.sharecards

import com.sun.net.httpserver.HttpServer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.{InetSocketAddress, URI}
import java.nio.charset.StandardCharsets

/** The Graph client over real HTTP: Facebook's answer read with its `X-App-Usage`, which the
 *  drain paces the fleet on. */
class HttpFacebookGraphSpec extends AnyFlatSpec with Matchers {

  /** A server answering every request with `status`, `body` and `headers`, for the length of `use`. */
  private def answering[T](status: Int, body: String, headers: Map[String, String])(use: URI => T): T = {
    val server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0)
    server.createContext("/", exchange => {
      val bytes = body.getBytes(StandardCharsets.UTF_8)
      headers.foreach { case (k, v) => exchange.getResponseHeaders.add(k, v) }
      exchange.sendResponseHeaders(status, bytes.length.toLong)
      exchange.getResponseBody.write(bytes)
      exchange.close()
    })
    server.start()
    try use(URI.create(s"http://127.0.0.1:${server.getAddress.getPort}/")) finally server.stop(0)
  }

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
