package services.facebook

import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration

/** One Graph API answer: its status, its body, and the share of the app's hourly allowance
 *  Facebook says is spent ([[AppUsage]]) — reported on refusals too. */
final case class FacebookGraphResponse(status: Int, body: String, usage: Option[AppUsage])

/**
 * The HTTP client every Graph API call made with the app's credentials goes through — the
 * worker's re-scrapes (`HttpFacebookGraph`) and the web's Facebook sign-ins
 * (`FacebookGraphFetch`) alike: one connect and request timeout, the fleet's TLS trust, and each
 * answer read with the [[AppUsage]] its `X-App-Usage` header carries. What a caller does with the
 * usage is its own: the worker hands it to its drain, the web to the [[FacebookUsageBrake]].
 */
final class FacebookGraphHttp(tls: javax.net.ssl.SSLContext) {
  private val client = HttpClient.newBuilder().connectTimeout(FacebookGraphHttp.ConnectTimeout).sslContext(tls).build()

  /** Send `request` with [[FacebookGraphHttp.RequestTimeout]]; throws what `HttpClient.send` throws. */
  def send(request: HttpRequest.Builder): FacebookGraphResponse = {
    val response = client.send(request.timeout(FacebookGraphHttp.RequestTimeout).build(), HttpResponse.BodyHandlers.ofString())
    FacebookGraphResponse(response.statusCode(), response.body(), AppUsage.of(response.headers()))
  }
}

object FacebookGraphHttp {
  val ConnectTimeout: Duration = Duration.ofSeconds(10)
  val RequestTimeout: Duration = Duration.ofSeconds(20)
}
