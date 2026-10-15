package services.auth

import play.api.Logging
import services.facebook.{AppUsage, FacebookUsageBrake}
import tools.{GetOnlyHttpFetch, HttpStatusException}

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.Duration
import scala.util.{Failure, Success, Try}

/**
 * The Graph API calls a Facebook sign-in makes (the token exchange, `/me`, `debug_token`), each
 * reporting Facebook's `X-App-Usage` to the fleet's [[FacebookUsageBrake]]. They spend the same
 * hourly allowance as the worker's re-scrapes, which only learned of them from the next
 * re-scrape's reading — after deciding to send it. A sign-in cannot wait, so a reading at the
 * ceiling holds the re-scrapes instead.
 *
 * GET only, as the sign-in clients are; a non-2xx answer throws [[HttpStatusException]], as
 * `RealHttpFetch`'s does, after its usage is reported: Facebook reports it on refusals too.
 */
class FacebookGraphFetch(brake: FacebookUsageBrake, tls: javax.net.ssl.SSLContext) extends GetOnlyHttpFetch with Logging {
  private val client = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(10)).sslContext(tls).build()

  def get(url: String): String = {
    val response = client.send(HttpRequest.newBuilder(URI.create(url)).timeout(Duration.ofSeconds(20)).GET().build(),
      HttpResponse.BodyHandlers.ofString())
    AppUsage.of(response.headers()).foreach(report)
    if (response.statusCode() / 100 == 2) response.body()
    else throw new HttpStatusException(response.statusCode(), "GET", url, None)
  }

  // The hold is the fleet's re-scrapes' business, never the sign-in's: a fleet database that cannot
  // be written is logged, and the sign-in goes on.
  private def report(usage: AppUsage): Unit = Try(brake.observe(usage)) match {
    case Success(Some(until)) =>
      logger.info(s"Facebook sign-in: Facebook says ${usage.percent}% of the app's hourly allowance is spent — the fleet's re-scrapes wait until $until")
    case Success(None) => ()
    case Failure(e)    => logger.warn(s"Facebook sign-in: could not hold the fleet's re-scrapes at ${usage.percent}% (${e.getClass.getSimpleName}: ${e.getMessage})")
  }
}
