package services.sharecards

import models.{CityPath, Country}
import play.api.Logging
import services.readmodel.{FilmSlugs, ReadModelReader}
import tools.{Env, TlsTrust}

import java.net.URI
import java.net.URLEncoder
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.time.Duration

/** Asks Facebook to fetch a page again, so a preview it cached (image included) is replaced. */
trait FacebookGraph {
  /** Right when Facebook accepted the re-scrape; Left with why not. */
  def scrape(url: String): Either[String, Unit]
}

/** The Graph API's re-scrape (`POST /?id=<url>&scrape=true`) with an app access token. The token
 *  travels in the form body, not the query string, so no URL that reaches a log carries it. */
class HttpFacebookGraph(appId: String, appSecret: String) extends FacebookGraph {
  private val client = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(10)).sslContext(TlsTrust.augmentedContext).build()

  def scrape(url: String): Either[String, Unit] = {
    def enc(s: String) = URLEncoder.encode(s, StandardCharsets.UTF_8)
    val body = s"id=${enc(url)}&scrape=true&access_token=${enc(s"$appId|$appSecret")}"
    try {
      val request = HttpRequest.newBuilder(URI.create("https://graph.facebook.com/")).timeout(Duration.ofSeconds(20))
        .header("Content-Type", "application/x-www-form-urlencoded").POST(HttpRequest.BodyPublishers.ofString(body)).build()
      val response = client.send(request, HttpResponse.BodyHandlers.discarding())
      if (response.statusCode() / 100 == 2) Right(()) else Left(s"HTTP ${response.statusCode()}")
    } catch { case e: Exception => Left(e.getClass.getSimpleName) }
  }
}

object FacebookGraph {
  /** The Graph client when the worker has the app's credentials (`FACEBOOK_APP_ID` +
   *  `FACEBOOK_APP_SECRET`, the web's login names), else None — re-scraping is then a no-op. */
  def fromEnv(): Option[FacebookGraph] =
    for { id <- Env.get("FACEBOOK_APP_ID"); secret <- Env.get("FACEBOOK_APP_SECRET") } yield new HttpFacebookGraph(id, secret)
}

/**
 * The `RescrapeShareCard` task's work: once a film published without its card (the first-publish
 * gate timed out) has one, ask Facebook to re-scrape each of its city pages, so the fallback image
 * it may have cached is replaced. Spaced out at enqueue ([[ShareCardService.RescrapeSpacing]]).
 */
class ShareCardRescraper(graph: Option[FacebookGraph], reader: ReadModelReader, country: Country,
                         metrics: ShareCardMetrics) extends Logging {

  /** False when a request failed and the task should be retried. */
  def rescrape(filmId: String): Boolean = graph match {
    case None =>
      metrics.rescrape(ShareCardMetrics.RescrapeOutcome.Disabled)
      true
    case Some(facebook) =>
      val urls = pageUrls(filmId)
      val failures = urls.flatMap(url => facebook.scrape(url).left.toOption.map(url -> _))
      metrics.rescrape(ShareCardMetrics.RescrapeOutcome.Sent)
      failures.foreach { case (url, why) =>
        metrics.rescrape(ShareCardMetrics.RescrapeOutcome.Failed)
        logger.info(s"share card: Facebook re-scrape of $url failed: $why")
      }
      failures.isEmpty
  }

  /** The film's page in every city it screens in, on the country's public origin. */
  private[sharecards] def pageUrls(filmId: String): Seq[String] = {
    val slug   = FilmSlugs(reader.findAllMovies()).slugFor(filmId)
    val cities = reader.findAllScreeningRefs().iterator.filter(_.filmId == filmId)
      .map(_._id.stripPrefix(s"$filmId|").takeWhile(_ != '|')).toSet
    (for {
      origin <- country.webOrigin.toSeq
      s      <- slug.toSeq
      city   <- country.cities.filter(c => cities(c.slug))
    } yield origin + CityPath.film(city, s)).sorted
  }
}
