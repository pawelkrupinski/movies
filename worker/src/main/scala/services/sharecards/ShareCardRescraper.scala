package services.sharecards

import models.{CityPath, Country}
import play.api.Logging
import services.readmodel.{FilmSlugs, ReadModelReader}
import tools.{Env, TlsTrust}

import java.net.URI
import java.net.URLEncoder
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.time.{Clock, Duration}

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
 * The `RescrapeShareCard` task's work: ask Facebook to re-scrape each of a film's city pages, so a
 * preview it cached is replaced — once a film published without its card (the first-publish gate
 * timed out) has one, and when the card of a film in its first week changes. Spaced out at enqueue
 * ([[ShareCardService.RescrapeSpacing]]).
 */
class ShareCardRescraper(graph: Option[FacebookGraph], reader: ReadModelReader, country: Country,
                         metrics: ShareCardMetrics, clock: Clock) extends Logging {

  /** False when a request failed and the task should be retried. */
  def rescrape(filmId: String): Boolean = graph match {
    case None =>
      metrics.rescrape(ShareCardMetrics.RescrapeOutcome.Disabled)
      true
    case Some(facebook) =>
      // A read that failed has no pages to ask for, and "none of them failed" would then
      // hold for a re-scrape that never ran: retry instead.
      val urls = scala.util.Try(pageUrls(filmId)) match {
        case scala.util.Success(read) => read
        case scala.util.Failure(e) =>
          logger.info(s"share card: re-scrape of $filmId deferred, its pages could not be read: ${e.getMessage}")
          return false
      }
      val failures = urls.flatMap(url => facebook.scrape(url).left.toOption.map(url -> _))
      metrics.rescrape(ShareCardMetrics.RescrapeOutcome.Sent)
      failures.foreach { case (url, why) =>
        metrics.rescrape(ShareCardMetrics.RescrapeOutcome.Failed)
        logger.info(s"share card: Facebook re-scrape of $url failed: $why")
      }
      failures.isEmpty
  }

  /** The film's page in every city it screens in, on the country's public origin. THROWS
   *  when the read model cannot be read — an empty list is "no page to refresh". */
  private[sharecards] def pageUrls(filmId: String): Seq[String] = {
    val (refs, refsRead) = reader.findAllScreeningRefsChecked()
    if (!refsRead) throw new IllegalStateException("web_screenings read incomplete")
    val slug   = slugFor(filmId)
    val cities = refs.iterator.filter(_.filmId == filmId)
      .map(_._id.stripPrefix(s"$filmId|").takeWhile(_ != '|')).toSet
    (for {
      origin <- country.webOrigin.toSeq
      s      <- slug.toSeq
      city   <- country.cities.filter(c => cities(c.slug))
    } yield origin + CityPath.film(city, s)).sorted
  }

  // A film's slug depends on every film (collisions are settled across the corpus), so it takes a
  // read of all of `web_movies`; a burst of re-scrapes (a template change re-draws every recent
  // film) shares one read for [[ShareCardRescraper.SlugsFor]]. A film the read lacks re-reads.
  private var slugs = Option.empty[(FilmSlugs, java.time.Instant)]
  private def slugFor(filmId: String): Option[String] = synchronized {
    val fresh = slugs.filter { case (_, at) => clock.instant().isBefore(at.plusMillis(ShareCardRescraper.SlugsFor.toMillis)) }
      .map(_._1).filter(_.slugFor(filmId).isDefined)
    fresh.getOrElse {
      val (movies, complete) = reader.findAllMoviesChecked()
      if (!complete) throw new IllegalStateException("web_movies read incomplete")
      val read = FilmSlugs(movies)
      slugs = Some(read -> clock.instant())
      read
    }.slugFor(filmId)
  }
}

object ShareCardRescraper {
  import scala.concurrent.duration.*
  /** How long one read of the film slugs serves re-scrapes. */
  val SlugsFor: FiniteDuration = 10.minutes
}
