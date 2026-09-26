package services.sharecards

import models.{CityPath, Country}
import play.api.libs.json.Json
import services.readmodel.{FilmSlugs, ReadModelReader}

import java.net.URI
import java.net.URLEncoder
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets
import java.time.{Clock, Duration}
import scala.util.Try

/** Asks Facebook to fetch a page again, so a preview it cached (image included) is replaced. */
trait FacebookGraph {
  def scrape(url: String): FacebookScrape
}

/** What Facebook said to one re-scrape request. */
sealed trait FacebookScrape
object FacebookScrape {
  case object Accepted extends FacebookScrape
  /** The app's rate limit: every request from the fleet shares it, so the whole fleet waits. */
  final case class RateLimited(why: String) extends FacebookScrape
  /** Anything else — this page's request failed. */
  final case class Refused(why: String) extends FacebookScrape

  /** Graph API error codes that mean "too many calls", not "this call is wrong": 4 (application
   *  request limit), 17 (user request limit), 32 (page request limit), 613 (calls within a period). */
  val RateLimitCodes: Set[Int] = Set(4, 17, 32, 613)

  /** Read a response: 2xx is accepted; a 429, or an error body naming a rate-limit code, is the
   *  rate limit; anything else is refused, with the Graph API's own code and message kept. */
  def of(status: Int, body: String): FacebookScrape =
    if (status / 100 == 2) Accepted
    else {
      val error   = Try(Json.parse(body) \ "error").toOption.flatMap(_.toOption)
      val code    = error.flatMap(e => (e \ "code").asOpt[Int])
      val message = error.flatMap(e => (e \ "message").asOpt[String])
      val why     = (Seq(s"HTTP $status") ++ code.map(c => s"code $c") ++ message).mkString(" ")
      if (status == 429 || code.exists(RateLimitCodes)) RateLimited(why) else Refused(why)
    }
}

/** The Graph API's re-scrape (`POST /?id=<url>&scrape=true`) with an app access token. The token
 *  travels in the form body, not the query string, so no URL that reaches a log carries it. */
class HttpFacebookGraph(appId: settings.FacebookAppId, appSecret: settings.FacebookAppSecret, tls: javax.net.ssl.SSLContext) extends FacebookGraph {
  private val client = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(10)).sslContext(tls).build()

  def scrape(url: String): FacebookScrape = {
    def enc(s: String) = URLEncoder.encode(s, StandardCharsets.UTF_8)
    val body = s"id=${enc(url)}&scrape=true&access_token=${enc(s"${appId.value}|${appSecret.value}")}"
    try {
      val request = HttpRequest.newBuilder(URI.create("https://graph.facebook.com/")).timeout(Duration.ofSeconds(20))
        .header("Content-Type", "application/x-www-form-urlencoded").POST(HttpRequest.BodyPublishers.ofString(body)).build()
      val response = client.send(request, HttpResponse.BodyHandlers.ofString())
      FacebookScrape.of(response.statusCode(), response.body())
    } catch { case e: Exception => FacebookScrape.Refused(e.getClass.getSimpleName) }
  }
}

object FacebookGraph {
  /** The Graph client when the worker has the app's credentials (`FACEBOOK_APP_ID` +
   *  `FACEBOOK_APP_SECRET`, the web's login names), else None — re-scraping is then off. */
  def fromConfiguration(configuration: settings.ProcessConfiguration, tls: javax.net.ssl.SSLContext): Option[FacebookGraph] =
    for { id <- configuration.facebookAppId; secret <- configuration.facebookAppSecret } yield new HttpFacebookGraph(id, secret, tls)
}

/**
 * A film's page in every city it screens in, on the country's public origin — the URLs a
 * re-scrape asks Facebook to fetch again ([[FacebookRescrapeDrain]]).
 */
class FilmPageUrls(reader: ReadModelReader, country: Country, clock: Clock) extends (String => Seq[String]) {

  /** THROWS when the read model cannot be read — an empty list is "no page to refresh". The
   *  cities come from the film's own screenings ([[ReadModelReader.findCard]], a read by `_id`
   *  range), not a scan of every screening: a burst re-scrapes hundreds of films. */
  def apply(filmId: String): Seq[String] = {
    val card   = reader.findCard(filmId).getOrElse(throw new IllegalStateException(s"read-model card $filmId unreadable"))
    val slug   = slugFor(filmId)
    val cities = card.screenings.map(_._id.stripPrefix(s"$filmId|").takeWhile(_ != '|')).toSet
    (for {
      origin <- country.webOrigin.toSeq
      s      <- slug.toSeq
      city   <- country.cities.filter(c => cities(c.slug))
    } yield origin + CityPath.film(city, s)).sorted
  }

  // A film's slug depends on every film (collisions are settled across the corpus), so it takes a
  // read of all of `web_movies`; a burst of re-scrapes (a template change re-draws every recent
  // film) shares one read for [[FilmPageUrls.SlugsFor]]. A film the read lacks re-reads.
  private var slugs = Option.empty[(FilmSlugs, java.time.Instant)]
  private def slugFor(filmId: String): Option[String] = synchronized {
    val fresh = slugs.filter { case (_, at) => clock.instant().isBefore(at.plusMillis(FilmPageUrls.SlugsFor.toMillis)) }
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

object FilmPageUrls {
  import scala.concurrent.duration.*
  /** How long one read of the film slugs serves re-scrapes. */
  val SlugsFor: FiniteDuration = 10.minutes
}
