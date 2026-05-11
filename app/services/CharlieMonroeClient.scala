package clients

import models.{CharlieMonroe, CinemaMovie, Movie, Showtime}
import play.api.libs.json._

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.{LocalDateTime, ZonedDateTime}
import scala.util.Try

object CharlieMonroeClient {

  private val PageUrl       = "https://kinomalta.pl/seanse"
  private val JsonLdRegex   = """<script[^>]+type="application/ld\+json"[^>]*>([\s\S]*?)</script>""".r
  private val ArticleRegex  = """<article[^>]+class="movie-card"[^>]*>([\s\S]*?)</article>""".r
  private val FilmUrlRegex  = """href="(https://kinomalta\.pl/movies/[^"]+)"""".r
  private val TitleRegex    = """<h2[^>]*class="title"[^>]*>([\s\S]*?)</h2>""".r
  private val SynopsisRegex = """<p[^>]*class="desc"[^>]*>([\s\S]*?)</p>""".r

  private val httpClient = HttpClient.newBuilder()
    .version(HttpClient.Version.HTTP_1_1)
    .followRedirects(HttpClient.Redirect.NORMAL)
    .build()

  def fetch(): Seq[CinemaMovie] = {
    val request = HttpRequest.newBuilder()
      .uri(URI.create(PageUrl))
      .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
      .header("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
      .header("Accept-Language", "pl-PL,pl;q=0.9,en-US;q=0.8,en;q=0.7")
      .GET()
      .build()

    val response = httpClient.send(request, HttpResponse.BodyHandlers.ofString())
    if (response.statusCode() != 200)
      throw new RuntimeException(s"kinomalta.pl returned ${response.statusCode()}")

    val html = response.body()

    val screenings = JsonLdRegex.findAllMatchIn(html)
      .map(_.group(1))
      .flatMap(parseScreeningEvent)
      .toSeq

    val detailsByTitle = parseArticleDetails(html)

    screenings
      .groupBy(_.movie.title)
      .toSeq
      .map { case (title, events) =>
        val sorted  = events.sortBy(_.dateTime)
        val details = detailsByTitle.getOrElse(title, ArticleDetails(None, None, None))
        CinemaMovie(
          movie     = sorted.head.movie,
          cinema    = CharlieMonroe,
          posterUrl = details.posterUrl.orElse(sorted.flatMap(_.posterUrl).headOption),
          filmUrl   = details.filmUrl,
          synopsis  = details.synopsis,
          cast      = None,
          director  = None,
          showtimes = sorted.map(event => Showtime(event.dateTime, event.bookingUrl))
        )
      }
  }

  // ── Article HTML parsing ───────────────────────────────────────────────────

  private case class ArticleDetails(filmUrl: Option[String], synopsis: Option[String], posterUrl: Option[String])

  private def parseArticleDetails(html: String): Map[String, ArticleDetails] =
    ArticleRegex.findAllMatchIn(html).map(_.group(1)).flatMap { articleHtml =>
      for {
        titleMatch <- TitleRegex.findFirstMatchIn(articleHtml)
      } yield {
        val title    = stripHtml(titleMatch.group(1))
        val filmUrl  = FilmUrlRegex.findFirstMatchIn(articleHtml).map(_.group(1))
        val synopsis = SynopsisRegex.findFirstMatchIn(articleHtml)
                         .map(m => stripHtml(m.group(1)).replaceAll("\\.{3,}$", "").trim)
                         .filter(_.nonEmpty)
        val posterUrl = """data-src="(https://[^"]+)"""".r
                          .findFirstMatchIn(articleHtml).map(_.group(1))
        title -> ArticleDetails(filmUrl, synopsis, posterUrl)
      }
    }.toMap

  private def stripHtml(s: String): String =
    s.replaceAll("<[^>]+>", "").replaceAll("\\s+", " ").trim

  // ── JSON-LD parsing ────────────────────────────────────────────────────────

  private case class ScreeningEntry(
    movie: Movie,
    dateTime: LocalDateTime,
    posterUrl: Option[String],
    bookingUrl: Option[String]
  )

  private def parseScreeningEvent(json: String): Option[ScreeningEntry] =
    Try {
      val node = Json.parse(json)
      val eventType = (node \ "@type").asOpt[String]
        .orElse((node \ "@type").asOpt[JsArray].flatMap(_.value.headOption.flatMap(_.asOpt[String])))

      eventType.filter(_ == "ScreeningEvent").flatMap { _ =>
        for {
          title     <- (node \ "workPresented" \ "name").asOpt[String]
          startDate <- (node \ "startDate").asOpt[String]
        } yield {
          ScreeningEntry(
            movie      = Movie(title),
            dateTime   = ZonedDateTime.parse(startDate).toLocalDateTime,
            posterUrl  = (node \ "workPresented" \ "image").asOpt[String].filter(_.nonEmpty),
            bookingUrl = (node \ "offers" \ "url").asOpt[String].filter(_.nonEmpty)
          )
        }
      }
    }.toOption.flatten
}
