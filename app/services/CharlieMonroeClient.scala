package clients

import models.{CharlieMonroe, CinemaMovie, Movie, Showtime}
import org.jsoup.Jsoup
import play.api.libs.json._

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.{LocalDateTime, ZonedDateTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

object CharlieMonroeClient {

  private val PageUrl = "https://kinomalta.pl/seanse"

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

    parseHtml(response.body())
  }

  private def parseHtml(html: String): Seq[CinemaMovie] = {
    val doc = Jsoup.parse(html)

    val screenings = doc.select("script[type=application/ld+json]").asScala
      .flatMap(el => Try(Json.parse(el.data())).toOption)
      .flatMap(parseScreeningEvent)
      .toSeq

    // Build (lowercaseTitle, "DD.MM.YYYY", "HH:MM") → hall name from HTML articles
    val roomMap = collection.mutable.Map[(String, String, String), String]()
    val detailsByTitle = doc.select("article.movie-card").asScala.flatMap { article =>
      Option(article.selectFirst("h2.title")).map(_.text().trim).map { title =>
        article.select(".showtimes-row").asScala.foreach { row =>
          val hall = Option(row.selectFirst(".hall-label")).map(_.text().trim).filter(_.nonEmpty)
          hall.foreach { hallName =>
            row.select("button.btn-showtime").asScala.foreach { btn =>
              val time = Option(btn.selectFirst("span.time")).map(_.text().trim)
              val date = Option(btn.selectFirst("span.price")).map(_.text().trim)
              for (t <- time; d <- date)
                roomMap((title.toLowerCase, d, t)) = hallName
            }
          }
        }
        val filmUrl   = Option(article.selectFirst("a[href*=/movies/]")).map(_.attr("href"))
        val synopsis  = Option(article.selectFirst("p.desc")).map(_.text().replaceAll("\\.{3,}$", "").trim).filter(_.nonEmpty)
        val posterUrl = Option(article.selectFirst("img[data-src]")).map(_.attr("data-src"))
        title -> ArticleDetails(filmUrl, synopsis, posterUrl)
      }
    }.toMap

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
          showtimes = sorted.map { event =>
            val dateKey = "%02d.%02d.%d".format(event.dateTime.getDayOfMonth, event.dateTime.getMonthValue, event.dateTime.getYear)
            val timeKey = "%02d:%02d".format(event.dateTime.getHour, event.dateTime.getMinute)
            val room    = roomMap.get((title.toLowerCase, dateKey, timeKey))
            Showtime(event.dateTime, event.bookingUrl, room)
          }
        )
      }
  }

  private case class ArticleDetails(filmUrl: Option[String], synopsis: Option[String], posterUrl: Option[String])

  private case class ScreeningEntry(
    movie:      Movie,
    dateTime:   LocalDateTime,
    posterUrl:  Option[String],
    bookingUrl: Option[String]
  )

  private def parseDuration(iso: String): Option[Int] =
    """PT(?:(\d+)H)?(?:(\d+)M)?""".r.findFirstMatchIn(iso).map { m =>
      Option(m.group(1)).map(_.toInt * 60).getOrElse(0) +
      Option(m.group(2)).map(_.toInt).getOrElse(0)
    }.filter(_ > 0)

  private def parseScreeningEvent(json: JsValue): Option[ScreeningEntry] =
    Try {
      val eventType = (json \ "@type").asOpt[String]
        .orElse((json \ "@type").asOpt[JsArray].flatMap(_.value.headOption.flatMap(_.asOpt[String])))

      eventType.filter(_ == "ScreeningEvent").flatMap { _ =>
        for {
          title     <- (json \ "workPresented" \ "name").asOpt[String]
          startDate <- (json \ "startDate").asOpt[String]
        } yield {
          val runtimeMinutes = (json \ "workPresented" \ "duration").asOpt[String].flatMap(parseDuration)
          val releaseYear    = (json \ "workPresented" \ "dateCreated").asOpt[Int]
                                 .orElse((json \ "workPresented" \ "copyrightYear").asOpt[Int])
          ScreeningEntry(
            movie      = Movie(title, runtimeMinutes, releaseYear),
            dateTime   = ZonedDateTime.parse(startDate).toLocalDateTime,
            posterUrl  = (json \ "workPresented" \ "image").asOpt[String].filter(_.nonEmpty),
            bookingUrl = (json \ "offers" \ "url").asOpt[String].filter(_.nonEmpty)
          )
        }
      }
    }.toOption.flatten
}
