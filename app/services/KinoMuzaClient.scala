package clients

import models.{CinemaMovie, KinoMuza, Movie, Showtime}
import org.jsoup.Jsoup

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

object KinoMuzaClient {

  private val RepertoireUrl = "https://www.kinomuza.pl/repertuar/"

  private val httpClient = HttpClient.newBuilder()
    .version(HttpClient.Version.HTTP_1_1)
    .followRedirects(HttpClient.Redirect.NORMAL)
    .build()

  private def buildRequest(url: String): HttpRequest =
    HttpRequest.newBuilder()
      .uri(URI.create(url))
      .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
      .header("Accept", "text/html")
      .GET()
      .build()

  private def parseDate(ddMM: String): Option[LocalDate] =
    Try {
      val parts     = ddMM.trim.split("\\.")
      val today     = LocalDate.now()
      val candidate = LocalDate.of(today.getYear, parts(1).toInt, parts(0).toInt)
      if (candidate.isBefore(today.minusDays(1))) candidate.plusYears(1) else candidate
    }.toOption

  def fetch(): Seq[CinemaMovie] = {
    val response = httpClient.send(buildRequest(RepertoireUrl), HttpResponse.BodyHandlers.ofString())
    if (response.statusCode() != 200)
      throw new RuntimeException(s"kinomuza.pl returned ${response.statusCode()}")
    parseHtml(response.body())
  }

  private def parseHtml(html: String): Seq[CinemaMovie] = {
    val doc      = Jsoup.parse(html)
    val previews = doc.select("#movies .preview").asScala

    previews.flatMap { preview =>
      val title    = Option(preview.selectFirst(".preview-title")).map(_.text().trim).filter(_.nonEmpty)
      val filmUrl  = Option(preview.selectFirst("a[href*=/movie/]")).map(_.attr("href"))
      val posterUrl = Option(preview.selectFirst("img[data-src]")).map(_.attr("data-src"))
      val director = Option(preview.selectFirst(".f1-bold p")).map(_.text())
        .flatMap(t => t.split("reż\\.").lift(1))
        .map(_.split("[,\n]").head.trim)
        .filter(_.nonEmpty)

      title.map { t =>
        val showtimes = preview.select(".table-row").asScala.flatMap { row =>
          val dateOpt = Option(row.selectFirst(".day")).flatMap(el => parseDate(el.text()))
          val items   = row.select(".ticket-list-item").asScala

          dateOpt.toSeq.flatMap { date =>
            items.flatMap { item =>
              val timeText   = Option(item.selectFirst(".ticket-hour")).map(_.text().trim)
              val bookingUrl = Option(item.selectFirst("a.ticket-buy")).map(_.attr("href"))
              timeText.flatMap { t =>
                Try {
                  val parts = t.split(":")
                  Showtime(LocalDateTime.of(date, LocalTime.of(parts(0).toInt, parts(1).toInt)),
                           bookingUrl)
                }.toOption
              }
            }
          }
        }.toSeq.distinctBy(_.dateTime)

        CinemaMovie(
          movie     = Movie(t),
          cinema    = KinoMuza,
          posterUrl = posterUrl,
          filmUrl   = filmUrl,
          synopsis  = None,
          cast      = None,
          director  = director,
          showtimes = showtimes.sortBy(_.dateTime)
        )
      }
    }.toSeq.filter(_.showtimes.nonEmpty)
  }
}
