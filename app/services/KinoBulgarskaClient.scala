package clients

import models.{CinemaMovie, KinoBulgarska, Movie, Showtime}
import org.jsoup.Jsoup
import org.jsoup.nodes.Element

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.{LocalDate, LocalDateTime, LocalTime}
import scala.jdk.CollectionConverters._
import scala.util.Try

object KinoBulgarskaClient {

  private val PageUrl = "http://kinobulgarska19.pl/repertuar"

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

  private val PolishMonths = Map(
    "stycznia" -> 1, "lutego" -> 2, "marca" -> 3, "kwietnia" -> 4,
    "maja" -> 5, "czerwca" -> 6, "lipca" -> 7, "sierpnia" -> 8,
    "września" -> 9, "października" -> 10, "listopada" -> 11, "grudnia" -> 12
  )

  private val PremiereSuffixPat = """(?i)\s+–\s+poznańska premiera$""".r
  private val DatePat           = """(\d+)\s+(\w+)""".r

  private def normalizeTitle(raw: String): String = {
    val stripped = PremiereSuffixPat.replaceFirstIn(raw, "")
    if (stripped.isEmpty) stripped else stripped.head.toUpper + stripped.tail.toLowerCase
  }

  private def parsePolishDate(text: String): Option[LocalDate] =
    DatePat.findFirstMatchIn(text).flatMap { m =>
      PolishMonths.get(m.group(2)).map { month =>
        val day       = m.group(1).toInt
        val today     = LocalDate.now()
        val candidate = LocalDate.of(today.getYear, month, day)
        if (candidate.isBefore(today.minusDays(1))) candidate.plusYears(1) else candidate
      }
    }

  private def extractDirector(section: Element): Option[String] =
    Option(section.selectFirst("p.movie-meta")).flatMap { meta =>
      val text = meta.text()
      if (!text.startsWith("reż. ")) None
      else {
        val afterRez = text.stripPrefix("reż. ")
        val parts    = afterRez.split(",\\s*")
        val yearIdx  = parts.indexWhere(_.matches("\\d{4} r\\."))
        if (yearIdx > 1) Some(parts.take(yearIdx - 1).mkString(", ")) else None
      }
    }

  def fetch(): Seq[CinemaMovie] = {
    val response = httpClient.send(buildRequest(PageUrl), HttpResponse.BodyHandlers.ofString())
    if (response.statusCode() != 200)
      throw new RuntimeException(s"kinobulgarska19.pl returned ${response.statusCode()}")
    parseHtml(response.body())
  }

  private def parseHtml(html: String): Seq[CinemaMovie] = {
    val doc            = Jsoup.parse(html)
    val filmByUrl      = collection.mutable.Map[String, (String, Option[String], Option[String], Option[String])]()
    val showtimesByUrl = collection.mutable.Map[String, collection.mutable.ListBuffer[Showtime]]()

    doc.select("article").asScala.foreach { article =>
      val dateOpt = Option(article.selectFirst("h3")).flatMap(h3 => parsePolishDate(h3.text()))

      article.select("section.clearfix").asScala.foreach { section =>
        for (link <- Option(section.selectFirst("h4 a[href]")); date <- dateOpt) {
          val filmUrl = link.attr("href")
          val title   = normalizeTitle(link.text())

          val timeOpt = Option(section.selectFirst(".start-info.clock")).flatMap { clockEl =>
            val text = clockEl.text().replaceAll("\\s+", "")
            Try {
              val parts = text.split(":")
              LocalDateTime.of(date, LocalTime.of(parts(0).toInt, parts(1).toInt))
            }.toOption
          }

          if (!filmByUrl.contains(filmUrl)) {
            val posterUrl = Option(section.selectFirst("img[src]")).map { img =>
              img.attr("src").replaceAll("-\\d+x\\d+(\\.jpg)", "$1")
            }
            val synopsis = Option(section.selectFirst("p:not(.movie-meta)")).map(_.text()).filter(_.nonEmpty)
            val director = extractDirector(section)
            filmByUrl(filmUrl) = (title, posterUrl, synopsis, director)
          }

          timeOpt.foreach { dateTime =>
            showtimesByUrl.getOrElseUpdate(filmUrl, collection.mutable.ListBuffer()) += Showtime(dateTime, None)
          }
        }
      }
    }

    filmByUrl.toSeq.flatMap { case (filmUrl, (title, posterUrl, synopsis, director)) =>
      showtimesByUrl.get(filmUrl).map { slots =>
        CinemaMovie(
          movie     = Movie(title),
          cinema    = KinoBulgarska,
          posterUrl = posterUrl,
          filmUrl   = Some(filmUrl),
          synopsis  = synopsis,
          cast      = None,
          director  = director,
          showtimes = slots.toSeq.sortBy(_.dateTime)
        )
      }
    }
  }
}
