package clients

import models.{CinemaMovie, Movie, Rialto, Showtime}
import org.jsoup.Jsoup

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.LocalDateTime
import scala.jdk.CollectionConverters._
import scala.util.Try

object RialtoClient {

  private val RepertoireUrl = "https://www.kinorialto.poznan.pl/repertuar/"
  private val BaseUrl       = "https://www.kinorialto.poznan.pl"

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

  // "DKF Absolwent: FILM TITLE" → "FILM TITLE"  (prefix identified by containing lowercase letters)
  private def stripCyclePrefix(title: String): String = {
    val colonIdx = title.indexOf(": ")
    if (colonIdx > 0 && colonIdx < 30) {
      val prefix = title.substring(0, colonIdx)
      if (prefix != prefix.toUpperCase) title.substring(colonIdx + 2) else title
    } else title
  }

  private val DateTimePat = """- (\d{4}-\d{2}-\d{2}) (\d{2}:\d{2}) -""".r

  private case class FilmEntry(
    title:     String,
    eventUrl:  String,
    posterUrl: Option[String],
    synopsis:  Option[String],
    director:  Option[String]
  )

  def fetch(): Seq[CinemaMovie] = {
    val response = httpClient.send(buildRequest(RepertoireUrl), HttpResponse.BodyHandlers.ofString())
    if (response.statusCode() != 200)
      throw new RuntimeException(s"kinorialto.poznan.pl returned ${response.statusCode()}")

    val filmEntries = parseRepertoire(response.body())

    val pendingPages = filmEntries.map { entry =>
      entry -> httpClient.sendAsync(buildRequest(entry.eventUrl), HttpResponse.BodyHandlers.ofString())
    }
    val showtimesByUrl: Map[String, Seq[Showtime]] = pendingPages.flatMap { case (entry, future) =>
      Try(parseEventPage(future.join().body())).toOption.map(entry.eventUrl -> _)
    }.toMap

    filmEntries
      .groupBy(_.title.toUpperCase)
      .values
      .flatMap { group =>
        val primary      = group.head
        val allShowtimes = group.flatMap(e => showtimesByUrl.getOrElse(e.eventUrl, Seq.empty))
                                .sortBy(_.dateTime)
        if (allShowtimes.isEmpty) None
        else Some(CinemaMovie(
          movie     = Movie(primary.title),
          cinema    = Rialto,
          posterUrl = primary.posterUrl,
          filmUrl   = Some(primary.eventUrl),
          synopsis  = primary.synopsis,
          cast      = None,
          director  = primary.director,
          showtimes = allShowtimes
        ))
      }
      .toSeq
  }

  private def parseRepertoire(html: String): Seq[FilmEntry] =
    // Split by the hr separator, then parse each block with jsoup
    html.split("""<hr class="table-seperator"/>""").toSeq.flatMap { blockHtml =>
      val block = Jsoup.parseBodyFragment(blockHtml)

      // Only film blocks have a link with title starting with "Film:"
      Option(block.selectFirst("""a[title^="Film:"]""")).map { link =>
        val rawEventUrl = link.attr("href")
        val eventUrl    = if (rawEventUrl.startsWith("http")) rawEventUrl else BaseUrl + rawEventUrl

        val rawTitle = Option(block.selectFirst("div.title")).map(_.text().trim).getOrElse("")
        val title    = stripCyclePrefix(rawTitle)

        val posterUrl = Option(block.selectFirst("img[src]")).map(_.attr("src"))

        val (synopsis, director) = Option(block.selectFirst("span.text")) match {
          case None => (None, None)
          case Some(span) =>
            val lines    = span.html().split("(?i)<br\\s*/?>").map(l => Jsoup.parseBodyFragment(l).body().text().trim)
            val dir      = lines.find(_.startsWith("Reż. ")).map(_.stripPrefix("Reż. ").trim)
            val emptyIdx = lines.indexWhere(_.isEmpty)
            val synLines = if (emptyIdx >= 0) lines.drop(emptyIdx + 1) else Array.empty[String]
            val synText  = synLines.filter(_.nonEmpty).mkString(" ").trim
            (Option(synText).filter(_.nonEmpty), dir)
        }

        FilmEntry(title, eventUrl, posterUrl, synopsis, director)
      }
    }

  private def parseEventPage(html: String): Seq[Showtime] = {
    val doc = Jsoup.parse(html)
    doc.select("a.b24-button.show[href][title]").asScala.flatMap { a =>
      val href  = a.attr("href")
      val title = a.attr("title")
      val url   = if (href.startsWith("http")) href else BaseUrl + href
      DateTimePat.findFirstMatchIn(title).flatMap { dt =>
        Try(LocalDateTime.parse(s"${dt.group(1)}T${dt.group(2)}")).toOption
          .map(dateTime => Showtime(dateTime, Some(url)))
      }
    }.toSeq.distinctBy(_.dateTime)
  }
}
