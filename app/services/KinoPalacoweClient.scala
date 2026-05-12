package clients

import models.{CinemaMovie, KinoPalacowe, Movie, Showtime}
import play.api.libs.json._

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.time.LocalDateTime

object KinoPalacoweClient {

  private val BaseUrl  = "https://kinopalacowe.pl"
  private val ApiBase  = s"$BaseUrl/public/api/calendar/?widgetHash=widget_17943"

  private val httpClient = HttpClient.newBuilder()
    .version(HttpClient.Version.HTTP_1_1)
    .followRedirects(HttpClient.Redirect.NORMAL)
    .build()

  def fetch(): Seq[CinemaMovie] =
    fetchAllEntries()
      .groupBy(_.movieTitle)
      .toSeq
      .map { case (title, entries) =>
        val sorted = entries.sortBy(_.dateTime)
        val first  = sorted.head
        CinemaMovie(
          movie     = Movie(title, first.runtimeMinutes),
          cinema    = KinoPalacowe,
          posterUrl = first.posterUrl,
          filmUrl   = first.filmUrl,
          synopsis  = None,
          cast      = None,
          director  = None,
          showtimes = sorted.map(entry => Showtime(entry.dateTime, entry.bookingUrl, entry.room))
        )
      }

  // ── Pagination ─────────────────────────────────────────────────────────────

  private def fetchAllEntries(): Seq[ScreeningEntry] = {
    var entries = Seq.empty[ScreeningEntry]
    var page    = 1
    var hasNext = true
    while (hasNext) {
      val (pageEntries, next) = fetchPage(page)
      entries = entries ++ pageEntries
      hasNext = next
      page   += 1
    }
    entries
  }

  private def fetchPage(page: Int): (Seq[ScreeningEntry], Boolean) = {
    val request = HttpRequest.newBuilder()
      .uri(URI.create(s"$ApiBase&page=$page"))
      .header("User-Agent", "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36")
      .header("Accept", "application/json, text/plain, */*")
      .header("Accept-Language", "pl-PL,pl;q=0.9,en-US;q=0.8,en;q=0.7")
      .header("Referer", s"$BaseUrl/podstrony/371-repertuar/")
      .GET()
      .build()

    val response = httpClient.send(request, HttpResponse.BodyHandlers.ofString())
    if (response.statusCode() != 200)
      throw new RuntimeException(s"kinopalacowe.pl returned ${response.statusCode()}")

    parseJson(response.body())
  }

  // ── JSON parsing ───────────────────────────────────────────────────────────

  private case class ScreeningEntry(
    movieTitle:     String,
    dateTime:       LocalDateTime,
    posterUrl:      Option[String],
    filmUrl:        Option[String],
    bookingUrl:     Option[String],
    room:           Option[String] = None,
    runtimeMinutes: Option[Int]    = None
  )

  private def parseJson(json: String): (Seq[ScreeningEntry], Boolean) = {
    val root    = Json.parse(json)
    val hasNext = (root \ "next").asOpt[Boolean].getOrElse(false)
    val days    = (root \ "results").asOpt[JsArray].map(_.value).getOrElse(Seq.empty)

    val entries = days.flatMap { day =>
      (day \ "subsections").asOpt[JsArray].map(_.value).getOrElse(Seq.empty).flatMap { sub =>
        (sub \ "entries").asOpt[JsArray].map(_.value).getOrElse(Seq.empty).flatMap(parseEntry)
      }
    }

    (entries.toSeq, hasNext)
  }

  private def parseEntry(entry: JsValue): Option[ScreeningEntry] = {
    val filmUrl    = (entry \ "url").asOpt[String].filter(_.contains("/filmy/"))
    val ticketType = (entry \ "ticket_type").asOpt[Int].getOrElse(0)

    if (filmUrl.isEmpty || ticketType != 2) None
    else {
      val rawTitle  = (entry \ "title").asOpt[String].getOrElse("")
      val title     = rawTitle.split(" \\| ").head.trim
        .stripPrefix("Poranek dla dzieci: ")
        .stripPrefix("DKF Zamek: ")
      val startDate = (entry \ "start_date").asOpt[String]
      val startTime = (entry \ "start_time").asOpt[String]

      for {
        dateStr <- startDate
        timeStr <- startTime
        if title.nonEmpty
      } yield {
        val dateTime  = LocalDateTime.parse(s"${dateStr}T${timeStr}")
        val photoPath = (entry \ "photo" \ "sizes" \ "lg").asOpt[String]
                          .filter(_.nonEmpty)
                          .orElse((entry \ "photo" \ "image").asOpt[String].filter(_.nonEmpty))
        val room = (entry \ "category").asOpt[String].filter(_.nonEmpty)
        val runtime = (entry \ "duration").asOpt[Int]
                        .orElse((entry \ "runtime").asOpt[Int])
                        .orElse((entry \ "length").asOpt[Int])
        ScreeningEntry(
          movieTitle     = title,
          dateTime       = dateTime,
          posterUrl      = photoPath.map(path => if (path.startsWith("http")) path else s"$BaseUrl$path"),
          filmUrl        = filmUrl,
          bookingUrl     = (entry \ "ticket_url").asOpt[String].filter(_.nonEmpty),
          room           = room,
          runtimeMinutes = runtime
        )
      }
    }
  }
}
