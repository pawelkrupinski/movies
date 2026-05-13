package clients

import models.{CinemaMovie, KinoPalacowe, Movie, Showtime}
import play.api.libs.json._
import tools.{HttpFetch, RealHttpFetch}

import java.time.LocalDateTime
import java.util.concurrent.Executors
import scala.util.Try

class KinoPalacoweClient(http: HttpFetch = new RealHttpFetch()) {

  private val BaseUrl = "https://kinopalacowe.pl"
  private val ApiBase = s"$BaseUrl/public/api/calendar/?widgetHash=widget_17943"

  private val RuntimePat = "(\\d+)'".r

  private def fetchFilmRuntime(filmUrl: String): Option[Int] =
    Try {
      RuntimePat.findAllMatchIn(http.get(filmUrl))
        .flatMap(m => Try(m.group(1).toInt).toOption)
        .filter(n => n >= 30 && n <= 300)
        .toSeq.headOption
    }.toOption.flatten

  def fetch(): Seq[CinemaMovie] = {
    val entries = fetchAllEntries()

    val pool = Executors.newFixedThreadPool(8)
    val runtimeByUrl: Map[String, Option[Int]] =
      try {
        entries.flatMap(_.filmUrl).distinct
          .map(url => url -> pool.submit[Option[Int]](() => fetchFilmRuntime(url)))
          .map { case (url, f) => url -> f.get() }
          .toMap
      } finally pool.shutdown()

    entries
      .groupBy(_.movieTitle)
      .toSeq
      .map { case (title, group) =>
        val sorted  = group.sortBy(_.dateTime)
        val first   = sorted.head
        val runtime = first.filmUrl.flatMap(runtimeByUrl.get).flatten
        CinemaMovie(
          movie     = Movie(title, runtime),
          cinema    = KinoPalacowe,
          posterUrl = first.posterUrl,
          filmUrl   = first.filmUrl,
          synopsis  = None,
          cast      = None,
          director  = None,
          showtimes = sorted.map(entry => Showtime(entry.dateTime, entry.bookingUrl, entry.room))
        )
      }
  }

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

  private def fetchPage(page: Int): (Seq[ScreeningEntry], Boolean) =
    parseJson(http.get(s"$ApiBase&page=$page"))

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

object KinoPalacoweClient {
  def fetch(): Seq[CinemaMovie] = new KinoPalacoweClient().fetch()
}
