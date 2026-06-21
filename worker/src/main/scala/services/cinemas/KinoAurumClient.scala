package services.cinemas

import models._
import play.api.libs.json._
import tools.HttpFetch

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.Try

/**
 * Kino Aurum (Złotoryja). The site kinoaurum.pl is a JS app backed by a public
 * Google Firestore database; the `seanse` collection's REST endpoint returns
 * every screening as a Firestore document. Each `seanse` document carries the film
 * title denormalised (`film_tytul`) plus the date (`data`, "YYYY-MM-DD") and
 * time (`godzina`, "HH:MM") as plain Warsaw-local strings — so one fetch yields
 * the whole programme with no join to the `filmy` collection.
 *
 * The API key is the site's own public web key (embedded in its frontend).
 *
 * Previously scraped from Filmweb, which had silently gone empty for the venue
 * (every poll returned `[]`) though the cinema is open and screening.
 */
class KinoAurumClient(http: HttpFetch, override val cinema: Cinema = KinoAurum)
    extends CinemaScraper {

  def scrapeHosts: Set[String] = CinemaScraper.hostsOf(KinoAurumClient.SeanseUrl)

  def fetch(): Seq[CinemaMovie] =
    KinoAurumClient.parse(http.get(KinoAurumClient.SeanseUrl), cinema)
}

object KinoAurumClient {

  private val Project = "kinoaurum-ebbf2"
  private val ApiKey  = "AIzaSyBXRqgkwnSdDSm1SGaA0Rjo2GfevXgkl4E"
  val SeanseUrl =
    s"https://firestore.googleapis.com/v1/projects/$Project/databases/(default)/documents/seanse?pageSize=300&key=$ApiKey"

  private val Fmt = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

  def parse(json: String, cinema: Cinema): Seq[CinemaMovie] = {
    val documents = (Try(Json.parse(json)).toOption.getOrElse(JsNull) \ "documents")
      .asOpt[Seq[JsValue]].getOrElse(Seq.empty)

    val slots = documents.flatMap { document =>
      val fields = document \ "fields"
      for {
        title <- (fields \ "film_tytul" \ "stringValue").asOpt[String].map(_.trim).filter(_.nonEmpty)
        date  <- (fields \ "data" \ "stringValue").asOpt[String]
        time  <- (fields \ "godzina" \ "stringValue").asOpt[String]
        dt    <- Try(LocalDateTime.parse(s"$date $time", Fmt)).toOption
      } yield (title, dt)
    }

    SlotsToMovies.fold(
      slots,
      titleOf    = _._1,
      showtimeOf = s => Showtime(s._2, None),
      distinctBy = _.dateTime
    ) { (title, _, showtimes) =>
      CinemaMovie(
        movie     = Movie(title),
        cinema    = cinema,
        posterUrl = None,
        filmUrl   = None,
        synopsis  = None,
        cast      = Seq.empty,
        director  = Seq.empty,
        showtimes = showtimes
      )
    }
  }
}
