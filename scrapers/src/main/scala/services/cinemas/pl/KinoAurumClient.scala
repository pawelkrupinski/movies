package services.cinemas.pl

import models.*
import play.api.libs.json.*
import services.cinemas.common.ScrapeDates
import services.cinemas.common.ScraperParse.jsonText
import services.cinemas.common.{CinemaScraper, ScrapedSlot, SlotsToMovies}
import tools.{HttpFetch, HttpRead}


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
  // The Firestore collection path, without the API key riding in the query.
  override def sourceKey: Option[String] = Some(CinemaScraper.urlKey(KinoAurumClient.SeanseUrl.takeWhile(_ != '?')))

  def fetch(): Seq[CinemaMovie] =
    KinoAurumClient.parse(HttpRead.page(http, KinoAurumClient.SeanseUrl), cinema)
}

object KinoAurumClient {

  private val Project = "kinoaurum-ebbf2"
  private val ApiKey  = "AIzaSyBXRqgkwnSdDSm1SGaA0Rjo2GfevXgkl4E"
  val SeanseUrl =
    s"https://firestore.googleapis.com/v1/projects/$Project/databases/(default)/documents/seanse?pageSize=300&key=$ApiKey"


  def parse(json: String, cinema: Cinema): Seq[CinemaMovie] = {
    // A body that is not JSON throws rather than reading as a venue with no documents.
    val documents = (Json.parse(json) \ "documents")
      .asOpt[Seq[JsValue]].getOrElse(Seq.empty)

    val slots = documents.flatMap { document =>
      val fields = document \ "fields"
      for {
        title <- jsonText(fields \ "film_tytul" \ "stringValue")
        date  <- (fields \ "data" \ "stringValue").asOpt[String]
        time  <- (fields \ "godzina" \ "stringValue").asOpt[String]
        dt    <- ScrapeDates.parseDateTime(s"$date $time")
      } yield ScrapedSlot(title, dt)
    }

    SlotsToMovies.basic(slots, cinema, distinctBy = _.dateTime)
  }
}
