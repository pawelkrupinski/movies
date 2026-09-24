package services.cinemas.roster

import models.GeoPoint
import play.api.libs.json.{JsValue, Json}
import services.cinemas.common.CinemaScraper
import services.cinemas.pl.{CinemaCityClient, MultikinoClient}

import java.time.LocalDate

/**
 * A chain's own list of its venues — the ONLINE roster audit's source for the
 * chain venues no [[VenueSourcePage]] covers. One request answers for every
 * venue of the chain, keyed by the id our scraper for that venue is wired with,
 * so the audit reads the list once and looks each of our venues up in it: a
 * venue whose id the chain no longer lists is gone, one whose listed town is
 * not the town we file it under is miswired.
 */
sealed trait ChainDirectory {
  def name: String

  /** The host the chain's own scrapers fetch from — how a scraper is told to
   *  be this chain's, before its [[CinemaScraper.chainVenueId]] is looked up. */
  def host: String

  /** The list, as of `today` where the endpoint wants a date. */
  def listUrl(today: LocalDate): String

  /** A page whose session cookies the list endpoint wants, fetched on the same
   *  client when a list call fails (see `tools.ChainListEgress`). */
  def warmUpUrl: Option[String] = None

  /** Venue id → where the chain says that venue is. */
  def parse(body: String): Map[String, PublishedVenue]
}

object ChainDirectory {

  val all: Seq[ChainDirectory] = Seq(Helios, CinemaCity, Multikino)

  def of(scraper: CinemaScraper): Option[(ChainDirectory, String)] =
    for {
      id        <- scraper.chainVenueId
      directory <- all.find(d => scraper.scrapeHosts.contains(d.host))
    } yield directory -> id

  private def text(js: JsValue, key: String): Option[String] =
    (js \ key).asOpt[String].map(_.trim).filter(_.nonEmpty)

  private def point(js: JsValue, lat: String, lon: String): Option[GeoPoint] =
    for { la <- (js \ lat).asOpt[Double]; lo <- (js \ lon).asOpt[Double] } yield GeoPoint(la, lo)

  /** `restapi.helios.pl/api/cinema`: `[{"id":"34b2…","city":"Legnica",
   *  "street":"ul. Najświętszej Marii Panny 9 59-220","latitude":…,…}]` — the
   *  id is the `sourceId` a [[services.cinemas.pl.HeliosCinema]] is wired with. */
  case object Helios extends ChainDirectory {
    val name = "Helios"
    // Helios appends the postcode to the street: "ul. Mostowa 5 43-300".
    private val TrailingPostcode = """\s*\d{2}-\d{3}$""".r

    private val ListUrl = "https://restapi.helios.pl/api/cinema"
    val host = CinemaScraper.hostsOf(ListUrl).head
    def listUrl(today: LocalDate): String = ListUrl
    def parse(body: String): Map[String, PublishedVenue] =
      Json.parse(body).as[Seq[JsValue]].flatMap { js =>
        for { id <- text(js, "id"); town <- text(js, "city") } yield
          id -> PublishedVenue(town, text(js, "street").map(TrailingPostcode.replaceFirstIn(_, "")).filter(_.nonEmpty),
                               point(js, "latitude", "longitude"))
      }.toMap
  }

  /** Cinema City's quickbook `cinemas/with-event/until/<date>`: `{"body":
   *  {"cinemas":[{"id":"1100","addressInfo":{"address1":"ul. Brzeska 27",
   *  "city":"Biała Podlaska",…},"latitude":…,…}]}}` — the id is the
   *  externalCode a `CinemaCityScraper` is wired with. */
  case object CinemaCity extends ChainDirectory {
    val name = "Cinema City"
    val host = CinemaScraper.hostsOf(CinemaCityClient.BaseApiUrl).head
    // A year out: a venue with nothing on sale that far ahead is not open.
    def listUrl(today: LocalDate): String = s"${CinemaCityClient.BaseApiUrl}/cinemas/with-event/until/${today.plusYears(1)}"
    def parse(body: String): Map[String, PublishedVenue] =
      (Json.parse(body) \ "body" \ "cinemas").as[Seq[JsValue]].flatMap { js =>
        val address = (js \ "addressInfo").toOption
        for { id <- text(js, "id"); town <- address.flatMap(text(_, "city")) } yield
          id -> PublishedVenue(town, address.flatMap(text(_, "address1")), point(js, "latitude", "longitude"))
      }.toMap
  }

  /** Multikino's `showings/cinemas`: `{"result":[{"alpha":"B","cinemas":
   *  [{"cinemaId":"0006","cinemaName":"Bydgoszcz",…}]}]}` — the id is the one a
   *  [[MultikinoClient]] is wired with. It publishes no street and no
   *  coordinates, and names a venue by its town with the mall after it
   *  ("Warszawa G City Reduta"), which [[TownName.same]] already reads as the
   *  town with a qualifier. Behind the same session-cookie wall as the
   *  programme API, so the home page goes first. */
  case object Multikino extends ChainDirectory {
    val name = "Multikino"
    val host = CinemaScraper.hostsOf(MultikinoClient.BaseUrl).head
    def listUrl(today: LocalDate): String = s"${MultikinoClient.BaseUrl}/api/microservice/showings/cinemas"
    override def warmUpUrl: Option[String] = Some(MultikinoClient.HomeUrl)
    def parse(body: String): Map[String, PublishedVenue] =
      (Json.parse(body) \ "result").as[Seq[JsValue]]
        .flatMap(group => (group \ "cinemas").asOpt[Seq[JsValue]].getOrElse(Nil))
        .flatMap(js => for { id <- text(js, "cinemaId"); town <- text(js, "cinemaName") } yield id -> PublishedVenue(town, None))
        .toMap
  }
}
