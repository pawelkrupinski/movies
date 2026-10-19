package services.cinemas.pl

import play.api.libs.json.{JsObject, Json}
import services.identity.ScreeningDays
import services.identity.agreement.Showing
import tools.{HttpFetch, HttpRead}

import java.time.LocalDate
import scala.util.Try

/**
 * A Polish venue's programme as Filmweb lists it — every film it screens over Filmweb's window, by Filmweb's film id,
 * with its days — what the agreement stage joins a venue's own listings to ([[services.identity.agreement.VenueListings]]).
 * One request a venue: `/api/v1/showtimes/cinema/<id>?date=<a day with no seances>` answers the whole window under
 * `filmDates` (asked for a day it screens, those films come back without their days). A venue Filmweb does not list,
 * or lists with an empty programme, is answered by its town's (`/showtimes/city/<id>`, one request a town: a programme
 * Filmweb has from the town's other cinemas, the venue's own often among them).
 *
 * `cinemaOf` is the venue's Filmweb cinema id ([[FilmwebCinemaIdResolver]], resolved at boot), `townsOf` the towns it
 * sits in (`City.townsOf`); Filmweb's town ids are read once, on the first venue a town answers.
 */
final class FilmwebProgrammes(http: HttpFetch, cinemaOf: String => Option[Int], townsOf: String => Seq[String], today: () => LocalDate) {
  import FilmwebProgrammes._

  private lazy val townIds: Map[String, Seq[Int]] =
    FilmwebCinemaIdResolver.parseTowns(HttpRead.page(http, FilmwebCinemaIdResolver.TownsUrl)).groupMap(_.name)(_.id)

  /** The venue's programme, else its towns'. A failed read throws: the question is asked again. */
  def of(venue: String): Seq[Showing] = {
    val yesterday = today().minusDays(1)
    val own = cinemaOf(venue).fold(Seq.empty[Showing])(id => parseProgramme(HttpRead.page(http, cinemaUrl(id, yesterday))))
    if (own.nonEmpty) own
    else merged(townsOf(venue).flatMap(town => townIds.getOrElse(town, Nil)).distinct
      .flatMap(id => parseProgramme(HttpRead.page(http, townUrl(id, yesterday)))))
  }
}

object FilmwebProgrammes {
  private val Showtimes = "https://www.filmweb.pl/api/v1/showtimes"

  /** The towns a venue sits in, by its display name (`City.townsOf`): none for a venue no city lists. */
  def townsOf(venue: String): Seq[String] =
    models.City.all.iterator.flatMap(city => city.cinemas.find(_.displayName == venue).map(city.townsOf)).nextOption().getOrElse(Nil)

  /** Every Polish venue's programme, each venue's Filmweb id resolved from Filmweb's town listings on the first question
   *  ([[FilmwebCinemaIdResolver]]) — for a harness with no worker's boot-time resolution to read; dated by `today`,
   *  the day the harness reads Filmweb's programmes on. */
  def resolving(http: HttpFetch, today: () => LocalDate): FilmwebProgrammes = {
    lazy val ids: Map[String, Int] = new FilmwebCinemaIdResolver(http).resolveAll().collect {
      case resolution if resolution.resolved => resolution.cinema.displayName -> resolution.filmwebId.get }.toMap
    new FilmwebProgrammes(http, venue => ids.get(venue), townsOf, today)
  }

  def cinemaUrl(cinemaId: Int, date: LocalDate): String = s"$Showtimes/cinema/$cinemaId?date=$date"
  def townUrl(townId: Int, date: LocalDate): String     = s"$Showtimes/city/$townId?date=$date"

  /** `{"filmDates":{"10085635":["2026-10-07"],…}, …}` as each film's days, sorted by film id. A body that is not JSON throws. */
  def parseProgramme(json: String): Seq[Showing] =
    (Json.parse(json) \ "filmDates").asOpt[JsObject].toSeq.flatMap(_.fields).flatMap { case (film, days) =>
      days.asOpt[Seq[String]].map(ds => Showing(film, ScreeningDays.of(ds.flatMap(d => Try(LocalDate.parse(d)).toOption))))
    }.filterNot(_.days.isEmpty).sortBy(_.film)

  /** Several programmes as one: each film once, with every day any lists it on. */
  def merged(showings: Seq[Showing]): Seq[Showing] =
    showings.groupMapReduce(_.film)(_.days)(_ ++ _).toSeq.map { case (film, days) => Showing(film, days) }.sortBy(_.film)
}
