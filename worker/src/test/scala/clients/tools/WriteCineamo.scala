package clients.tools

import models.GermanCinema
import services.cinemas.common.CineamoClient
import tools.{RateLimitedHttpFetch, RealHttpFetch}

import java.time.{Clock, LocalDate}
import scala.concurrent.duration._

/** Record venues' Cineamo programmes — every `/showings` page the client reads — under
 *  test/resources/fixtures/cineamo/ for CineamoClientSpec:
 *
 *   - Babylon (Mitte), Berlin: Filmstarts lists 23 of its shows in a fortnight, Cineamo 52
 *     — the venue the per-venue source choice exists for; twelve pages;
 *   - Cineplex Cineworld Dettelbach: a multiplex fifteen pages deep, 3D and D-BOX;
 *   - Astor Film Lounge HafenCity, Hamburg: bills a Dolby Atmos screening as "ATMOS: <film>".
 *
 *  Replay-first, like [[WriteKinoheld]]. Pass the recording day, which the spec pins. */
object WriteCineamo {
  val Venues: Seq[(String, CineamoClient.CinemaId)] = Seq(
    "Babylon (Mitte)"               -> CineamoClient.CinemaId("997"),
    "Cineplex Cineworld Dettelbach" -> CineamoClient.CinemaId("1"),
    "Astor Film Lounge HafenCity"   -> CineamoClient.CinemaId("1243"))

  def main(args: Array[String]): Unit = {
    val today = LocalDate.parse(args.headOption.getOrElse(sys.error("usage: WriteCineamo <yyyy-MM-dd>")))
    val live  = new RateLimitedHttpFetch(new RealHttpFetch(), _ => Some(300.millis), Clock.systemUTC())
    val fetch = new RecordMissingFetch("cineamo", Set("cineamo.com"), live)
    Venues.foreach { case (name, id) =>
      val movies = new CineamoClient(fetch, id, new GermanCinema(name, name), today = today).fetch()
      println(s"$name: ${movies.size} films, ${movies.flatMap(_.showtimes).size} showtimes, " +
        s"${movies.flatMap(_.showtimes).map(_.dateTime.toLocalDate).maxOption.getOrElse("-")} last day")
    }
  }
}
