package clients.tools

import models.GermanRoster
import services.cinemas.common.KinoprogrammClient
import tools.RealHttpFetch

import java.time.LocalDate

/** Record two venues' kinoprogramm.com programme — every week the client walks — as
 *  fixtures under test/resources/fixtures/kinoprogramm/ for KinoprogrammClientSpec.
 *  A multiplex (CineStar Kulturbrauerei, many films and versions) and an arthouse
 *  (3001 Kino, OmU next to German). Pass the recording day, which the spec pins. */
object WriteKinoprogramm {
  val Venues: Seq[(String, String)] = Seq(
    "A0738" -> "/kino/berlin/cinestar-kino-in-der-kulturbrauerei-39607",
    "A0002" -> "/kino/hamburg/3001-kino-32436")

  def main(args: Array[String]): Unit = {
    val today = LocalDate.parse(args.headOption.getOrElse(sys.error("usage: WriteKinoprogramm <yyyy-MM-dd>")))
    val fetch = new RecordingHttpFetch("kinoprogramm", new RealHttpFetch())
    Venues.foreach { case (theaterId, path) =>
      val cinema = GermanRoster.theaterIdByCinema.collectFirst { case (c, id) if id == theaterId => c }.get
      val movies = new KinoprogrammClient(fetch, path, cinema, today = Some(today)).fetch()
      println(s"${cinema.displayName}: ${movies.size} films, ${movies.flatMap(_.showtimes).size} showtimes, " +
        s"${movies.flatMap(_.showtimes).map(_.dateTime.toLocalDate).maxOption.getOrElse("-")} last day")
    }
  }
}
