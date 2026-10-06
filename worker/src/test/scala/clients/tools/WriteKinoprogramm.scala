package clients.tools

import models.GermanRoster
import services.cinemas.common.KinoprogrammClient
import tools.{RateLimitedHttpFetch, RealHttpFetch}

import java.time.{Clock, LocalDate}
import scala.concurrent.duration._

/** Record two venues' kinoprogramm.com programme — every week the client walks, and
 *  every listed film's catalogue page — as fixtures under
 *  test/resources/fixtures/kinoprogramm/ for KinoprogrammClientSpec. A multiplex
 *  (CineStar Kulturbrauerei, many films and versions) and an arthouse (3001 Kino, OmU
 *  next to German). Pass the recording day, which the spec pins.
 *
 *  Replay-first: a page already on disk is served from it, so re-running for the
 *  pinned day only fills the pages a client change newly asks for (the catalogue
 *  pages were added 2026-10-06, after the weeks of 2026-09-26). Live requests keep
 *  kinoprogramm's one-a-second pace. */
object WriteKinoprogramm {
  val Venues: Seq[(String, String)] = Seq(
    "A0738" -> "/kino/berlin/cinestar-kino-in-der-kulturbrauerei-39607",
    "A0002" -> "/kino/hamburg/3001-kino-32436")

  def main(args: Array[String]): Unit = {
    val today = LocalDate.parse(args.headOption.getOrElse(sys.error("usage: WriteKinoprogramm <yyyy-MM-dd>")))
    val live  = new RateLimitedHttpFetch(new RealHttpFetch(), _ => Some(1.second), Clock.systemUTC())
    val fetch = new RecordMissingFetch("kinoprogramm", Set("kinoprogramm.com"), live)
    Venues.foreach { case (theaterId, path) =>
      val cinema = GermanRoster.theaterIdByCinema.collectFirst { case (c, id) if id == theaterId => c }.get
      val movies = new KinoprogrammClient(fetch, path, cinema, today = today, filmPages = fetch).fetch()
      println(s"${cinema.displayName}: ${movies.size} films, ${movies.flatMap(_.showtimes).size} showtimes, " +
        s"${movies.count(_.synopsis.isDefined)} with a synopsis, " +
        s"${movies.flatMap(_.showtimes).map(_.dateTime.toLocalDate).maxOption.getOrElse("-")} last day")
    }
  }
}
