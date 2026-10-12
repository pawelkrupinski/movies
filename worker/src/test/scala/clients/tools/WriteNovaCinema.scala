package clients.tools

import models._
import tools.RealHttpFetch
import services.cinemas.uk.NovaCinemaClient

/** Measure Nova Cinema Woking's home page live — film and showtime count and last date —
 *  and, with `--record`, re-record it under test/resources/fixtures/nova/ for
 *  NovaCinemaClientSpec. */
object WriteNovaCinema {
  def main(args: Array[String]): Unit = {
    val live = new RealHttpFetch()
    val http = if (args.contains("--record")) new RecordingHttpFetch("nova", live) else live
    val movies = new NovaCinemaClient(http, NovaCinemaWoking, VenueClock.system.today(TimeZones.UnitedKingdom)).fetch()
    val times  = movies.flatMap(_.showtimes)
    movies.foreach(m => println(s"FILM\t${m.movie.title}\t${m.showtimes.size}"))
    println(s"MEASURE\t${NovaCinemaWoking.displayName}\t${movies.size}\t${times.size}\t${times.map(_.dateTime.toLocalDate).maxOption.getOrElse("")}")
  }
}
