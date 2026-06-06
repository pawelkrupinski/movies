package clients.tools

import services.cinemas.NoweHoryzontyClient
import tools.RealHttpFetch

import java.time.LocalDate

object WriteNoweHoryzonty {
  def main(args: Array[String]): Unit = {
    // Pin `today` to the capture date so the recorded `rep.json?dzien=…` URLs
    // (and thus the fixture filenames) match what the spec replays.
    val today  = args.headOption.map(LocalDate.parse).getOrElse(LocalDate.now())
    val client = new NoweHoryzontyClient(new RecordingHttpFetch("nowe-horyzonty", new RealHttpFetch()), today)
    val movies = client.fetch()
    movies.foreach(println)
    println(s"\n=== ${movies.size} films, ${movies.flatMap(_.showtimes).size} showtimes recorded for week from $today ===")
  }
}
