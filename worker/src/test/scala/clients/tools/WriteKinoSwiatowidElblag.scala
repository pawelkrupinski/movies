package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.KinoSwiatowidElblagClient

import java.time.LocalDate

object WriteKinoSwiatowidElblag {
  def main(args: Array[String]): Unit = {
    // Pin `today` to the capture date so the recorded `repertuar?dzien=…` URLs
    // (and thus the fixture filenames) match what the spec replays.
    val today  = args.headOption.map(LocalDate.parse).getOrElse(LocalDate.now())
    val client = new KinoSwiatowidElblagClient(new RecordingHttpFetch("kino-swiatowid-elblag", new RealHttpFetch()), today = today)
    val movies = client.fetch()
    movies.foreach(println)
    println(s"\n=== ${movies.size} films, ${movies.flatMap(_.showtimes).size} showtimes recorded from $today ===")
  }
}
