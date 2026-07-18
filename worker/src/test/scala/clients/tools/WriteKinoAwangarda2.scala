package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.KinoAwangarda2Client

object WriteKinoAwangarda2 {
  def main(args: Array[String]): Unit = {
    val client = new KinoAwangarda2Client(new RecordingHttpFetch("kino-awangarda2", new RealHttpFetch()))
    val movies = client.fetch()
    movies.foreach(println)
    println(s"--- ${movies.size} films, ${movies.map(_.showtimes.size).sum} showtimes ---")
  }
}
