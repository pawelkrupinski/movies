package clients.tools

import models.KinoPatria
import tools.RealHttpFetch
import services.cinemas.pl.KinoPatriaClient

/** Record the live kinopatria.com/repertuar/ response as a fixture under
 *  test/resources/fixtures/kino-patria/ for replay by KinoPatriaClientSpec. */
object WriteKinoPatria {
  def main(args: Array[String]): Unit = {
    val client = new KinoPatriaClient(new RecordingHttpFetch("kino-patria", new RealHttpFetch()), KinoPatria)
    val movies = client.fetch()
    movies.foreach(println)
    println(s"\n=== ${movies.size} films, ${movies.flatMap(_.showtimes).size} showtimes recorded ===")
  }
}
