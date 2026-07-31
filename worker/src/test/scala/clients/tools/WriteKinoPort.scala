package clients.tools

import models.KinoPort
import tools.RealHttpFetch
import services.cinemas.pl.KinoPortClient

/** Record the live gcsw.pl WP-REST repertoire response as a fixture under
 *  test/resources/fixtures/kinoport/ for replay by KinoPortClientSpec. */
object WriteKinoPort {
  def main(args: Array[String]): Unit = {
    val client = new KinoPortClient(new RecordingHttpFetch("kinoport", new RealHttpFetch()), KinoPort)
    val movies = client.fetch()
    movies.foreach(println)
    println(s"\n=== ${movies.size} films, ${movies.flatMap(_.showtimes).size} showtimes recorded ===")
  }
}
