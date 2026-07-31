package clients.tools

import models.TheOldCourtWindsor
import tools.RealHttpFetch
import services.cinemas.uk.TheOldCourtClient

/** Record the live oldcourt.org.uk/events listing as a fixture under
 *  test/resources/fixtures/the-old-court/ for replay by TheOldCourtClientSpec. */
object WriteTheOldCourt {
  def main(args: Array[String]): Unit = {
    val client = new TheOldCourtClient(new RecordingHttpFetch("the-old-court", new RealHttpFetch()), TheOldCourtWindsor)
    val movies = client.fetch()
    movies.foreach(println)
    println(s"\n=== ${movies.size} films, ${movies.flatMap(_.showtimes).size} showtimes recorded ===")
  }
}
