package clients.tools

import services.cinemas.MultikinoClient
import tools.RealHttpFetch

/** Refresh the Multikino fixture. `RecordingHttpFetch` writes every
 *  response body the client touches under `test/resources/fixtures/multikino/`,
 *  so simply running `MultikinoClient.fetch()` through it captures the API
 *  response (and the homepage, if the session warm-up fires) without any
 *  bespoke recording code here. */
object WriteMultikino {
  def main(args: Array[String]): Unit = {
    val fetch  = new RecordingHttpFetch("multikino", new RealHttpFetch())
    val client = new MultikinoClient(MultikinoClient.fetchFor(fetch))
    client.fetch().foreach(m => println(s"${m.movie.title} (${m.showtimes.size} showtimes)"))
  }
}
