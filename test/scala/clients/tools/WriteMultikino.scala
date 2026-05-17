package clients.tools

import services.cinemas.MultikinoClient
import tools.RealHttpFetch

/** Refresh the Multikino fixture. `RecordingHttpFetch` writes every
 *  response body the client touches under `test/resources/fixtures/multikino/`,
 *  so simply running `MultikinoClient.fetch()` through it captures the API
 *  response (and the homepage, if the session warm-up fires) without any
 *  bespoke recording code here. */
object WriteMultikino extends App {
  private val fetch  = new RecordingHttpFetch("multikino", new RealHttpFetch())
  private val client = new MultikinoClient(fetch, MultikinoClient.scrapingAntFromEnv)
  client.fetch().foreach(m => println(s"${m.movie.title} (${m.showtimes.size} showtimes)"))
}
