package clients.tools

import services.cinemas.CharlieMonroeClient
import tools.RealHttpFetch

object WriteCharlieMonroe extends App {
  private val client = new CharlieMonroeClient(new RecordingHttpFetch("charlie-monroe", new RealHttpFetch()))
  client.fetch().foreach(println)
}
