package clients.tools

import clients.CharlieMonroeClient
import tools.RealHttpFetch

object WriteCharlieMonroe extends App {
  private val client = new CharlieMonroeClient(new RecordingHttpFetch("charlie-monroe", new RealHttpFetch()))
  client.fetch().foreach(println)
}
