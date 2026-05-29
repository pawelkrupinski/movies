package clients.tools

import services.cinemas.CharlieMonroeClient
import tools.RealHttpFetch

object WriteCharlieMonroe {
  def main(args: Array[String]): Unit = {
    val client = new CharlieMonroeClient(new RecordingHttpFetch("charlie-monroe", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
