package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.CharlieMonroeClient

object WriteCharlieMonroe {
  def main(args: Array[String]): Unit = {
    val client = new CharlieMonroeClient(new RecordingHttpFetch("charlie-monroe", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
