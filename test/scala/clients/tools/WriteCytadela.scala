package clients.tools

import services.cinemas.CytadelaClient
import tools.RealHttpFetch

object WriteCytadela {
  def main(args: Array[String]): Unit = {
    val client = new CytadelaClient(new RecordingHttpFetch("kino-cytadela", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
