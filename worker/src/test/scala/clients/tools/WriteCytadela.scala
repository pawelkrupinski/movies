package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.CytadelaClient

object WriteCytadela {
  def main(args: Array[String]): Unit = {
    val client = new CytadelaClient(new RecordingHttpFetch("kino-cytadela", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
