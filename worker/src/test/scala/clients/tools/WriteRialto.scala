package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.RialtoClient

object WriteRialto {
  def main(args: Array[String]): Unit = {
    val client = new RialtoClient(new RecordingHttpFetch("rialto", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
