package clients.tools

import services.cinemas.RialtoClient
import tools.RealHttpFetch

object WriteRialto {
  def main(args: Array[String]): Unit = {
    val client = new RialtoClient(new RecordingHttpFetch("rialto", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
