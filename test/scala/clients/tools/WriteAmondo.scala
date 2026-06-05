package clients.tools

import services.cinemas.AmondoClient
import tools.RealHttpFetch

object WriteAmondo {
  def main(args: Array[String]): Unit = {
    val client = new AmondoClient(new RecordingHttpFetch("kino-amondo", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
