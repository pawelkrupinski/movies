package clients.tools

import tools.RealHttpFetch
import services.cinemas.pl.AmondoClient

object WriteAmondo {
  def main(args: Array[String]): Unit = {
    val client = new AmondoClient(new RecordingHttpFetch("kino-amondo", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
