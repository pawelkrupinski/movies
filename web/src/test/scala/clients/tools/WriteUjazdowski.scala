package clients.tools

import services.cinemas.UjazdowskiClient
import tools.RealHttpFetch

object WriteUjazdowski {
  def main(args: Array[String]): Unit = {
    val client = new UjazdowskiClient(new RecordingHttpFetch("ujazdowski", new RealHttpFetch()))
    client.fetch().foreach(println)
  }
}
