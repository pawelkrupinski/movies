package clients.tools

import tools.HeliosFetch
import services.cinemas.pl.HeliosClient

object WriteHelios {
  def main(args: Array[String]): Unit = {
    val client = new HeliosClient(new RecordingHttpFetch("helios/missing-runtime", HeliosFetch))
    client.fetch().foreach(println)
  }
}
