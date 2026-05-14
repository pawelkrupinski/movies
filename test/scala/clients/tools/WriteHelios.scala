package clients.tools

import services.cinemas.HeliosClient
import tools.HeliosFetch

object WriteHelios extends App {
  private val client = new HeliosClient(new RecordingHttpFetch("helios/missing-runtime", HeliosFetch))
  client.fetch().foreach(println)
}
