package clients.tools

import clients.HeliosClient
import tools.HeliosFetch

object WriteHelios extends App {
  private val client = new HeliosClient(new RecordingHttpFetch("helios/bla", HeliosFetch))
  client.fetch().foreach(println)
}
