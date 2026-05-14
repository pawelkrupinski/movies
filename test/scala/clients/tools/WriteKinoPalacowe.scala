package clients.tools

import services.cinemas.KinoPalacoweClient
import tools.RealHttpFetch

object WriteKinoPalacowe extends App {
  private val client = new KinoPalacoweClient(new RecordingHttpFetch("kino-palacowe", new RealHttpFetch()))
  client.fetch().foreach(println)
}
