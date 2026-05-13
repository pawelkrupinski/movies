package clients.tools

import clients.KinoPalacoweClient
import tools.RealHttpFetch

object WriteKinoPalacowe extends App {
  private val client = new KinoPalacoweClient(new RecordingHttpFetch("kino-palacowe", new RealHttpFetch()))
  client.fetch().foreach(println)
}
